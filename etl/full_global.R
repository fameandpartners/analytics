library(readr)
library(RPostgreSQL)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(shiny)
library(DT)
library(httr)

# ---- FUNCTIONS ----
year_month <- function(date_value){
    paste(year(date_value),
          formatC(month(date_value), width = 2, flag = "0"),
          sep = "-")
}

dollar <- function(number){
    paste0("$", round(number, digits = 2) %>%
               format(nsmall = 2, big.mark = ","))
}

percent <- function(number){
    paste0(format(100 * round(number, 2), big.mark = ","), "%")
}

short_dollar <- function(number){
    ifelse(number < 1000, paste0("$", round(number)),
           ifelse(number < 1000000, paste0("$", round(number / 1000, 1), "K"),
                  ifelse(number < 1000000000, paste0("$", round(number / 1000000, 1), "M"),
                         paste0("$", round(number / 1000000000, 1), "B"))))
}

short_number <- function(number){
    ifelse(number < 1000, number,
           ifelse(number < 1000000, paste0(round(number / 1000, 1), "K"),
                  ifelse(number < 1000000000, paste0((round(number / 1000000, 1)), "M"),
                         paste0(round(number / 1000000000, 1), "B"))))
}

h3c <- function(x) h3(x, align = "center")

query_aud_to_usd <- function(){
    resp <- GET("https://api.fixer.io/latest?base=AUD")
    if(resp$status_code == 200){
        return(content(resp)$rates$USD)
    } else { return(0.75) }
}

dress_image_url <- function(asset_id, attachment_file_name){
    paste0("https://d1msb7dh8kb0o9.cloudfront.net/spree/products/",
           asset_id,
           "/original/",
           attachment_file_name)
}

sql_convert_to_LA_time <- function(utc_time){
    paste0("(", utc_time, " at time zone 'UTC') at time zone 'America/Los_Angeles'")
}

# ---- DATA ----

# query conversion rates
aud_to_usd <- 0.77  # query_aud_to_usd()

# read collections data
collections <- read_csv("~/code/analytics/ecommerce-performance/static-data/collections_2.csv",
                        col_types = "iccccc") %>%
    transmute(product_id = `Product ID`,
              collection_na = Collection)

all_touches <- read_csv("~/code/analytics/ecommerce-performance/static-data/all_touches.csv",
                        col_types = cols(
                            .default = col_character(),
                            order_id = col_integer(),
                            user_id = col_integer(),
                            touch_time = col_datetime(format = ""),
                            ordered_at = col_datetime(format = ""),
                            added_to_cart_at = col_datetime(format = ""),
                            total = col_double(),
                            revenue_usd = col_double(),
                            step = readr::col_factor(levels = c("Cart","Checkout","Purchase", ordered = TRUE)),
                            cohort = readr::col_factor(levels = c("Prom", "Bridal", "Contemporary", "Not Assigned", ordered = TRUE))
                        )) %>%
    rename(sales_usd = revenue_usd)

# factory manufacturing cost data
# still missing some costs
factory_costs <- read_csv("~/code/analytics/ecommerce-performance/static-data/eCommerce Factory Cost.csv",
                          col_types = cols(
                              StyleNumber = col_character(),
                              `Unit Price` = col_double())) %>%
    transmute(style_number = toupper(StyleNumber), manufacturing_cost = `Unit Price`)

shipping_costs <- read_csv("~/code/analytics/ecommerce-performance/static-data/avg_shipping_costs.csv",
                           col_types = cols(
                               YearNum = col_integer(),
                               MonthNum = col_integer(),
                               USD = col_double())) %>%
    rename(ship_year = YearNum, ship_month = MonthNum, avg_order_shipping_cost = USD) %>%
    mutate(ship_year_month = year_month(as.Date(paste(ship_year, ship_month, 1, sep = "-"))))

# set db connection
source("~/code/analytics/ecommerce-performance/fp_init.R")

# ---- SALES ----
ordered_units <- tbl(fp_con, sql(paste(
    "SELECT",
    "o.id order_id,",
    "o.number order_number,",
    "o.state order_state,",
    "o.payment_state,",
    sql_convert_to_LA_time("o.completed_at"), "completed_timestamp,",
    "o.total,",
    "o.item_total,",
    "o.adjustment_total o_adjustments,",
    "o.email,",
    "o.user_id,",
    "o.user_first_name || ' ' || o.user_last_name customer_name,",
    "o.currency,",
    "o.ship_address_id,",
    "li.id line_item_id,",
    "li.quantity,",
    "li.price,",
    "v.product_id,",
    "g.size g_size",
    "FROM spree_orders o",
    "INNER JOIN spree_line_items li",
    "ON li.order_id = o.id",
    "LEFT JOIN spree_variants v",
    "ON li.variant_id = v.id",
    "LEFT JOIN (",
    "SELECT DISTINCT order_id",
    "FROM spree_payments",
    "WHERE state = 'completed'",
    ") pay ON pay.order_id = o.id",
    "LEFT JOIN global_skus g",
    "ON g.sku = v.sku",
    "WHERE completed_at IS NOT NULL",
    "AND completed_at >= '2015-01-01'",
    "AND total > 0"))) %>%
    collect() %>%
    group_by(order_id) %>%
    mutate(gross_extra_attributed = (item_total - sum(price)) / n(),
           net_extra_attributed = (total - sum(price)) / n(),
           conversion_rate = ifelse(currency == "AUD", aud_to_usd, 1),
           sales_usd = (price + net_extra_attributed) * conversion_rate,
           gross_revenue_usd = (price + gross_extra_attributed) * conversion_rate,
           adjustments_total_percentage = o_adjustments / item_total,
           adjustments_usd = gross_revenue_usd * adjustments_total_percentage,
           order_date = as.Date(completed_timestamp)) %>%
    ungroup()

# ---- CUSTOMIZATIONS ----
customizations <- tbl(fp_con, sql(paste(
    "SELECT",
    "lip.line_item_id,",
    "MAX(CASE WHEN lip.customization_value_ids SIMILAR TO '%([1-9])%'",
    "THEN 1 ELSE 0 END) customized,",
    "STRING_AGG(DISTINCT lip.size, ', ') lip_size,",
    "INITCAP(STRING_AGG(DISTINCT lip.color, ', ')) color,",
    "INITCAP(STRING_AGG(DISTINCT lip.height, ', ')) lip_height",
    "FROM line_item_personalizations lip",
    "WHERE lip.line_item_id IN (",
    paste(ordered_units$line_item_id, collapse = ","), ")",
    "GROUP BY line_item_id"))) %>%
    collect()

# ---- PRODUCTS ----
products <- tbl(fp_con, sql(paste(
    "SELECT",
    "p.id product_id,",
    "g.style style_number,",
    "INITCAP(p.name) style_name,",
    "p.hidden,",
    "p.deleted_at,",
    "p.available_on",
    "FROM spree_products p",
    "LEFT JOIN (",
    "SELECT product_id, STRING_AGG(DISTINCT UPPER(style_number), ',') style",
    "FROM global_skus",
    "GROUP BY product_id",
    ") g ON g.product_id = p.id",
    "WHERE p.id IN (", 
    paste(unique(ordered_units$product_id), collapse = ","), ")"))) %>%
    collect()

# ---- ADDRESSES ----
addresses <- tbl(fp_con, sql(paste(
    "SELECT",
    "sa.id ship_address_id,",
    "INITCAP(sa.city) ship_city,",
    "INITCAP(ss.name) ship_state,",
    "INITCAP(sc.name) ship_country",
    "FROM spree_addresses sa",
    "INNER JOIN spree_states ss",
    "ON ss.id = sa.state_id",
    "INNER JOIN spree_countries sc",
    "ON sc.id = sa.country_id",
    "WHERE sa.id IN (",
    (ordered_units %>%
         filter(!is.na(ship_address_id)))$ship_address_id %>%
        unique() %>%
        paste(collapse = ","), 
    ")"))) %>%
    collect()

# ---- SHIPMENTS ----
shipment_data <- tbl(fp_con, sql(paste(
    "SELECT s.order_id, liu.line_item_id ship_line_item_id, s.shipped_at",
    "FROM spree_shipments s",
    "LEFT JOIN (",
    "SELECT DISTINCT line_item_id, shipment_id",
    "FROM line_item_updates",
    "WHERE match_errors not like '%:%'",
    "AND shipment_errors not similar to '%([a-zA-Z])%'",
    "AND line_item_id is not null",
    "AND shipment_id is not null",
    ") liu ON liu.line_item_id = s.id",
    "WHERE s.shipped_at IS NOT NULL"))) %>%
    collect()
o_shipments <- shipment_data %>%
    select(order_id, shipped_at)%>%
    group_by(order_id) %>%
    summarise(li_ship_date = max(shipped_at) %>% as.Date)
li_shipments <- shipment_data %>%
    filter(!is.na(ship_line_item_id)) %>%
    rename(line_item_id = ship_line_item_id) %>%
    select(line_item_id, shipped_at) %>%
    group_by(line_item_id) %>%
    summarise(o_ship_date = max(shipped_at) %>% as.Date)

# ---- RETURNS ----
returns <- tbl(fp_con, "item_returns") %>%
    select(requested_at, refunded_at, line_item_id, refund_amount, 
           reason_category, reason_sub_category, acceptance_status) %>%
    filter(line_item_id %in% ordered_units$line_item_id) %>%
    collect()

# ---- PAYMENTS ----
payments <- tbl(fp_con, sql(paste(
    "SELECT order_id, amount p_amount",
    "FROM spree_payments",
    "WHERE state = 'completed'",
    "AND created_at >= '2015-12-01'"))) %>%
    collect() %>%
    group_by(order_id) %>%
    summarise(order_payments = n(),
              total_payment_amount = sum(p_amount))

# ---- ADJUSTMENTS ----
adjustments <- tbl(fp_con, sql(paste(
    "SELECT adjustable_id order_id, originator_type, SUM(amount) adjustments",
    "FROM spree_adjustments",
    "WHERE amount != 0 AND eligible",
    "GROUP BY order_id, originator_type"))) %>%
    collect() %>%
    left_join(data_frame(originator_type = c("Spree::TaxRate","Spree::ShippingMethod","Spree::PromotionAction",NA), 
                         adjustment_type = c("o_taxes","o_shipping","o_promotions","o_other_adjustments")), 
              by = "originator_type") %>%
    select(-originator_type) %>%
    spread(adjustment_type, adjustments, fill = 0)

# ---- PRODUCT TAXONS ----
product_taxons <- tbl(fp_con, sql(paste(
    "SELECT pt.product_id, t.name taxon_name",
    "FROM spree_products_taxons pt",
    "JOIN spree_taxons t on t.id = pt.taxon_id",
    "WHERE pt.product_id IN (",
    ordered_units$product_id %>%
        unique() %>%
        paste(collapse = ","),
    ")"))) %>%
    collect()

# ---- DRESS IMAGES ----
dress_images <- tbl(fp_con, sql(paste(
    "SELECT p.id product_id, a.id asset_id, a.attachment_file_name, ",
    "a.attachment_width, a.attachment_height",
    "FROM spree_assets a",
    "INNER JOIN product_color_values pcv ",
    "ON a.viewable_id = pcv.id",
    "INNER JOIN spree_products p",
    "ON p.id = pcv.product_id",
    "WHERE a.attachment_width < 2000",
    "AND a.viewable_type = 'ProductColorValue'",
    "AND a.attachment_updated_at >= '2015-01-01'",
    "AND p.id IN (", paste(ordered_units$product_id %>% 
                               unique(), collapse = ","), ")"))) %>%
    collect() %>%
    filter(!duplicated(product_id)) %>%
    mutate(
        dress_image_url = paste0(
            'https://d1msb7dh8kb0o9.cloudfront.net/spree/products/',
            asset_id,
            '/original/',
            attachment_file_name
        ),
        dress_image_tag = paste0(
            '<img src="',
            dress_image_url,
            '" height="100" width="',
            100*(attachment_width/attachment_height),
            '"></img>'
        )
    )

# ---- MERGE + TRANSFORM QUERIES INTO MASTER SALES DF ----
products_sold <- ordered_units %>%
    left_join(customizations, by = "line_item_id") %>%
    left_join(products, by = "product_id") %>%
    left_join(addresses, by = "ship_address_id") %>%
    left_join(o_shipments, by = "order_id") %>%
    left_join(li_shipments, by = "line_item_id") %>%
    left_join(returns, by = "line_item_id") %>%
    left_join(payments, by = "order_id") %>%
    left_join(adjustments, by = "order_id") %>%
    left_join(product_taxons %>%
                  filter(taxon_name %>% tolower() %>% str_detect("mini|knee|petti|midi|ankle|maxi|long")
                         & !str_detect(taxon_name, " ")) %>%
                  group_by(product_id) %>%
                  summarise(length = paste0(taxon_name[1] %>% str_trim() %>% substr(1, 1) %>% toupper(), 
                                            taxon_name[1] %>% str_trim() %>% substr(2, 10))),
              by = "product_id") %>%
    group_by(order_id) %>%
    mutate(payments = coalesce(order_payments / n(), 0),
           item_total_usd = item_total * conversion_rate,
           promotions_usd = gross_revenue_usd * (o_promotions / item_total),
           shipping_usd = gross_revenue_usd * (o_shipping / item_total),
           taxes_usd = gross_revenue_usd * (o_taxes / item_total),
           other_adjustments_usd = gross_revenue_usd * (o_other_adjustments / item_total)) %>%
    ungroup() %>%
    mutate(ship_date = coalesce(li_ship_date, o_ship_date),
           refund_amount_usd = (refund_amount / 100) * ifelse(currency == "AUD", aud_to_usd, 1),
           price_usd = price * ifelse(currency == "AUD", aud_to_usd, 1),
           height = paste0(substr(lip_height, 1, 1) %>% toupper(), substr(lip_height, 2, 250)),
           size = coalesce(g_size, lip_size),
           return_order_id = ifelse(!is.na(acceptance_status), order_id, NA),
           product_live = ifelse(!hidden & (is.na(deleted_at) | deleted_at > today()) & (available_on <= today()),
                                 "Yes", "No"),
           is_shipped = !is.na(ship_date),
           return_requested = !is.na(return_order_id),
           item_returned = !is.na(refund_amount),
           order_status = ifelse(order_state == "canceled", "Canceled",
                                 ifelse(item_returned, "Returned",
                                        ifelse(return_requested, "Refund Requested",
                                               ifelse(is_shipped, "Shipped","Paid")))),
           return_reason = ifelse(tolower(reason_category) %in% c('n/a','na','not specified','not stated','not satisfied')
                                  | is.na(reason_category),
                                  "No Reason", 
                                  paste0(toupper(substr(str_trim(reason_category), 1, 1)),
                                         substr(str_trim(reason_category), 2, 250))),
           estimated_ship_date = ifelse(is.na(ship_date), order_date + 10, ship_date) %>% as.Date(origin = "1970-01-01"),
           ship_year_month = year_month(estimated_ship_date),
           order_year_month = year_month(order_date),
           payment_processing_cost = sales_usd * 0.029 * payments,
           physically_customized = ifelse(is.na(customized), 0, customized)) %>%
    left_join(collections %>%
                  group_by(product_id) %>%
                  summarise(collection_na = paste(unique(collection_na), collapse = ", ")),
              by = "product_id") %>%
    mutate(collection = ifelse(is.na(collection_na), "2014-2015 -  Old", collection_na)) %>%
    select(-collection_na) %>%
    separate(size, c("us_size_str","au_size_str"), sep = "/", remove = FALSE) %>%
    mutate(us_size = as.integer(str_replace_all(us_size_str, "US", ""))) %>%
    left_join(factory_costs %>%
                  group_by(style_number) %>%
                  summarise(manufacturing_cost = mean(manufacturing_cost)),
              by = "style_number") %>%
    # Filter out Lacey's PR Order discounted improperly
    filter(order_id != 26075489) %>%
    left_join(shipping_costs, by = c("ship_year_month")) %>%
    group_by(order_id) %>%
    mutate(li_shipping_cost = coalesce(avg_order_shipping_cost / n(),
                                       median(shipping_costs$avg_order_shipping_cost) / n()),
           units_in_order = n()) %>%
    ungroup() %>%
    left_join(dress_images, by = "product_id")

products_sold$order_status <- factor(
    products_sold$order_status,
    levels = c("Paid","Shipped","Refund Requested","Returned","Canceled")
)

products_sold$size <- factor(
    products_sold$size,
    levels = paste0("US", seq(0,22,2), "/AU", seq(4,26,2))
)

products_sold$height <- factor(
    products_sold$height,
    levels = c(c("Petite", "Standard", "Tall"))
)