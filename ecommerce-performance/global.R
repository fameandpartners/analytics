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
    ifelse(number < 1000, round(number) %>% as.integer(),
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

# query conversion rates
aud_to_usd <- 0.74  # query_aud_to_usd()

# ---- COLLECTIONS ----
collections <- read_csv("static-data/collections_2.csv",
                        col_types = "iccccc") %>%
    transmute(product_id = `Product ID`,
              collection_na = Collection)

# ---- MANUFACTURING COSTS ----
factory_costs <- read_csv("static-data/factory_costs.csv",
                          col_types = cols(
                              StyleNumber = col_character(),
                              `Unit Price` = col_double())) %>%
    group_by(style_number = toupper(StyleNumber)) %>%
    summarise(manufacturing_cost = min(`Unit Price`))

# ---- SHIPPING COSTS ----
shipping_costs <- read_csv("static-data/avg_shipping_costs.csv",
                           col_types = cols(
                               YearNum = col_integer(),
                               MonthNum = col_integer(),
                               USD = col_double())) %>%
    rename(ship_year = YearNum, ship_month = MonthNum, avg_order_shipping_cost = USD) %>%
    mutate(ship_year_month = year_month(as.Date(paste(ship_year, ship_month, 1, sep = "-"))))

# ---- TOUCH POINTS ----
all_touches <- read_csv("static-data/all_touches.csv",
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

# ---- CUSTOMER ACQUISITIONS ----
customer_aquisitions <- read_csv("static-data/customer_aquisitions.csv",
                                 col_types = cols(
                                     email = col_character(),
                                     date = col_date(format = "")))

# ---- CONNECT TO REPLICA ----
# set db connection
source("fp_init.R")

# ---- FB Ad Images ----
fb_images <- tbl(fp_con, sql(paste(
    "SELECT DISTINCT ads.name ad_name, copy.image_url ad_image",
    "FROM facebook_ads ads",
    "INNER JOIN facebook_ad_creatives copy",
        "ON copy.facebook_ad_id = ads.id"))) %>%
    collect() %>%
    group_by(ad_name) %>%
    filter(!duplicated(str_replace_all(ad_image, "\\?(.*)", ""))) %>%
    summarise(ad_images = paste(paste0("<img height=200px width=350px src=", 
                                       ad_image, ">"), collapse = ""))

# ---- GA & FB ----
col_args <- function(){
    cols(.default = col_number(),
         utm_campaign = col_character(),
         Platform = col_character(),
         Date = col_date(format = ""))
}
fb <- read_csv("static-data/fb.csv", col_types = col_args())
ga <- read_csv("static-data/ga.csv", col_types = col_args())
ga_fb <- read_csv("static-data/ga_fb.csv",
                  col_types = cols(
                      .default = col_number(),
                      utm_campaign = col_character(),
                      cohort = col_character(),
                      country = col_character(),
                      region = col_character(),
                      age = col_character(),
                      target = col_character(),
                      device_type = col_character(),
                      creative_type = col_character(),
                      creative_strategy = col_character(),
                      theme = col_character(),
                      ad_format = col_character(),
                      pic_source = col_character(),
                      copy_type = col_character(),
                      landing_page = col_character(),
                      product_category = col_character(),
                      products = col_character(),
                      creative = col_character(),
                      Platform = col_character(),
                      prospecting = col_logical(),
                      Date = col_date(format = ""))) %>%
    mutate(Amount_Spent_USD = Amount_Spent_AUD * aud_to_usd) %>%
    left_join(fb_images %>%
                  rename(utm_campaign = ad_name),
              by = "utm_campaign") %>%
    rename(creative_no_image = creative) %>%
    mutate(creative = coalesce(ad_images, creative_no_image))

# ---- COHORTS ----
cohort_assigments <- all_touches %>%
    transmute(email, assigned_cohort = cohort) %>%
    unique()

comp_choices <- c("Spend (USD)","Purchases","CAC","CTR","CPAC","CPL",
                  "T.O.S.","Sessions","Total Carts","Bounce Rate")

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
        "AND completed_at >= '2015-12-21 06:43:34'"))) %>%
    collect() %>%
    group_by(order_id) %>%
    mutate(gross_extra_attributed = (item_total - sum(price)) / n(),
           net_extra_attributed = (total - sum(price)) / n(),
           conversion_rate = ifelse(currency == "AUD", aud_to_usd, 1),
           sales_usd = (price + net_extra_attributed) * conversion_rate,
           gross_revenue_usd = (price + gross_extra_attributed) * conversion_rate,
           adjustments_total_percentage = o_adjustments / item_total,
           adjustments_usd = gross_revenue_usd * adjustments_total_percentage,
           order_date = as_date(completed_timestamp)) %>%
    ungroup()

# ---- CUSTOMIZATIONS + HEIGHTS ----
customizations <- tbl(fp_con, sql(paste(
    "SELECT",
        "lip.line_item_id,",
        "MAX(CASE WHEN lip.customization_value_ids SIMILAR TO '%([1-9])%'",
            "THEN 1 ELSE 0 END) customized,",
        "STRING_AGG(DISTINCT lip.size, ', ') lip_size,",
        "STRING_AGG(DISTINCT lip.customization_value_ids, '/n') customisation_value_ids,",
        "INITCAP(STRING_AGG(DISTINCT lip.color, ', ')) color,",
        "INITCAP(STRING_AGG(DISTINCT lip.height, ', ')) lip_height",
    "FROM line_item_personalizations lip",
    "WHERE lip.line_item_id IN (",
    paste(ordered_units$line_item_id, collapse = ","), ")",
    "GROUP BY line_item_id"))) %>%
    collect()

line_item_customizations <- customizations %>%
    filter(str_detect(customisation_value_ids, "[0-9]")) %>% 
    mutate(parsed_ids = 
               str_replace_all(customisation_value_ids, "\n|'| |---", "") %>% 
               substr(2, 20)) %>% 
    select(line_item_id, parsed_ids) %>%
    separate(parsed_ids, sep = "-", paste0("id",seq(1,5)), 
             extra = "warn", fill = "right") %>% 
    gather(which_cust_id, customization_value_id_char, -line_item_id, na.rm = TRUE) %>%
    transmute(line_item_id,
              customization_value_id = as.integer(customization_value_id_char))

customization_values <- tbl(fp_con, sql(paste(
    "SELECT id customization_value_id, presentation, price",
    "FROM customisation_values",
    "WHERE id IN (",
    line_item_customizations$customization_value_id %>%
        unique() %>%
        paste(collapse = ","),
    ")"))) %>%
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
        "SELECT product_id, STRING_AGG(DISTINCT UPPER(sku), ',') style",
        "FROM spree_variants",
        "WHERE is_master",
        "GROUP BY product_id",
    ") g ON g.product_id = p.id"))) %>%
    collect()

# ---- ADDRESSES ----
addresses <- tbl(fp_con, sql(paste(
    "SELECT",
        "sa.id ship_address_id,",
        "INITCAP(sa.city) ship_city,",
        "INITCAP(COALESCE(ss.name, sa.state_name)) ship_state,",
        "INITCAP(sc.name) ship_country",
    "FROM spree_addresses sa",
    "LEFT JOIN spree_states ss",
        "ON ss.id = sa.state_id",
    "LEFT JOIN spree_countries sc",
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

# ---- SHIP DATE CORRECTIONS ----
correct_shipments <- read_csv(
    "static-data/Correct Ship Dates.csv",
    col_types = cols(LINE = col_number(),
                     `SENT DATE` = col_date(format = ""))) %>%
    rename(line_item_id = LINE,
           correct_ship_date = `SENT DATE`) %>%
    filter(!is.na(line_item_id))
correct_shipments$line_item_id <- as.integer(correct_shipments$line_item_id)

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
        "AND a.attachment_file_name ilike '%front-crop%'",
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

# ---- SLOW AND FAST MAKING ----
slow_fast_items <- tbl(fp_con, sql(paste(
    "SELECT limo.line_item_id, pmo.option_type making_option",
    "FROM line_item_making_options limo",
    "INNER JOIN product_making_options pmo",
        "ON pmo.id = limo.making_option_id"))) %>%
    collect()

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
    left_join(cohort_assigments, by = "email") %>%
    left_join(correct_shipments %>%
                  group_by(line_item_id) %>%
                  summarise(correct_ship_date = min(correct_ship_date)), 
              by = "line_item_id") %>%
    left_join(slow_fast_items, by = "line_item_id") %>%
    left_join(customer_aquisitions %>%
                  group_by(email) %>%
                  summarise(acquisition_date = min(date)) %>%
                  rbind(ordered_units %>%
                            anti_join(customer_aquisitions, by = "email") %>%
                            group_by(email) %>%
                            summarise(acquisition_date = min(order_date))),
              by = "email") %>%
    group_by(order_id) %>%
    mutate(payments = coalesce(order_payments / n(), 0),
           item_total_usd = item_total * conversion_rate,
           promotions_usd = gross_revenue_usd * (o_promotions / item_total),
           shipping_usd = gross_revenue_usd * (o_shipping / item_total),
           taxes_usd = gross_revenue_usd * (o_taxes / item_total),
           other_adjustments_usd = gross_revenue_usd * (o_other_adjustments / item_total)) %>%
    ungroup() %>%
    mutate(ship_date = coalesce(correct_ship_date, li_ship_date, o_ship_date),
           refund_amount_usd = (refund_amount / 100) * ifelse(currency == "AUD", aud_to_usd, 1),
           price_usd = price * ifelse(currency == "AUD", aud_to_usd, 1),
           height = paste0(substr(lip_height, 1, 1) %>% toupper(), substr(lip_height, 2, 250)),
           size = coalesce(g_size, lip_size),
           return_order_id = ifelse(!is.na(acceptance_status), order_id, NA),
           product_live = ifelse(!hidden 
                                 & (is.na(deleted_at) | deleted_at > today()) 
                                 & (available_on <= today()),
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
           payment_processing_cost = (sales_usd * 0.029) + 0.3,
           physically_customized = ifelse(is.na(customized), 0, customized),
           repeat_purchase = order_date > acquisition_date) %>%
    left_join(collections %>%
                  group_by(product_id) %>%
                  summarise(collection_na = paste(unique(collection_na), collapse = ", ")),
              by = "product_id") %>%
    mutate(collection = ifelse(is.na(collection_na), "2014-2015 - Old", collection_na)) %>%
    select(-collection_na) %>%
    separate(size, c("us_size_str","au_size_str"), sep = "/", remove = FALSE) %>%
    mutate(us_size = as.integer(str_replace_all(us_size_str, "US", ""))) %>%
    left_join(factory_costs %>%
                  group_by(style_number) %>%
                  summarise(manufacturing_cost = mean(manufacturing_cost)),
              by = "style_number") %>%
    # Filter out Lacey's PR Order discounted improperly
    filter(order_id != 26075489) %>%
    # Filter out mysterious Manual order that's not right
    filter(order_id != 32162864) %>%
    # Filter PR orders
    filter(email != "abbyv@fameandpartners.com") %>%
    filter(adjustments_total_percentage > -0.7) %>%
    filter(payments != 0) %>%
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
    levels = c("Petite", "Standard", "Tall", paste0("Length", 1:6))
)

# ---- BUDGET DATA ----

monthly_budget_2017 <- read_csv("static-data/direct_2017_monthly_budget.csv",
                                col_types = "iddddddddddddddd")

monthly_actuals_2017 <- products_sold %>%
    filter(is_shipped & order_state != "canceled" & year(ship_date) >= 2016) %>%
    group_by(ship_year = year(ship_date),
             ship_month = month(ship_date)) %>%
    summarise(gross_revenue = sum(gross_revenue_usd),
              net_sales = sum(sales_usd),
              units_shipped = sum(quantity),
              cogs = sum(coalesce(manufacturing_cost, 70))
                    + sum(li_shipping_cost)
                    + sum(payment_processing_cost),
              returns = sum(coalesce(refund_amount_usd, 0)),
              total_adjustments = sum(adjustments_usd)) %>%
    mutate(average_selling_price = gross_revenue / units_shipped,
           average_unit_cogs = cogs / units_shipped,
           return_rate = returns / gross_revenue,
           gross_margin = (gross_revenue + total_adjustments - cogs - returns)
                        / (gross_revenue + total_adjustments - returns),
           returns_per_unit = returns / units_shipped,
           average_discount = abs(total_adjustments) / gross_revenue)
# NOTES:
# 98% of returns are processed within 90 days
# less_than_90_days n `n/sum(n)`
# 1 FALSE           87 0.01549145
# 2 TRUE          5529 0.98450855

# # 74% of refund request dollars are refunded
# # This observation drives the crude estimate of 0.65 * refunds requesteds
# # We anticipate that this rate will drop this year because of bridal returns
# # refunded
# # 1 0.7391868

monthly_budget_actuals_2017 <- monthly_actuals_2017 %>%
    gather(metric, value, -ship_month, -ship_year) %>%
    mutate(year_colname = paste("actuals", ship_year, sep = "_")) %>%
    ungroup() %>%
    select(-ship_year) %>%
    spread(year_colname, value) %>%
    filter(!is.na(actuals_2017)) %>%
    inner_join(monthly_budget_2017 %>%
                   gather(metric, budget_2017, -ship_month),
               by = c("ship_month","metric")) %>%
    transmute(ship_quarter = ceiling(ship_month / 3),
              ship_month,
              metric,
              actuals_2016 = round(actuals_2016, 2),
              actuals_2017 = round(actuals_2017, 2),
              budget_2017  = round(budget_2017, 2),
              percent_change_yoy = (actuals_2017 - actuals_2016) / actuals_2016,
              percent_of_budget = actuals_2017 / budget_2017) %>%
    arrange(metric, ship_month)

quarterly_budget_2017 <- monthly_budget_2017 %>%
    group_by(ship_quarter = ceiling(ship_month / 3)) %>%
    summarise(gross_revenue = sum(gross_revenue),
              units_shipped = sum(units_shipped),
              cogs = sum(cogs),
              returns = sum(returns)) %>%
    mutate(average_selling_price = gross_revenue / units_shipped,
           average_unit_cogs = cogs / units_shipped,
           return_rate = returns / gross_revenue,
           gross_margin = (gross_revenue - returns - cogs)
                        / (gross_revenue - returns),
           returns_per_unit = returns / units_shipped) %>%
    gather(metric, budget_2017, -ship_quarter)

quarterly_budget_actuals_2017 <- monthly_actuals_2017 %>%
    group_by(ship_year,
             ship_quarter = ceiling(ship_month / 3)) %>%
    summarise(gross_revenue = sum(gross_revenue),
              units_shipped = sum(units_shipped),
              cogs = sum(cogs),
              returns = sum(returns),
              total_adjustments = sum(total_adjustments)) %>%
    mutate(average_selling_price = gross_revenue / units_shipped,
           average_unit_cogs = cogs / units_shipped,
           return_rate = returns / gross_revenue,
           gross_margin = (gross_revenue + total_adjustments - cogs - returns)
           / (gross_revenue + total_adjustments - returns),
           returns_per_unit = returns / units_shipped) %>%
    gather(metric, value, -ship_quarter, -ship_year) %>%
    mutate(year_colname = paste("actuals", ship_year, sep = "_")) %>%
    ungroup() %>%
    select(-ship_year) %>%
    spread(year_colname, value) %>%
    filter(!is.na(actuals_2017)) %>%
    inner_join(quarterly_budget_2017, by = c("ship_quarter", "metric")) %>%
    mutate(percent_change_yoy = (actuals_2017 - actuals_2016) / actuals_2016,
           percent_of_budget = round(actuals_2017 / budget_2017, 4)) %>%
    transmute(ship_quarter, metric,
              actuals_2016 = round(actuals_2016, 2),
              actuals_2017 = round(actuals_2017, 2),
              budget_2017  = round(budget_2017, 2),
              percent_change_yoy, percent_of_budget)

annual_budget_2017 <- monthly_budget_2017 %>%
    summarise(gross_revenue = sum(gross_revenue),
              units_shipped = sum(units_shipped),
              cogs = sum(cogs),
              returns = sum(returns)) %>%
    mutate(average_selling_price = gross_revenue / units_shipped,
           average_unit_cogs = cogs / units_shipped,
           return_rate = returns / gross_revenue,
           gross_margin = (gross_revenue - returns - cogs)
           / (gross_revenue - returns),
           returns_per_unit = returns / units_shipped) %>%
    gather(metric, budget_2017)

annual_budget_actuals_2017 <- monthly_actuals_2017 %>%
    group_by(ship_year) %>%
    summarise(gross_revenue = sum(gross_revenue),
              units_shipped = sum(units_shipped),
              cogs = sum(cogs),
              returns = sum(returns),
              total_adjustments = sum(total_adjustments)) %>%
    mutate(average_selling_price = gross_revenue / units_shipped,
           average_unit_cogs = cogs / units_shipped,
           return_rate = returns / gross_revenue,
           gross_margin = (gross_revenue + total_adjustments - cogs - returns)
           / (gross_revenue + total_adjustments - returns),
           returns_per_unit = returns / units_shipped) %>%
    gather(metric, value, -ship_year) %>%
    mutate(year_colname = paste("actuals", ship_year, sep = "_")) %>%
    ungroup() %>%
    select(-ship_year) %>%
    spread(year_colname, value) %>%
    inner_join(annual_budget_2017, by = c("metric")) %>%
    transmute(metric,
              actuals_2016 = round(actuals_2016, 2),
              actuals_2017 = round(actuals_2017, 2),
              budget_2017  = round(budget_2017, 2),
              percent_change_yoy = (actuals_2017 - actuals_2016) / actuals_2016,
              percent_of_budget = round(actuals_2017 / budget_2017, 4))

