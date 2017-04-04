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
    require(magrittr)
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

# ---- DATA ----

# query conversion rates
aud_to_usd <- 0.77  # query_aud_to_usd()

# read collections data
collections <- read_csv("static-data/collections_2.csv", 
                        col_types = "iccccc") %>%
    transmute(product_id = `Product ID`,
              collection_na = Collection)

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

# factory manufacturing cost data
# still missing some costs
factory_costs <- read_csv("static-data/eCommerce Factory Cost.csv",
                          col_types = cols(
                              StyleNumber = col_character(),
                              `Unit Price` = col_double())) %>%
    transmute(style_number = toupper(StyleNumber), manufacturing_cost = `Unit Price`)

shipping_costs <- read_csv("static-data/avg_shipping_costs.csv",
                           col_types = cols(
                               YearNum = col_integer(),
                               MonthNum = col_integer(),
                               USD = col_double())) %>%
    rename(ship_year = YearNum, ship_month = MonthNum, avg_order_shipping_cost = USD) %>%
    mutate(ship_year_month = year_month(as.Date(paste(ship_year, ship_month, 1, sep = "-"))))

# set db connection
source("fp_init.R")

# min completed_at for orders shipped in jan
completed_at <- tbl(fp_con, sql(paste(
    "SELECT MIN(o.completed_at) FROM spree_orders o",
    "LEFT JOIN spree_shipments s ON s.order_id = o.id",
    "WHERE COALESCE(s.shipped_at, o.completed_at::DATE + 10) >= '2016-01-01'",
    "AND o.completed_at IS NOT NULL"))) %>%
    collect()

sql_convert_to_LA_time <- function(utc_time, column_alias){
    paste0("(", utc_time, 
           " at time zone 'UTC') at time zone 'America/Los_Angeles' ",
           column_alias)
}

# query taxons
product_taxons <- tbl(fp_con, sql(paste(
    "SELECT pt.product_id, t.name taxon_name",
    "FROM spree_products_taxons pt",
    "JOIN spree_taxons t on t.id = pt.taxon_id"))) %>%
    collect()

dress_images <- collect(tbl(fp_con, sql(paste(
    "select p.id product_id, a.id asset_id, a.attachment_file_name, ",
    "a.attachment_width, a.attachment_height",
    "from spree_assets a",
    "join product_color_values pcv ",
    "on a.viewable_id = pcv.id",
    "join spree_products p",
    "on p.id = pcv.product_id",
    "WHERE a.attachment_width < 2000",
        "AND a.viewable_type = 'ProductColorValue'",
        "AND a.attachment_updated_at >= '2015-01-01'")))) %>%
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

# query sales
products_sold <- tbl(fp_con, sql(paste(
        "SELECT",
            "li.id line_item_id,",
            "li.order_id,",
            "o.number order_number,",
            "o.state order_state,",
            "o.payment_state,",
            "li.quantity,",
            "li.price,",
        # ((Order Total Price - Order Subtotal) / Quantity of Orders) + Price
        # Equally distribute non item specific revenue
            "(",
                "((o.total - (SUM(li.price) OVER (PARTITION BY li.order_id)))",
                "/ (COUNT(*) OVER (PARTITION BY li.order_id)))",
                "+ li.price",
            ") * CASE WHEN o.currency = 'AUD' THEN", aud_to_usd, "ELSE 1 END sales_usd,",
            "(",
                "((o.item_total - (SUM(li.price) OVER (PARTITION BY li.order_id)))",
                "/ (COUNT(*) OVER (PARTITION BY li.order_id)))",
                "+ li.price",
            ") * CASE WHEN o.currency = 'AUD' THEN", aud_to_usd, "ELSE 1 END gross_revenue_usd,",
            "o.adjustment_total / (COUNT(*) OVER (PARTITION BY li.order_id))",
                "* CASE WHEN o.currency = 'AUD' THEN", aud_to_usd, "ELSE 1 END adjustments_usd,",
            "li.currency,",
            "loc.ship_city,",
            "loc.ship_state,",
            "loc.ship_country,",
            "o.completed_at::date order_date,",
            "COALESCE(s.ship_date, o.completed_at::DATE + 10) ship_date,",
            "COALESCE(s.ship_date <= current_date, FALSE) is_shipped,",
            "o.email,",
            "o.user_id,",
            "INITCAP(o.user_first_name) || ' ' || INITCAP(o.user_last_name) customer_name,",
            "p.id product_id,",
            "INITCAP(p.name) style_name,",
            "UPPER(style.number) style_number,",
            "ir.refund_amount IS NOT NULL item_returned,",
            "ir.refund_amount / 100 * CASE WHEN o.currency = 'AUD' THEN", aud_to_usd, "ELSE 1 END refund_amount_usd,",
            "CASE",
                "WHEN o.state = 'canceled' THEN 'Canceled'",
                "WHEN ir.refund_amount IS NOT NULL THEN 'Returned'",
                "WHEN rri.line_item_id IS NOT NULL THEN 'Refund Requested'",
                "WHEN s.ship_date <= current_date THEN 'Shipped'",
            "ELSE 'Paid' END order_status,",
            "CASE WHEN LOWER(ir.reason_category) IN ('n/a','na','not specified','not stated','not satisfied')",
                "THEN 'No Reason' ELSE INITCAP(TRIM(ir.reason_category))",
            "END return_reason,",
            "ir.reason_sub_category,",
            "rri.return_request_action,",
            "CASE WHEN ir.id IS NOT NULL THEN li.order_id END return_order_id,",
            "COALESCE(cust.physical_customization, 0) physically_customized,",
            "cust.color,",
            "COALESCE(cust.size, gsku.size) size,",
            "cust.height,",
            "RANK() OVER (PARTITION BY o.email ORDER BY o.completed_at) order_num,",
            "CASE WHEN NOT p.hidden AND (p.deleted_at IS NULL OR p.deleted_at > CURRENT_DATE) AND p.available_on <= CURRENT_DATE",
                "THEN 'Yes' ELSE 'No' END product_live,",
            "li.price * CASE WHEN o.currency = 'AUD' THEN", aud_to_usd, "ELSE 1 END price_usd,",
            "s.ship_states,",
            "rri.line_item_id IS NOT NULL return_requested,",
            "rri.return_requested_at,",
            "ir.refunded_at,",
            "pay.o_lvl_payments::DECIMAL / (COUNT(*) OVER (PARTITION BY li.order_id)) payments",
        "FROM spree_line_items li",
        "LEFT JOIN spree_orders o",
            "ON o.id = li.order_id",
        "LEFT JOIN (",
            "SELECT",
                "p.id,",
                "p.hidden,",
                "p.deleted_at,",
                "p.available_on,",
                "p.name,",
                "v.id variant_id,",
                "v.sku variant_sku",
            "FROM spree_variants v",
            "LEFT JOIN spree_products p",
                "ON p.id = v.product_id",
        ") p ON p.variant_id = li.variant_id",
        "LEFT JOIN (",
            "SELECT",
                "sa.id,",
                "INITCAP(sa.city) ship_city,",
                "INITCAP(ss.name) ship_state,",
                "INITCAP(sc.name) ship_country",
            "FROM spree_addresses sa",
            "INNER JOIN spree_states ss",
                "ON ss.id = sa.state_id",
            "INNER JOIN spree_countries sc",
                "ON sc.id = sa.country_id",
        ") loc ON loc.id = o.ship_address_id",
        "LEFT JOIN (",
            "SELECT",
                "id,",
                "refunded_at,",
                "line_item_id,",
                "refund_amount,",
                "reason_category,",
                "reason_sub_category",
            "FROM item_returns",
            "WHERE acceptance_status != 'rejected'",
                "AND line_item_id IS NOT NULL",
        ") ir ON ir.line_item_id = li.id",
        "LEFT JOIN (",
            "SELECT", 
                "lip.line_item_id,",
                "MAX(CASE WHEN lip.customization_value_ids SIMILAR TO '%([1-9])%'",
                    "THEN 1 ELSE 0 END) physical_customization,",
                "STRING_AGG(DISTINCT lip.size, ', ') size,",
                "INITCAP(STRING_AGG(DISTINCT lip.color, ', ')) color,",
                "INITCAP(STRING_AGG(DISTINCT lip.height, ', ')) height",
            "FROM line_item_personalizations lip",
            "GROUP BY line_item_id",
        ") cust ON cust.line_item_id = li.id",
        "LEFT JOIN (",
            "SELECT order_id, MAX(shipped_at::DATE) ship_date, STRING_AGG(DISTINCT state, ',') ship_states",
            "FROM spree_shipments",
            "GROUP BY order_id",
        ") s ON s.order_id = li.order_id",
        "LEFT JOIN (",
            "SELECT",
                "product_id,",
                "sku,",
                "size",
            "FROM global_skus",
        ") gsku ON gsku.sku = p.variant_sku",
        "LEFT JOIN (",
            "SELECT product_id, STRING_AGG(DISTINCT style_number, ',') number",
            "FROM global_skus",
            "GROUP BY product_id",
        ") style ON style.product_id = p.id",
        "LEFT JOIN (",
            "SELECT",
                "line_item_id,",
                "STRING_AGG(DISTINCT reason_category, ', ') reason_category,",
                "STRING_AGG(DISTINCT reason, ', ') reason_sub_category,",
                "STRING_AGG(DISTINCT action, ', ') return_request_action,",
                "MAX(created_at) return_requested_at",
            "FROM return_request_items",
            "WHERE action != 'keep'",
            "GROUP BY line_item_id",
        ") rri ON rri.line_item_id = li.id",
        "LEFT JOIN (",
            "SELECT order_id, COUNT(*) o_lvl_payments",
            "FROM spree_payments",
            "WHERE state = 'completed'",
            "GROUP BY order_id",
        ") pay ON pay.order_id = o.id",
        "WHERE o.completed_at IS NOT NULL",
            "AND o.completed_at >=", paste0("'", completed_at$min %>% as.character(), "'"),
            "AND o.payment_state = 'paid'",
            "AND o.total > 0"
        ))) %>%
    collect(n = Inf) %>%
    mutate(estimated_ship_date = ifelse(is.na(ship_date), order_date + 10, ship_date) %>% as.Date(origin = "1970-01-01"),
           ship_year_month = year_month(estimated_ship_date),
           order_year_month = year_month(order_date),
           payment_processing_cost = sales_usd * 0.029 * payments) %>%
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
    levels = c("Petite", "Standard", "Tall")
)

# ---- Budget Data ----

monthly_budget_2017 <- read_csv("static-data/direct_2017_monthly_budget.csv",
                                col_types = "iddddddddddddddd")

monthly_actuals_2017 <- products_sold %>%
    filter(is_shipped & year(ship_date) >= 2016) %>%
    mutate(estimated_returns = ifelse(# See NOTES
        ship_date >= today() - 90,
        coalesce(refund_amount_usd, return_requested * sales_usd * 0.65),
        coalesce(refund_amount_usd, 0))) %>%
    group_by(ship_year = year(ship_date), 
             ship_month = month(ship_date)) %>%
    summarise(gross_revenue = sum(gross_revenue_usd),
              units_shipped = sum(quantity),
              cogs = sum(coalesce(manufacturing_cost, 70)) 
                    + sum(li_shipping_cost) 
                    + sum(payment_processing_cost),
              returns = sum(estimated_returns),
              total_adjustments = sum(adjustments_usd)) %>%
    mutate(average_selling_price = gross_revenue / units_shipped,
           average_unit_cogs = cogs / units_shipped,
           return_rate = returns / gross_revenue,
           gross_margin = (gross_revenue + total_adjustments - cogs - returns)
                        / (gross_revenue + total_adjustments - returns),
           returns_per_unit = returns / units_shipped,
           average_discount = abs(total_adjustments) / gross_revenue)
# NOTES:
# 98% of returns are processed within 90 days, which is also our policy
# products_sold %>%
#     filter(!is.na(refunded_at)) %>%
#     mutate(ship_to_return_request = difftime(refunded_at,
#                                              ship_date,
#                                              units = "days") %>% round(),
#            less_than_90_days = ship_to_return_request < 90) %>%
#     ggplot(aes(x = ship_to_return_request, fill = less_than_90_days)) +
#     geom_histogram(binwidth = 1) +
#     scale_x_continuous(breaks = seq(0, 150, 10), limits = c(0, 150))
# products_sold %>%
#     filter(!is.na(refunded_at)) %>%
#     mutate(ship_to_return_request = difftime(refunded_at,
#                                              ship_date,
#                                              units = "days") %>% round(),
#            less_than_90_days = ship_to_return_request < 90) %>%
#     count(less_than_90_days) %>%
#     mutate(n / sum(n))
# less_than_90_days n `n/sum(n)`
#  <lgl>          <int>   <dbl>
# 1 FALSE           87 0.01549145
# 2 TRUE          5529 0.98450855

# # 74% of refund request dollars are refunded
# # This observation drives the crude estimate of 0.65 * refunds requesteds
# # We anticipate that this rate will drop this year because of bridal returns
# products_sold %>%
#     filter(return_requested & ship_date < today() - 90) %>%
#     summarise(refunded = sum(coalesce(refund_amount_usd, 0)) / sum(sales_usd))
# # refunded
# #   <dbl>
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