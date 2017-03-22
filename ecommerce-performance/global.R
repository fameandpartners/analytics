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
str_right <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
}

cap1 <- function(string){
    paste0(toupper(substr(string, 1, 1)), tolower(substr(string, 2, 25)))
}

year_month <- function(date_value){
    paste(year(date_value), 
          formatC(month(date_value), width = 2, flag = "0"), 
          sep = "-")
}

short_dollar <- function(number){
    ifelse(number < 1000, scales::dollar(number),
        ifelse(number >= 1000 & number < 1000000,
            paste0("$", round(number / 1000, 1), "K"),
            ifelse(number >= 1000000,
                paste0("$", round(number / 1000000, 1), "M"),
                paste0("$", round(number / 1000000000, 1), "B"))))
}

short_number <- function(number){
    ifelse(number < 1000, number,
           ifelse(number >= 1000 & number < 1000000,
                  paste0(round(number / 1000, 1), "K"),
                  ifelse(number >= 1000000,
                         paste0((round(number / 1000000, 1)), "M"),
                         paste0(round(number / 1000000000, 1), "B"))))
}

short_number_space <- function(number){
    paste0(short_number(number), " ")
}

h3c <- function(x){h3(x, align = "center")}

abbr_month <- function(date_value){
    month(date_value, label = TRUE)
}

query_aud_to_usd <- function(){
    resp <- GET("https://api.fixer.io/latest?base=AUD")
    if(resp$status_code == 200){
        return(content(resp)$rates$USD)
    } else { return(0.75) }
}

# ---- DATA ----

# query conversion rates
aud_to_usd <- query_aud_to_usd()

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
                        ))

# set db connection
source("fp_init.R")

# min completed_at for orders shipped in jan
completed_at <- tbl(fp_con, sql(paste(
    "SELECT MIN(o.completed_at) FROM spree_orders o",
    "LEFT JOIN spree_shipments s ON s.order_id = o.id",
    "WHERE COALESCE(s.shipped_at, o.completed_at::DATE + 10) >= '2016-01-01'",
    "AND o.completed_at IS NOT NULL"))) %>%
    collect()

# query sales
products_sold <- tbl(fp_con, sql(paste(
        "SELECT",
            "li.id line_item_id,",
            "li.order_id,",
            "o.number order_number,",
            "o.state order_state,",
            "o.shipment_state,",
            "li.quantity,",
            "li.price,",
            "o.total / (COUNT(*) OVER (PARTITION BY li.order_id))",
                "* CASE WHEN o.currency = 'AUD' THEN", aud_to_usd, "ELSE 1 END revenue_usd,",
            "li.currency,",
            "INITCAP(sa.city) ship_city,",
            "INITCAP(ss.name) ship_state,",
            "INITCAP(sc.name) ship_country,",
            "o.completed_at::date order_date,",
            "COALESCE(s.ship_date, o.completed_at::DATE + 10) ship_date,",
            "o.email,",
            "o.user_id,",
            "INITCAP(o.user_first_name) || ' ' || INITCAP(o.user_last_name) customer_name,",
            "v.product_id product_id,",
            "INITCAP(p.name) style_name,",
            "UPPER(style.number) style_number,",
            "ir.refund_amount IS NOT NULL item_returned,",
            "ir.refund_amount / 100 * CASE WHEN o.currency = 'AUD' THEN", aud_to_usd, "ELSE 1 END refund_amount_usd,",
            "CASE",
                "WHEN ir.refund_amount IS NOT NULL THEN 'Returned'",
                "WHEN o.state != 'canceled' AND (o.shipment_state = 'partial' OR o.shipment_state IS NULL) THEN 'Paid'",
                "WHEN o.state != 'canceled' THEN INITCAP(o.shipment_state)",
            "ELSE INITCAP(o.state) END order_status,",
            "CASE WHEN LOWER(ir.reason_category) IN ('n/a','na','not specified','not stated','not satisfied')",
                "THEN 'No Reason' ELSE INITCAP(TRIM(ir.reason_category)) END return_reason,",
            "ir.reason_sub_category,",
            "CASE WHEN ir.id IS NOT NULL THEN li.order_id END return_order_id,",
            "COALESCE(cust.physical_customization, 0) physically_customized,",
            "cust.color,",
            "CASE WHEN cust.size IS NOT NULL THEN cust.size ELSE gsku.size END size,",
            "cust.height,",
            "RANK() OVER (PARTITION BY o.email ORDER BY o.completed_at) order_num,",
            "CASE WHEN NOT p.hidden AND (p.deleted_at IS NULL OR p.deleted_at > CURRENT_DATE) AND p.available_on <= CURRENT_DATE",
                "THEN 'Yes' ELSE 'No' END product_live,",
            "li.price * CASE WHEN o.currency = 'AUD' THEN", aud_to_usd, "ELSE 1 END price_usd,",
            "f.name factory_name,",
            "s.ship_states",
        "FROM spree_line_items li",
        "LEFT JOIN spree_orders o",
            "ON o.id = li.order_id",
        "LEFT JOIN spree_variants v",
            "ON v.id = li.variant_id",
        "LEFT JOIN spree_products p",
            "ON p.id = v.product_id",
        "LEFT JOIN spree_addresses sa",
            "ON sa.id = o.ship_address_id",
        "LEFT JOIN spree_states ss",
            "ON ss.id = sa.state_id",
        "LEFT JOIN spree_countries sc",
            "ON sc.id = sa.country_id",
        "LEFT JOIN item_returns ir",
            "ON ir.line_item_id = li.id",
        "LEFT JOIN (",
            "SELECT", 
                "lip.line_item_id,",
                "MAX(CASE WHEN lip.customization_value_ids SIMILAR TO '%([1-9])%'",
                    "THEN 1 ELSE 0 END) physical_customization,",
                "STRING_AGG(DISTINCT lip.size, ', ') size,",
                "INITCAP(STRING_AGG(DISTINCT lip.color, ', ')) color,",
                "INITCAP(STRING_AGG(DISTINCT lip.height, ', ')) height",
            "FROM line_item_personalizations lip",
            "GROUP BY line_item_id) cust",
            "ON cust.line_item_id = li.id",
        "LEFT JOIN (",
            "SELECT order_id, MAX(shipped_at::DATE) ship_date, STRING_AGG(DISTINCT state, ',') ship_states",
            "FROM spree_shipments",
            "GROUP BY order_id) s",
            "ON s.order_id = li.order_id",
        "LEFT JOIN (",
            "SELECT product_id, STRING_AGG(DISTINCT style_number, ',') number",
            "FROM global_skus",
            "GROUP BY product_id) style",
            "ON style.product_id = v.product_id",
        "LEFT JOIN (",
            "SELECT sku, STRING_AGG(size, ',') size",
            "FROM global_skus",
            "WHERE sku IS NOT NULL and size IS NOT NULL",
            "GROUP BY sku) gsku",
            "ON gsku.sku = v.sku",
        "LEFT JOIN factories f",
            "ON f.id = p.factory_id",
        "WHERE o.completed_at IS NOT NULL",
            "AND o.completed_at >=", paste0("'", completed_at$min %>% as.character(), "'"),
            "AND o.payment_state = 'paid'"
        ))) %>%
    collect(n = Inf) %>%
    left_join(collections %>%
                  group_by(product_id) %>%
                  summarise(collection_na = paste(unique(collection_na), collapse = ", ")), 
              by = "product_id") %>%
    mutate(collection = ifelse(is.na(collection_na), "2014-2015 -  Old", collection_na)) %>%
    select(-collection_na) %>%
    mutate(estimated_ship_date = ifelse(is.na(ship_date), order_date + 10, ship_date) %>% as.Date(origin = "1970-01-01"),
           ship_year_month = year_month(estimated_ship_date),
           order_year_month = year_month(order_date)) %>%
    separate(size, c("us_size_str","au_size_str"), sep = "/", remove = FALSE) %>% 
    mutate(us_size = as.integer(str_replace_all(us_size_str, "US", ""))) %>%
    filter(revenue_usd != 0)

products_sold$order_status <- factor(
    products_sold$order_status,
    levels = c("Paid","Ready","Shipped","Returned","Canceled")
)

products_sold$size <- factor(
    products_sold$size,
    levels = paste0("US", seq(0,22,2), "/AU", seq(4,26,2))
)

products_sold$height <- factor(
    products_sold$height,
    levels = c("Petite", "Standard", "Tall")
)
