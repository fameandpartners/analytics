library(readr)
library(RPostgreSQL)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(scales)
library(ggplot2)
library(shiny)
library(DT)

source("fp_init.R")

cap1 <- function(string){
    paste0(toupper(substr(string, 1, 1)), tolower(substr(string, 2, 25)))
}

year_month <- function(date_value){
    paste(year(date_value), 
          formatC(month(date_value), width = 2, flag = "0"), 
          sep = "-")
}

collections <- read_csv("static-data/collections_2.csv", col_types = "iccccc") %>%
    transmute(product_id = `Product ID`,
              collection_na = Collection)

time_dim <- data_frame(date_value = seq(as.Date("2016-01-01"), today(), 1)) %>%
    mutate(year_value = year(date_value),
           week_value = week(date_value)) %>%
    group_by(year_value, week_value) %>%
    mutate(week_start_date = min(date_value)) %>%
    ungroup() %>%
    select(date_value, week_start_date)

products_sold <-
    tbl(fp_con, sql(paste(
        "SELECT",
            "li.id line_item_id,",
            "li.order_id,",
            "o.number order_number,",
            "o.state order_state,",
            "o.shipment_state,",
            "li.quantity,",
            "li.price,",
            "o.total / (COUNT(*) OVER (PARTITION BY li.order_id)) order_total,",
            "li.currency,",
            "INITCAP(sa.city) ship_city,",
            "INITCAP(ss.name) ship_state,",
            "INITCAP(sc.name) ship_country,",
            "o.completed_at::date order_date,",
            "CASE WHEN s.ship_date IS NULL THEN o.projected_delivery_date::DATE ELSE s.ship_date::DATE END ship_date,",
            "o.email,",
            "o.user_id,",
            "INITCAP(o.user_first_name) || ' ' || INITCAP(o.user_last_name) customer_name,",
            "v.product_id product_id,",
            "INITCAP(p.name) style_name,",
            "UPPER(style.number) style_number,",
            "ir.refund_amount IS NOT NULL item_returned,",
            "ir.refund_amount / 100 refund_amount,",
            "INITCAP(TRIM(ir.reason_category)) return_reason_extra,",
            "CASE WHEN ir.id IS NOT NULL THEN li.order_id END return_order_id,",
            "COALESCE(cust.physical_customization, 0) physically_customized,",
            "RANK() OVER (PARTITION BY o.email ORDER BY o.completed_at) order_num",
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
                    "THEN 1 ELSE 0 END) physical_customization",
            "FROM line_item_personalizations lip",
            "LEFT JOIN product_color_values pcv",
                "ON pcv.id = lip.color_id",
            "GROUP BY line_item_id) cust",
            "ON cust.line_item_id = li.id",
        "LEFT JOIN (",
            "SELECT order_id, MAX(shipped_at::DATE) ship_date",
            "FROM spree_shipments",
            "GROUP BY order_id) s",
            "ON s.order_id = li.order_id",
        "LEFT JOIN (",
            "SELECT product_id, STRING_AGG(DISTINCT style_number, ',') number",
            "FROM global_skus",
            "GROUP BY product_id) style",
            "ON style.product_id = v.product_id",
        "WHERE o.completed_at IS NOT NULL",
            "AND o.completed_at >= '2016-01-01'",
            "AND o.payment_state = 'paid'"))) %>%
    collect(n = Inf) %>%
    mutate(return_reason = ifelse(
        is.na(return_reason_extra) 
        | return_reason_extra %in% c("N/A","Na","Not Specified","Not Stated","Not Satisfied"),
        "No Reason",
        return_reason_extra)) %>%
    left_join(collections, by = "product_id") %>%
    mutate(collection = ifelse(is.na(collection_na), "Old", collection_na)) %>%
    select(-collection_na) %>%
    mutate(ship_year_month = year_month(ship_date),
           order_year_month = year_month(order_date),
           revenue_usd = order_total * ifelse(currency == "AUD", 0.75, 1),
           refund_amount_usd = refund_amount * ifelse(currency == "AUD", 0.75, 1),
           order_status = 
               ifelse(item_returned, "Returned",
               ifelse(order_state != "canceled"
                      & (is.na(shipment_state) | shipment_state == "partial"),
                      "Paid",
               ifelse(order_state != "canceled", cap1(shipment_state), 
                      cap1(order_state))))) %>%
    left_join(time_dim, by = c("order_date" = "date_value")) %>%
    rename(order_date_week_starting = week_start_date)







# returns <- tbl(fp_con, sql(paste(
#     "SELECT",
#         "ir.reason_category,",
#         "ir.reason_sub_category,",
#         "SUM(li.quantity) items",
#     "FROM item_returns ir",
#     "INNER JOIN spree_line_items li",
#         "ON li.id = ir.line_item_id",
#     "INNER JOIN spree_orders o",
#         "ON o.id = li.order_id",
#     "WHERE o.completed_at IS NOT NULL ",
#         "AND ir.requested_action = 'return'",
#         "AND o.completed_at >= CURRENT_DATE - INTERVAL '12 months'",
#     "GROUP BY",
#         "ir.reason_category,",
#         "ir.reason_sub_category"))) %>%
#     collect() %>%
#     mutate(Reason = ifelse(reason_category == "Poor quality or faulty",
#                            "Poor quality\nor faulty",
#                     ifelse(reason_category == "Looks different to image on site",
#                            "Looks different to\nimage on site",
#                     ifelse(reason_category == "Ordered multiple styles or sizes",
#                            "Ordered multiple\nstyles or sizes",
#                            reason_category))),
#            sub_reason = 
#                ifelse(is.na(reason_sub_category) | str_detect(tolower(reason_sub_category),"not state|reason|n/a|na|cancel|not specifelseied|charged twice|not spec"),"No Reason",
#                ifelse(str_detect(tolower(reason_sub_category), "bust"),"Bust",
#                ifelse(str_detect(tolower(reason_sub_category),"waist"),"Waist", 
#                ifelse(str_detect(tolower(reason_sub_category), "long|short|length"), "Length", 
#                ifelse(str_detect(tolower(reason_sub_category), "hips"), "Hips", 
#                ifelse(str_detect(tolower(reason_sub_category), "fit|small|big|size|too low|tight|on the body"), "Fit",
#                ifelse(str_detect(tolower(reason_sub_category),"late|delivery|ship|transit|received part|event"),"Shipment",
#                ifelse(str_detect(tolower(reason_sub_category),"difficult to choose|not sure"), "Difficult to Choose", 
#                ifelse(str_detect(tolower(reason_sub_category),"expectations|not clear|difelseferent|does not match|on the website|wrong"), "Missed Expectations", 
#                ifelse(str_detect(tolower(reason_sub_category),"poor quality|stain|poorly made|damaged|difelseferent style|marks|customisation|cheap"), "Quality", 
#                "No Reason")))))))))))  %>%
#     rename(`Sub Reason` = sub_reason)
# 
# returns$`Sub Reason` <- factor(
#     returns$`Sub Reason`,
#     levels = c("Fit","Length","Bust","Waist","Hips","Quality","Shipment","Missed Expectations","Difficult to Choose","No Reason")
# )