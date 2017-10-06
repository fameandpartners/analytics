library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

source("~/code/analytics/ecommerce-performance/fp_init.R")

# ---- 3PL ----
tpl <- read_csv("/Users/Peter 1/Dropbox (Team Fame)/data/3PL/3PL Orders - COMBINED.csv",
                col_types = cols(
                    order_number = col_character(),
                    ship_date = col_date(format = "")
                ))

# ---- Spree DB ----

payment_lkp <-
    tbl(fp_con, sql(paste(
        "SELECT DISTINCT * FROM (",
        # It looks like for these response codes our payment processors only stored
        # the first 24 chars of the string. Why? No idea.
            "SELECT",
                "order_id,",
                "SUBSTRING(response_code FROM 1 FOR 24) response_code,",
                "'spree_payments' response_code_source",
            "FROM spree_payments",
            "WHERE response_code IS NOT NULL",
                "AND source_type = 'Spree::CreditCard'",
            "UNION",
            "SELECT",
                "p.order_id,",
                "pp.transaction_id,",
                "'spree_payments_paypal'",
            "FROM spree_payments p",
            "INNER JOIN spree_paypal_express_checkouts pp",
                "ON p.source_id = pp.id",
            "WHERE pp.transaction_id IS NOT NULL",
                "AND p.source_type = 'Spree::PaypalExpressCheckout' ",
            "UNION",
            "SELECT",
                "li.order_id,",
                "ir.refund_ref,",
                "'item_returns'",
            "FROM item_returns ir",
            "INNER JOIN spree_line_items li",
                "ON ir.line_item_id = li.id",
            "WHERE ir.refund_ref IS NOT NULL",
            "UNION",
            "SELECT",
                "order_id,",
                "refund_ref,",
                "'refund_requests'",
            "FROM refund_requests",
            "WHERE refund_ref IS NOT NULL) p",
        "WHERE p.order_id != 18804392"))) %>%
    collect() 

source("~/code/analytics/etl/full_global.R")
setwd("~/data")

read_paypal <- function(file_name){
    read_csv(file_name,
             col_types = cols(
                 .default = col_character(),
                 Gross = col_number(),
                 Fee = col_number(),
                 Net = col_number(),
                 Balance = col_number())) %>% 
        filter(Type == "Payment Refund") %>%
        transmute(response_code = `Transaction ID`, 
                  amount = Gross,
                  date_char = Date,
                  currency = Currency)
}

read_pin <- function(file_name){
    read_csv(file_name,
             col_types = cols(
                 .default = col_character(),
                 Amount = col_number())) %>% 
        transmute(response_code = Reference, 
                  amount = Amount, 
                  date_char = Date,
                  date = Date %>%
                      substr(1, 10) %>%
                      dmy(),
                  currency = Currency)
}

all_refund_transactions <- bind_rows(
    list(
        read_paypal("~/data/au_paypal_2017-01-01_2017-09-29.csv") %>%
            select(-currency) %>%
            mutate(date = dmy(date_char),
                   currency = "AUD",
                   source = "au_paypal"),
        read_paypal("~/data/us_paypal_2017-01-01_2017-09-29.csv") %>%
            select(-currency) %>%
            mutate(date = mdy(date_char),
                   currency = "USD",
                   source = "us_paypal"),
        read_pin("~/data/refunds-fame-and-partners-01-jan-00-00-30092017.csv") %>%
            mutate(source = "pin"),
        read_csv("~/data/F&P YTD report.csv",
                  col_types = cols(
                      .default = col_character(),
                      `Purchase Amt` = col_double(),
                      `Settlement Amt` = col_double())) %>% 
            filter(!is.na(`Merchant Order Number`) & `Txn Type` == "Refund") %>%
            transmute(response_code = `Merchant Order Number`,
                      amount = `Purchase Amt`,
                      date_char = `Activity Date`,
                      currency = `Purchase Currency`,
                      date = mdy(`Activity Date`),
                      source = "assembly_ytd"),
        read_csv("~/data/stripe_payments_2017-08-02_2017-09-28.csv",
                 col_types = cols(
                     .default = col_character(),
                     `Created (UTC)` = col_datetime(format = ""),
                     `Amount Refunded` = col_double())) %>%
            filter(`Amount Refunded` > 0) %>%
            transmute(response_code = id,
                      amount = `Amount Refunded`,
                      date_char = as.character(`Created (UTC)`),
                      currency = toupper(Currency),
                      date = as.Date(`Created (UTC)`),
                      source = "stripe"))) %>%
    group_by(response_code) %>%
    summarise(amount = sum(abs(amount)),
              currency = paste(unique(currency), collapse = " "),
              date_char = paste(unique(date_char), collapse = " "),
              date = max(date),
              source = paste(unique(source), collapse = " "),
              records = n())

payment_lkp$response_code_source <- factor(
    payment_lkp$response_code_source,
    levels = c("item_returns","refund_requests",
               "spree_payments","spree_payments_paypal")
)

orders <- ordered_units %>%
    left_join(o_shipments, by = "order_id") %>%
    left_join(li_shipments, by = "line_item_id") %>%
    left_join(correct_shipments %>%
                  group_by(line_item_id) %>%
                  summarise(correct_ship_date = min(correct_ship_date)), 
              by = "line_item_id") %>%
    left_join(returns, by = "line_item_id") %>%
    mutate(return_order_id = ifelse(!is.na(acceptance_status), order_id, NA),
           return_requested = !is.na(return_order_id),
           item_returned = !is.na(refund_amount),
           ship_date = coalesce(correct_ship_date, li_ship_date, o_ship_date)) %>%
    group_by(order_id) %>%
    summarise(refund_requested = max(return_requested),
              refund_processed = max(item_returned),
              last_order_date = max(order_date),
              last_ship_date = max(ship_date),
              original_sales_amount = sum(sales_usd),
              db_refund_amount = sum(refund_amount) / 100) %>%
    mutate(estimated_ship_date = coalesce(last_ship_date, 
                                          last_order_date + 10))

match1 <- all_refund_transactions %>%
    inner_join(payment_lkp, by = "response_code") %>%
    left_join(orders, by = "order_id")

matched_refunds <- bind_rows(
    list(match1,
         all_refund_transactions %>%
             anti_join(match1, by = "response_code") %>%
             rename(response_code_dirty = response_code) %>%
             mutate(response_code = substr(response_code_dirty, 1, 24)) %>%
             inner_join(payment_lkp %>%
                            rename(rc = response_code) %>%
                            mutate(response_code = substr(rc, 1, 24)), 
                        by = "response_code") %>%
             left_join(orders, by = "order_id") %>%
             select(-response_code) %>%
             rename(response_code = response_code_dirty))) %>%
    arrange(response_code_source) %>%
    filter(!duplicated(response_code))

missing_refunds <- all_refund_transactions %>%
    anti_join(matched_refunds, by = "response_code")

matched_refunds$response_code_source <- as.character(matched_refunds$response_code_source)

all_refunds <- bind_rows(
    list(matched_refunds %>%
             select(-date_char) %>%
             mutate(match_status = "Found in Database"),
         missing_refunds %>%
             select(-date_char) %>%
             mutate(refund_requested = 0,
                    refund_processed = 0,
                    last_ship_date = NA,
                    original_sales_amount = NA,
                    db_refund_amount = NA,
                    estimated_ship_date = date - 35,
                    match_status = "Missing from Database") 
        # Median of 45 days from order to return and 10 days to ship 
        )) %>% 
    rename(esd = estimated_ship_date) %>%
    mutate(payment_processor = ifelse(nchar(response_code) == 17, 
                                      "PayPal", "Pin+Assembly"),
           estimated_ship_date = coalesce(esd, date - 45)) %>%
    select(-esd) %>%
    unique() %>%
    left_join(ordered_units %>%
                  select(order_id, order_number) %>%
                  unique(),
              by = "order_id")

all_refunds %>% write_csv("~/data/returns_reconciled_2017-07-26.csv", na = "")
all_refunds %>%
    mutate(amount_usd = ifelse(currency == "AUD", abs(amount) * 0.74, abs(amount))) %>%
    group_by(ship_year = year(estimated_ship_date),
             ship_quarter = quarter(estimated_ship_date),
             ship_month = month(estimated_ship_date)) %>%
    summarise(adjusted_returns = sum(amount_usd)) %>%
    filter(ship_year >= 2017) %>%
    write_csv("~/code/analytics/ecommerce-performance/static-data/reconciled_returns.csv")
all_refunds %>%
    filter(order_number %in% tpl$order_number) %>%
    mutate(amount_usd = ifelse(currency == "AUD", abs(amount) * 0.74, abs(amount))) %>%
    group_by(ship_year = year(estimated_ship_date),
             ship_month = month(estimated_ship_date)) %>%
    summarise(adjusted_returns = sum(amount_usd)) %>%
    filter(ship_year >= 2017) %>%
    inner_join(products_sold %>%
                   filter(order_number %in% tpl$order_number & order_state != "canceled") %>%
                   group_by(ship_year = year(ship_date), ship_month = month(ship_date)) %>%
                   summarise(gross_revenue_usd = sum(gross_revenue_usd)),
               by = c("ship_year","ship_month")) %>%
    mutate(return_rate = adjusted_returns/gross_revenue_usd) %>%
    write_csv("~/data/refulfilled_reconciled_returns.csv")
