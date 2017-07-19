library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

source("~/code/analytics/ecommerce-performance/fp_init.R")

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

all_refund_transactions <- bind_rows(
    list(
        read_csv(
            "~/data/paypal_aud_jan_may_refunds.csv",
            col_types = cols(
                .default = col_character(),
                Gross = col_number(),
                Fee = col_number(),
                Net = col_number(),
                Balance = col_number())) %>% 
            transmute(response_code = `Transaction ID`, 
                      amount = Gross,
                      date_char = Date,
                      date = dmy(Date),
                      currency = Currency),
        read_csv(
            "~/data/paypal_usd_jan_may_refunds.csv",
            col_types = cols(
                .default = col_character(),
                Gross = col_number(),
                Fee = col_number(),
                Net = col_number(),
                Balance = col_number())) %>% 
            transmute(response_code = `Transaction ID`, 
                      amount = Gross, 
                      date_char = Date,
                      date = mdy(Date),
                      currency = Currency),
        read_csv(
            "~/data/pin_jan_may_refunds.csv",
            col_types = cols(
                .default = col_character(),
                Amount = col_number())) %>% 
            transmute(response_code = Reference, 
                      amount = Amount, 
                      date_char = Date,
                      date = Date %>%
                          substr(1, 10) %>%
                          dmy(),
                      currency = Currency),
        read_csv(
            "~/data/assembly_payments_jan_to_april_old_format.csv",
            col_types = cols(
                .default = col_character(),
                `Purchase Amt` = col_number())) %>% 
            transmute(response_code = `Merchant Order #`, 
                      amount = `Purchase Amt`,
                      date_char = `Batch Post Day`,
                      date = mdy(`Batch Post Day`),
                      currency = `Purchase Currency`),
        read_csv(
            "~/data/assembly_payments_may_new_format.csv",
            col_types = cols(
                .default = col_character(),
                `Purchase Amt` = col_number())) %>%
            separate(`Activity Date`, into = c("m","d","y"), 
                     sep = "/", remove = FALSE) %>%
            mutate(fixed_m = ifelse(as.integer(m) >= 6, d, m),
                   fixed_d = ifelse(as.integer(m) >= 6, m, d),
                   clean_date = paste(ifelse(as.integer(y) == 17, 2017, y), 
                                      formatC(as.integer(fixed_m), width = 2, flag = "0"), 
                                      formatC(as.integer(fixed_d), width = 2, flag = "0"), 
                                      sep = "-")) %>%
            transmute(response_code = `Merchant Transaction ID`,
                      amount = `Purchase Amt`,
                      date_char = `Activity Date`,
                      date = date(clean_date),
                      currency = `Purchase Currency`),
        read_csv(
            "~/data/Assembly Payments refunds_June 2017.csv",
            col_types = cols(
                .default = col_character(),
                `Purchase Amt` = col_number())) %>%
            transmute(response_code = `Merchant Transaction ID`,
                      amount = `Purchase Amt`,
                      date_char = `Activity Date`,
                      date = mdy(`Activity Date`),
                      currency = `Purchase Currency`)
    )
)

payment_lkp$response_code_source <- factor(
    payment_lkp$response_code_source,
    levels = c("item_returns","refund_requests",
               "spree_payments","spree_payments_paypal")
)

orders <- products_sold %>%
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
             inner_join(payment_lkp, by = "response_code") %>%
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
    mutate(payment_processor = ifelse(
        nchar(response_code) == 17, "PayPal", "Pin+Assembly")) 

all_refunds %>% write_csv("~/data/returns_reconciled_2017-07-18.csv", na = "")
    