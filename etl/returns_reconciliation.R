library(readr)
library(dplyr)
library(tidyr)
library(stringr)

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

setwd("~/code/analytics/ecommerce-performance/")
source("~/code/analytics/ecommerce-performance/global.R")
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
                      date = dmy(Date)),
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
                      date = mdy(Date)),
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
                          dmy()),
        read_csv(
            "~/data/assembly_payments_jan_to_april_old_format.csv",
            col_types = cols(
                .default = col_character(),
                `Purchase Amt` = col_number())) %>% 
            transmute(response_code = `Merchant Order #`, 
                      amount = `Purchase Amt`,
                      date_char = `Batch Post Day`,
                      date = mdy(`Batch Post Day`)),
        read_csv(
            "~/data/assembly_payments_may_new_format.csv",
            col_types = cols(
                .default = col_character(),
                `Purchase Amt` = col_number())) %>%
            transmute(response_code = `Merchant Order Number`,
                      amount = `Purchase Amt`,
                      date_char = `Activity Date`,
                      date = mdy(`Activity Date`))
    )
)

payment_lkp$response_code_source <- factor(
    payment_lkp$response_code_source,
    levels = c("item_returns","refund_requests","spree_payments","spree_payments_paypal")
)

matched_refunds <- all_refund_transactions %>%
    inner_join(payment_lkp, by = "response_code") %>%
    arrange(response_code_source) %>%
    filter(!duplicated(paste(response_code, order_id))) %>%
    inner_join(products_sold %>%
                   group_by(order_id) %>%
                   summarise(refund_requested = max(return_requested),
                             refund_processed = max(item_returned),
                             last_order_date = max(order_date),
                             last_ship_date = max(ship_date),
                             original_sales_amount = sum(sales_usd),
                             refund_amount = sum(refund_amount) / 100) %>%
                   mutate(estimated_ship_date = coalesce(last_ship_date, 
                                                         last_order_date + 10)), 
               by = "order_id")

missing_refunds <- all_refund_transactions %>%
    anti_join(matched_refunds, by = "response_code")
    