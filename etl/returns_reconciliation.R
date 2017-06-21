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

order_returns <-
    tbl(fp_con, sql(paste(
        "SELECT",
            "li.order_id,",
            "STRING_AGG(DISTINCT o.number, ',') order_number,",
            "STRING_AGG(DISTINCT o.state, ',') order_status,",
            "MAX(o.completed_at::DATE) order_date,",
            "MAX(s.shipped_at::DATE) ship_date,",
            "COUNT(DISTINCT li.id) items,",
            "SUM(CASE WHEN li.currency = 'AUD' ",
                    "THEN li.price * li.quantity * 0.74",
                    "ELSE li.price * li.quantity",
                "END) original_amount_usd,",
            "SUM(CASE WHEN ir.id IS NOT NULL",
                    "THEN CASE WHEN ir.order_paid_currency = 'AUD' ",
                        "THEN ir.refund_amount * 0.74",
                        "ELSE ir.refund_amount END",
                    "ELSE CASE WHEN rr.refund_currency = 'AUD' ",
                        "THEN rr.refund_amount * 0.74",
                        "ELSE rr.refund_amount END",
                "END) / 100 refund_amount_usd,",
            "STRING_AGG(DISTINCT ir.requested_action, ',') requested_action,",
            "STRING_AGG(DISTINCT",
                "CASE WHEN ir.id IS NOT NULL",
                    "THEN ir.acceptance_status",
                    "ELSE rr.acceptance_status",
                "END, ',') acceptance_status, ",
            "COUNT(DISTINCT ir.id) item_returns",
        "FROM spree_line_items li",
        "INNER JOIN spree_orders o",
            "ON o.id = li.order_id",
        "LEFT JOIN item_returns ir",
            "ON ir.line_item_id = li.id",
        "LEFT JOIN refund_requests rr",
            "ON rr.order_id = li.order_id",
        "LEFT JOIN spree_shipments s",
            "ON s.order_id = o.id",
        "WHERE o.completed_at IS NOT NULL",
        "GROUP BY li.order_id"))) %>%
    collect()

pay_pal_aud <- read_csv(
    "~/data/paypal_aud_jan_may_refunds.csv",
    col_types = cols(
        .default = col_character(),
        Gross = col_number(),
        Fee = col_number(),
        Net = col_number(),
        Balance = col_number())
)
pay_pal_usd <- read_csv(
    "~/data/paypal_usd_jan_may_refunds.csv",
    col_types = cols(
        .default = col_character(),
        Gross = col_number(),
        Fee = col_number(),
        Net = col_number(),
        Balance = col_number())
)
pin <- read_csv(
    "~/data/pin_jan_may_refunds.csv",
    col_types = cols(
        .default = col_character(),
        Amount = col_number()
    )
)
assembly_old <- read_csv(
    "~/data/assembly_payments_jan_to_april_old_format.csv",
    col_types = cols(
        .default = col_character(),
        `Purchase Amt` = col_number()
    )
)
assembly_new <- read_csv(
    "~/data/assembly_payments_may_new_format.csv",
    col_types = cols(
        .default = col_character(),
        `Purchase Amt` = col_number()
    )
)

all_refund_transactions <- bind_rows(
    list(
        pay_pal_aud %>% 
            transmute(response_code = `Transaction ID`, 
                      amount = Gross, 
                      date = Date),
        pay_pal_usd %>% 
            transmute(response_code = `Transaction ID`, 
                      amount = Gross, 
                      date = Date),
        pin %>% 
            transmute(response_code = Reference, 
                      amount = -Amount, 
                      date = Date),
        assembly_old %>% 
            transmute(response_code = `Merchant Order #`, 
                      amount = `Purchase Amt`, 
                      date = `Batch Post Day`),
        assembly_new %>%
            transmute(response_code = `Merchant Order Number`,
                      amount = `Purchase Amt`,
                      date = `Activity Date`)
    )
)

all_refund_transactions %>%
    anti_join(payment_lkp, by = "response_code")