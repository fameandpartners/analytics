# Reconcile Returns Based on Payment Data from Pin and PayPal
# Calculate Return Rate including any missing returns from Pin / PayPal

setwd("~/Documents/Data")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
}

refunds_pin <- read_csv(
    "pin refunds.csv",
    col_types = cols(`Refund Date` = col_date("%m/%d/%y"))) %>%
    mutate(id = row_number(),
           pin_amount_usd = ifelse(Currency == "AUD", `Pin Amount` * 0.74, `Pin Amount`))

refunds_paypal <- read_csv(
    "paypal refunds.csv",
    col_types = cols(date = col_date("%m/%d/%y"))) %>%
    mutate(amount_usd = ifelse(str_detect(account, "USD"), amount / 0.74, amount),
           Currency = substrRight(account, 3),
           id = row_number())

ben_ship_dates <- read_csv(
    "ben ship dates.csv",
    col_types = cols(
        order_date = col_date("%m/%d/%y"),
        ship_date = col_date("%m/%d/%y"))) %>%
    select(order_id, ship_date) %>%
    rename(b_ship_date = ship_date)

source("~/Documents/Code/R/fp_init.R")

payment_lkp <-
    tbl(fp_con, sql(paste(
        "SELECT DISTINCT * FROM (",
            "SELECT order_id, response_code",
            "FROM spree_payments",
            "WHERE response_code IS NOT NULL",
            "AND source_type = 'Spree::CreditCard'",
            "UNION",
            "SELECT p.order_id, pp.transaction_id",
            "FROM spree_payments p",
            "INNER JOIN spree_paypal_express_checkouts pp",
            "ON p.source_id = pp.id",
            "WHERE pp.transaction_id IS NOT NULL",
            "AND p.source_type = 'Spree::PaypalExpressCheckout' ",
            "UNION",
            "SELECT li.order_id, ir.refund_ref",
            "FROM item_returns ir",
            "INNER JOIN spree_line_items li",
            "ON ir.line_item_id = li.id",
            "WHERE ir.refund_ref IS NOT NULL",
            "UNION",
            "SELECT order_id, refund_ref",
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
    collect() %>%
    left_join(ben_ship_dates, by = "order_id") %>%
    rename(ship_date_del = ship_date) %>%
    mutate(has_shipping_data = !is.na(ship_date_del),
           ship_date_na = as.Date(ifelse(is.na(ship_date_del), b_ship_date, ship_date_del), origin = "1970-01-01"),
           ship_date = as.Date(ifelse(is.na(ship_date_na), order_date + 10, ship_date_na), origin = "1970-01-01")) %>%
    select(-ship_date_del, -b_ship_date, -ship_date_na)

pin_match1 <- refunds_pin %>%
    mutate(ref = substr(Reference, 1, 25)) %>%
    inner_join(payment_lkp %>% transmute(order_id, ref = response_code), by = "ref")

pin_match2 <- refunds_pin %>%
    filter(year(`Refund Date`) >= 2017) %>%
    anti_join(pin_match1, by = "id") %>%
    mutate(ref = substr(Reference, 1, 25)) %>%
    inner_join(payment_lkp %>% transmute(order_id, ref = substr(response_code, 1, 24)), by = "ref")

pin_matched_payments <- rbind(pin_match1, pin_match2)

paypal_matched_payments <- 
    refunds_paypal %>% 
    inner_join(payment_lkp, by = c("transaction_id" = "response_code"))

order_level_matched_refunds <- rbind(
    pin_matched_payments %>%
        group_by(order_id) %>%
        summarise(reference_codes = paste(ref, collapse = " | "),
                  Currency = paste(unique(Currency), collapse = ","),
                  amount_usd = sum(pin_amount_usd, na.rm = TRUE),
                  refund_date = max(`Refund Date`)) %>%
        mutate(payment_processor = "Pin"),
    paypal_matched_payments %>%
        group_by(order_id) %>%
        summarise(reference_codes = paste(reference, collapse = " | "),
                  Currency = paste(unique(Currency), collapse = ","),
                  amount_usd = sum(amount_usd),
                  refund_date = max(date)) %>%
        mutate(payment_processor = "PayPal")
)

payments_not_matched <- rbind(
    refunds_pin %>%
        anti_join(pin_matched_payments, by = "id") %>%
        select(`Refund Date`, `Amount (USD)` = pin_amount_usd, Reference, Currency) %>%
        mutate(`Payment Processor` = "Pin"),
    refunds_paypal %>%
        anti_join(paypal_matched_payments, by = "id") %>%
        select(`Refund Date` = date, `Amount (USD)` = amount_usd, Reference = reference, Currency) %>%
        mutate(`Payment Processor` = "PayPal")) %>%
    mutate(`Ship Date` = `Refund Date` - 30) %>%
    select(`Ship Date`, `Amount (USD)`, `Payment Processor`, Reference, Currency)

matched_refunds <-
    order_level_matched_refunds %>%
    inner_join(order_returns, by = "order_id")

all_refunds <- rbind(
    payments_not_matched %>% 
        mutate(Category = "Return",
               Matched_in_Spree = FALSE),
    matched_refunds %>% 
        mutate(Category = ifelse(order_status == "canceled", "Cancel", "Return")) %>%
        select(`Ship Date` = ship_date, 
               `Amount (USD)` = amount_usd, 
               `Payment Processor` = payment_processor,
               Currency,
               Category,
               Reference = reference_codes) %>%
        mutate(Matched_in_Spree = TRUE)
)

# weird stuff:

# when the refund amount is not missing
# 61% of refunds match the amount
# 10% of refunds match with bank amount * 2 - because of typo
# 30% don't match for an unknown reason

ref_diffs <- matched_refunds %>% 
    mutate(ref_diff = round(refund_amount_usd - amount_usd), 
           ref_diff_2x = round(refund_amount_usd - amount_usd * 2),
           ref_diff_compx = round( (refund_amount_usd / items) - amount_usd))
ref_diffs %>% 
    filter(!is.na(refund_amount_usd)) %>%
    count(ref_diff == 0) %>%
    mutate(perc_of_total = n/sum(n))
ref_diffs %>% 
    filter(!is.na(refund_amount_usd)) %>%
    count(ref_diff_2x == 0) %>%
    mutate(perc_of_total = n/sum(n))
ref_diffs %>% 
    filter(!is.na(refund_amount_usd)) %>%
    count(ref_diff_compx == 0 | ref_diff == 0) %>%
    mutate(perc_of_total = n/sum(n))

# of the returns from january only 42% of them were from orders shipped in december
matched_refunds %>% 
    filter(year(refund_date) == 2017 & month(refund_date) == 01) %>% 
    group_by(year(ship_date), month(ship_date)) %>% 
    summarise(refunds = n(), revenue = scales::dollar(sum(amount_usd)))

refund_cadence <-
    matched_refunds %>%
    filter(has_shipping_data) %>%
    mutate(ship_to_refund = refund_date - ship_date)

quantile(refund_cadence$ship_to_refund, seq(0, 1, 0.1))
