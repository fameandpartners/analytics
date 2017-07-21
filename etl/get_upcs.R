library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

source("~/code/analytics/ecommerce-performance/fp_init.R")

order_upcs <- tbl(fp_con, sql(paste(
    "SELECT DISTINCT o.id order_id, o.number order_number, g.id upc",
    "FROM spree_line_items li",
    "INNER JOIN spree_orders o",
        "ON o.id = li.order_id",
    "INNER JOIN spree_variants v",
        "ON v.id = li.variant_id",
    "INNER JOIN global_skus g",
        "ON g.variant_id = v.id",
    "WHERE o.completed_at IS NOT NULL"))) %>%
    collect(n = Inf)

bergen_orders <- read_csv("~/data/orders_needing_upcs.csv")

bergen_orders %>%
    inner_join(order_asns, by = "order_number")

setwd("~/code/analytics/ecommerce-performance")
source("~/code/analytics/ecommerce-performance/global.R")
setwd("~/data")

bergen_orders %>%
    inner_join(products_sold, by = "order_number") %>%
    transmute(line_item_id,
              order_id,
              order_number,
              style_number,
              order_date,
              ship_date,
              sales_usd,
              currency,
              return_requested,
              return_request_date = as.Date(requested_at),
              refund_date = as.Date(refunded_at),
              refund_amount = refund_amount / 100,
              return_comments) %>% 
    write_csv("~/data/Bergen Orders with Return Data.csv", na = "")
