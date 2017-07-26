library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

source("~/code/analytics/ecommerce-performance/fp_init.R")

bergen_orders <- read_csv("~/data/orders_needing_upcs.csv") %>%
    filter(order_number %>% str_detect("^[R|r|M|E]")
           & !str_detect(order_number, " |\\/")) %>%
    mutate(order_number_lookup = paste0("'", order_number, "'"))

order_upcs <- tbl(fp_con, sql(paste(
    "SELECT DISTINCT o.id order_id, o.number order_number, g.id upc",
    "FROM spree_line_items li",
    "INNER JOIN spree_orders o",
        "ON o.id = li.order_id",
    "INNER JOIN spree_variants v",
        "ON v.id = li.variant_id",
    "INNER JOIN line_item_personalizations lip",
        "ON lip.line_item_id = li.id",
    "INNER JOIN global_skus g",
        "ON g.product_id = v.product_id",
        "AND g.color_id = lip.color_id::VARCHAR",
        "AND g.size = lip.size",
        "AND (lip.customization_value_ids ~ g.customisation_id",
            "OR g.customisation_id IS NULL)",
    "WHERE o.completed_at IS NOT NULL",
        "AND v.deleted_at IS NULL",
        "AND o.number IN (", paste(
            bergen_orders$order_number_lookup,
            collapse = ","
        ), ")"))) %>%
    collect(n = Inf)

global_skus <- tbl(fp_con, "global_skus") %>% collect()

bergen_orders %>%
    select(-order_number_lookup) %>%
    inner_join(order_upcs, by = "order_number")

setwd("~/code/analytics/ecommerce-performance")
source("~/code/analytics/ecommerce-performance/global.R")
setwd("~/data")

bergen_sales <- bergen_orders %>%
    select(-order_number_lookup) %>%
    inner_join(products_sold, by = "order_number")

unique_upc_lines <- bergen_sales %>%
    inner_join(order_upcs, by = "order_id") %>%
    group_by(line_item_id) %>%
    summarise(upcs = n_distinct(upc)) %>%
    filter(upcs == 1)

unique_upcs <- bergen_sales %>%
    inner_join(order_upcs, by = "order_id") %>%
    group_by(upc) %>%
    summarise(styles = n_distinct(style_number)) %>%
    filter(styles == 1)

bergen_returns %>%
    semi_join(unique_upc_lines, by = "line_item_id") %>%
    inner_join(order_upcs %>% select(-order_number), by = "order_id") %>%
    semi_join(unique_upcs, by = "upc") %>%
    write_csv("~/data/bergen_orders_with_upcs.csv", na = "")
