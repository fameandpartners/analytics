library(dplyr)
library(readr)

# ALL live during this time: 1st july 2015-31st Dec 2016
source("~/code/analytics/ecommerce-performance/fp_init.R")

old_live_styles <- tbl(fp_con, sql(paste(
    "SELECT",
        "v.sku style_number,",
        "p.name product_name",
    "FROM spree_variants v",
    "INNER JOIN spree_products p",
       "ON v.product_id = p.id",
    "WHERE NOT p.hidden",
        "AND v.is_master",
        "AND (p.deleted_at IS NULL OR p.deleted_at > '2015-07-01')",
        "AND (p.available_on BETWEEN '2015-07-01' AND '2016-12-31')"))) %>%
    collect()

write_csv(old_live_styles, "~/data/Styles Active from 2015-07-01 to 2016-12-31.csv")
