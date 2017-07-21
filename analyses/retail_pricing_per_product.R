source("/Users/Peter 1/code/analytics/ecommerce-performance/fp_init.R")
# For Nancy on 2017-06-01
tbl(fp_con, sql(paste(
    "SELECT DISTINCT",
        "p.id pid,",
        "v2.style_number,",
        "pr.amount retail_price,",
        "pr.currency,",
        "p.available_on::DATE product_launch_date",
    "FROM spree_products p",
    "JOIN spree_variants v",
        "ON v.product_id = p.id",
    "JOIN spree_prices pr",
        "ON pr.variant_id = v.id",
    "JOIN (",
        "SELECT p.id pid, v.sku style_number",
        "FROM spree_products p",
        "JOIN spree_variants v",
            "ON v.product_id = p.id",
        "WHERE v.is_master",
    ") v2 ON v2.pid = p.id",
    "ORDER BY pid DESC"))) %>%
    collect() %>%
    write_csv("~/data/all_product_retail_prices.csv", na = "")