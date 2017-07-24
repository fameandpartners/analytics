# Pull style number and product name
tbl(fp_con, sql(paste(
    "SELECT DISTINCT v.sku style_number, p.name style_name",
    "FROM spree_assets a",
    "INNER JOIN product_color_values pcv",
        "ON a.viewable_id = pcv.id",
    "INNER JOIN spree_products p",
        "ON p.id = pcv.product_id",
    "INNER JOIN spree_variants v",
        "ON v.product_id = pcv.product_id",
    "WHERE a.viewable_type = 'ProductColorValue'",
        "AND a.attachment_file_name ilike '%-6%'",
        "AND v.is_master",
        "AND p.available_on BETWEEN '2017-01-01' AND CURRENT_DATE"))) %>%
    collect() %>%
    write_csv("~/data/flat_lay_styles.csv")