source("~/code/analytics/etl/full_global.R")

real_head <- function(x){
    if(length(x) > 0){
        x[[1]]
    } else {
        NA
    }
}

path_to_culling_dropbox <- "/Users/Peter 1/Dropbox (Team Fame)/data/strength/"

traffic <- paste0(
    path_to_culling_dropbox,
    list.files(path = path_to_culling_dropbox, pattern = "*.csv")) %>%
    lapply(
        read_csv,
        skip = 6,
        n_max = 5000,
        col_types = cols(
            .default = col_number(),
            Page = col_character())) %>%
    bind_rows() %>%
    mutate(product_id = Page %>%
               str_extract_all("dress\\-(.*)\\-[0-9]+") %>%
               str_extract_all("[0-9]+") %>%
               lapply(real_head) %>%
               as.integer()) %>%
    filter(product_id > 0) %>%
    group_by(product_id) %>%
    summarise(Sessions = sum(Sessions))

product_touch_sales <- traffic %>%
    inner_join(products_sold %>%
                   filter(year(order_date) > 2015) %>%
                   group_by(product_id) %>%
                   summarise(Units = sum(quantity),
                             Returns = sum(quantity * return_requested)),
               by = "product_id") %>%
    mutate(eCommConversion = Units / Sessions,
           ReturnRate = coalesce(Returns / Units, 0)) %>%
    filter(Units > 10 & Sessions > Units)

ecomm_conversion_rate <- sum(product_touch_sales$Units) / sum(product_touch_sales$Sessions)

product_touch_sales$eCommConversionPVAL <- product_touch_sales %>%
    select(Units, Sessions) %>%
    apply(1, function(x) {
        binom.test(x[1], x[2], p = ecomm_conversion_rate, "t")$p.val
    })

return_rate <- sum(product_touch_sales$Returns) / sum(product_touch_sales$Units)

product_touch_sales$ReturnRatePVAL <- product_touch_sales %>%
    select(Returns, Units) %>%
    apply(1, function(x) {
        binom.test(x[1], x[2], p = return_rate, "t")$p.val
    })

great_products <- product_touch_sales %>%
    filter(ReturnRatePVAL < 0.1 & ReturnRate < 0.35) %>%
    filter(eCommConversionPVAL < 0.1 & eCommConversion > 0.025) %>%
    inner_join(products)
    