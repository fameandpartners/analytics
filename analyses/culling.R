# Any products that have sold less than 10 net return units in 365 days
# are UNPROFITABLE so they MUST BE CULLED
# NO MERCY!!
source("~/code/analytics/etl/full_global.R")

real_head <- function(x){
    if(length(x) > 0){
        x[[1]]
    } else {
        NA
    }
}

path_to_culling_dropbox <- "/Users/Peter 1/Dropbox (Team Fame)/data/culling/"

active_products <- products %>% 
    filter(!hidden 
           & (is.na(deleted_at) | deleted_at > today())
           & available_on <= today())

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
    summarise(sessions = sum(Sessions),
              bounces = sum(Bounces)) %>%
    mutate(bounce_rate = bounces / sessions)

product_first_sale_dates <- products_sold %>%
    group_by(product_id) %>%
    summarise(first_sale_date = min(order_date))

zero_units <- function(df){
    df %>%
        transmute(product_id, 
                  units_ordered = 0,
                  return_request_units = 0,
                  net_return_request_units = 0)
}

ongoing_cull_styles <- product_first_sale_dates %>%
    filter(first_sale_date < (today() - 120))

ongoing_cull_sales <- products_sold %>%
    inner_join(ongoing_cull_styles, by = "product_id") %>%
    filter(order_date >= (today() - 120) & order_date < (today() - 30))

no_sales_live <- active_products %>%
    anti_join(products_sold, by = "product_id")

ongoing_cull <- ongoing_cull_sales %>%
    group_by(product_id) %>%
    summarise(units_ordered = sum(quantity),
              return_request_units = sum(return_requested),
              net_return_request_units = units_ordered - return_request_units) %>%
    filter(net_return_request_units <= 3) %>%
    bind_rows(list(
        no_sales_live %>%
            filter(available_on < (today() - 120)) %>% # include all styles
            zero_units(),
        ongoing_cull_styles %>%
            anti_join(ongoing_cull_sales, by = "product_id") %>%
            zero_units()
        )) %>%
    left_join(traffic, by = "product_id") %>%
    mutate(stake_holder = ifelse(sessions > 500, "Merchandising", "Marketing")) %>%
    inner_join(products %>%
                   select(product_id, style_number, style_name),
               by = "product_id")  %>%
    semi_join(active_products, by = "product_id") %>%
    unique()

write_csv(ongoing_cull %>% 
              left_join(collections %>% 
                            rename(collection = collection_na), 
                        by = "product_id"), 
          paste0(path_to_culling_dropbox, 
                 "styles/Endangered ", today(), ".csv"),
          na = "")

write_csv(dress_images %>%
              select(product_id, 
                     attachment_width, 
                     attachment_height, 
                     dress_image_url) %>%
              unique() %>%
              semi_join(ongoing_cull, by = "product_id"),
          paste0(path_to_culling_dropbox, 
                 "styles/Endangered Images ", today(), ".csv"),
          na = "")
