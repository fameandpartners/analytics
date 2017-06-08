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
    filter(!hidden & (is.na(deleted_at) | deleted_at > today())
           & available_on <= today())

no_sales_live <- active_products %>%
    anti_join(products_sold, by = "product_id")

traffic <- paste0(
    path_to_culling_dropbox,
    list.files(path = path_to_culling_dropbox, 
               pattern="*.csv")) %>%
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

first_cull <- products_sold %>%
    inner_join(product_first_sale_dates %>%
                   filter(first_sale_date < (today() - 395)), 
               by = "product_id") %>%
    filter(order_date >= (today() - 395) & order_date < (today() - 30)) %>%
    group_by(product_id) %>%
    summarise(units_ordered = sum(quantity),
              return_request_units = sum(return_requested),
              net_return_request_units = units_ordered - return_request_units) %>%
    filter(net_return_request_units < 10) %>%
    rbind(no_sales_live %>%
              filter(available_on < (today() - 395)) %>%
              transmute(product_id, 
                        units_ordered = 0,
                        return_request_units = 0,
                        net_return_request_units = 0)) %>%
    inner_join(products %>%
                   select(product_id, style_number, style_name),
               by = "product_id") %>%
    semi_join(active_products, by = "product_id")

ongoing_cull <- products_sold %>%
    inner_join(product_first_sale_dates %>%
                   filter(first_sale_date < (today() - 120)), 
               by = "product_id") %>%
    filter(order_date >= (today() - 120) & order_date < (today() - 30)) %>%
    group_by(product_id) %>%
    summarise(units_ordered = sum(quantity),
              return_request_units = sum(return_requested),
              net_return_request_units = units_ordered - return_request_units) %>%
    filter(net_return_request_units < 3) %>%
    anti_join(first_cull, by = "product_id") %>%
    rbind(no_sales_live %>%
              filter(available_on < (today() - 120)) %>%
              transmute(product_id, 
                        units_ordered = 0,
                        return_request_units = 0,
                        net_return_request_units = 0)) %>%
    left_join(traffic, by = "product_id") %>%
    mutate(stake_holder = ifelse(sessions > 500, "Merchandising", "Marketing")) %>%
    inner_join(products %>%
                   select(product_id, style_number, style_name),
               by = "product_id")  %>%
    semi_join(active_products, by = "product_id")


write_csv(first_cull, paste0(path_to_culling_dropbox, 
                             "styles/First Cull 2017-06-07.csv"))
write_csv(dress_images %>%
              select(product_id, 
                     attachment_width, 
                     attachment_height, 
                     dress_image_url) %>%
              unique() %>%
              semi_join(first_cull, by = "product_id"),
          paste0(path_to_culling_dropbox, 
                 "styles/First Cull Images 2017-06-07.csv"))
write_csv(ongoing_cull, paste0(path_to_culling_dropbox, 
                               "styles/Endangered 2017-06-07.csv"))
write_csv(dress_images %>%
              select(product_id, 
                     attachment_width, 
                     attachment_height, 
                     dress_image_url) %>%
              unique() %>%
              semi_join(ongoing_cull, by = "product_id"),
          paste0(path_to_culling_dropbox, 
                 "styles/Endangered Images 2017-06-07.csv"))