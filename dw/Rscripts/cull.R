# Any products that have sold less than 10 net return units in 365 days
# are UNPROFITABLE so they MUST BE CULLED
# NO MERCY!!
suppressMessages(library(readr))
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(tidyr))
suppressMessages(library(lubridate))
suppressMessages(library(stringr))
suppressMessages(library(feather))

dw <- src_postgres(
  host = Sys.getenv("RDS_HOST"),
  dbname = "dw",
  user = Sys.getenv("RDS_USER"),
  password = Sys.getenv("RDS_PASS")
)
collect_dw <- function(conn) conn %>% select(-id, -created_at) %>% collect()
products_sold <- tbl(dw, "sales") %>% collect_dw()
products <- tbl(dw, "products") %>% collect_dw()

real_head <- function(x){
    if(length(x) > 0){
        x[[1]]
    } else {
        NA
    }
}

FEATHERS <- "feathers/"

active_products <- products %>% filter(live)

# ga_cull_traffic*.csv generated with dw/google_apps/cull.py
traffic <- suppressWarnings(read_feather(paste0(FEATHERS,"ga_cull_traffic.feather"))) %>%
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
    inner_join(products %>% select(product_id, available_on), by = "product_id") %>%
    filter((first_sale_date < (today() - 120)) | (available_on < (today() - 120))) %>%
    transmute(product_id, first_sale_date = coalesce(first_sale_date, as.Date(available_on)))

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

write_csv(ongoing_cull, "Rscripts/static-data/ongoing_cull.csv")
