setwd("~/code/analytics/ecommerce-performance/")
source("~/code/analytics/ecommerce-performance/global.R")
setwd("~/data")

products_sold %>%
    filter(is_shipped 
           & order_state != "canceled" 
           & !return_requested) %>%
    group_by(email) %>%
    summarise(date = min(order_date)) %>%
    write_csv("~/code/analytics/ecommerce-performance/static-data/customer_aquisitions.csv")
    