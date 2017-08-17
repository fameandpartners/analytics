source("~/code/analytics/etl/full_global.R")
setwd("~/data")

customer_aquisitions <- products_sold %>%
    filter(is_shipped 
           & order_state != "canceled" 
           & !return_requested) %>%
    group_by(email) %>%
    summarise(date = min(order_date))


write_csv(
    customer_aquisitions, 
    "~/code/analytics/ecommerce-performance/static-data/customer_acquisitions.csv"
)
    