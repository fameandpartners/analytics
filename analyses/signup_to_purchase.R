library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

leads <- read_csv("~/data/Contacts-All.csv",
                  col_types = cols(
                      .default = col_character()),
                  skip = 0) %>%
    mutate(CONFIRM_TIME = substr(`Date Created`, 1, 10) %>%
               as.Date(format = "%m/%d/%Y") %>%
               as.POSIXct(),
           signup_date = ifelse(year(CONFIRM_TIME) %in% c(15,16,17),
                                CONFIRM_TIME + years(2000), 
                                CONFIRM_TIME) %>%
               as.POSIXct(origin = "1970-01-01") %>%
               as.Date(),
           email = `Email Address`) %>%
    select(email, signup_date)

dw <- src_postgres(dbname = "dw_dev", host = "localhost")

customer_acquisitions <- tbl(dw, sql("select email, min(order_date) first_order_date from sales group by email")) %>% collect()

out <- leads %>%
    filter(signup_date >= as.Date("2017-03-01")) %>%
    inner_join(customer_acquisitions, by = "email") %>%
    filter(signup_date <= first_order_date) %>%
    mutate(signup_to_purchase = difftime(first_order_date, signup_date, units="days"),
           s2p_bucket = ifelse(signup_to_purchase == 0, "Same Day",
                               ifelse(signup_to_purchase <= 7, "1 - 7 Days",
                                      ifelse(signup_to_purchase <= 14, "8 - 14 Days",
                                             ifelse(signup_to_purchase <= 30, "15 - 30 Days",
                                                    "30+ Days"))))) %>%
    count(s2p_bucket)

out$s2p_bucket <- factor(out$s2p_bucket, 
                         levels = c("Same Day","1 - 7 Days","8 - 14 Days",
                                    "15 - 30 Days","30+ Days"))

out %>% arrange(s2p_bucket) %>% mutate(perc = n/sum(n)) %>% write_csv("~/data/signup_to_purchase_dist_summary.csv")

leads %>%
    filter(signup_date >= as.Date("2017-03-01")) %>%
    inner_join(customer_acquisitions, by = "email") %>%
    filter(signup_date <= first_order_date) %>%
    mutate(signup_to_purchase = difftime(first_order_date, 
                                         signup_date, 
                                         units="days") %>% as.integer()) %>%
    filter(between(signup_to_purchase, 1, 7)) %>%
    ggplot(aes(signup_to_purchase)) +
    geom_histogram(bins = 7) +
    scale_x_continuous(breaks = 1:7) +
    theme_minimal()
