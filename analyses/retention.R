source("~/code/analytics/etl/full_global.R")
setwd("~/data")

unsubs <- read_csv("~/data/unsubscribed_members_export_3ccca61159.csv",
                   col_types = cols(.default = col_character())) %>%
    mutate(unsub_date = UNSUB_TIME %>%
               substr(1, 10) %>%
               as.Date(),
           email = `Email Address`) %>%
    select(email, unsub_date)

products_sold %>%
    group_by(email) %>%
    summarise(acquisition_date = min(order_date)) %>%
    left_join(unsubs, by = "email") %>%
    # There's some weirdness in this chunk of 2016 so I'm filtering it out
    filter(acquisition_date < as.Date("2016-02-15") | acquisition_date > as.Date("2017-07-30")) %>%
    filter(acquisition_date < as.Date("2017-05-01")) %>%
    group_by(acquisition_month = month(acquisition_date)) %>%
    summarise(Customers = n(),
              `Lost Customers` = sum(!is.na(unsub_date)),
              `Avg. Days to Unsub` = median(difftime(unsub_date, acquisition_date,
                                                     units = "days"), na.rm = T)) %>%
    mutate(`Retention Rate` = (Customers - `Lost Customers`) / Customers) %>% 
    select(acquisition_month, `Retention Rate`) %>%
    write_csv("~/data/Retention.csv")