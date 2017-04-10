library(dplyr)
library(lubridate)

source("~/Documents/Code/R/fp_init.R")

monthly_order_nums <- tbl(fp_con, sql("select extract(year from completed_at) order_year, extract(month from completed_at) order_month, order_num, count(distinct email) customers from (select completed_at, email, rank() over (partition by email order by completed_at) order_num from spree_orders where completed_at is not null and payment_state = 'paid' and total > 0) a group by extract(year from completed_at), extract(month from completed_at), order_num")) %>%
    collect()

monthly_order_nums %>%
    mutate(which_order = ifelse(order_num == 1, "order_1",
                                ifelse(order_num == 2, "order_2",
                                       "order_3n"))) %>%
    group_by(order_year, order_month, which_order) %>%
    summarise(customers = sum(customers)) %>%
    spread(which_order, customers, fill = 0) %>%
    mutate(repeat_rate = (order_2 + order_3n) / order_1,
           percent_2 = order_2 / order_1,
           percent_3 = order_3n / order_1) %>%
    filter(order_year > 2015) 

repeat_customer_details <- tbl(fp_con, sql("select completed_at, email, rank() over (partition by email order by completed_at) order_num, total, currency from spree_orders where completed_at is not null and payment_state = 'paid' and total > 0")) %>%
    collect()

# annual repeat rates
repeat_customer_details %>%
    group_by(order_year = year(completed_at), 
             which_order = ifelse(order_num < 3, paste0("order_", order_num), "order_3n")) %>%
    summarise(Customers = n_distinct(email)) %>%
    spread(which_order, Customers, fill = 0) %>%
    mutate(repeat_rate = (order_3n + order_2) / order_1)

cohort_details <- repeat_customer_details %>%
    group_by(email) %>%
    summarise(acquisition_date = completed_at %>% min %>% as.Date) %>%
    inner_join(repeat_customer_details, by = "email") %>%
    mutate(sales_usd = total * ifelse(currency == "AUD", 0.75, 1)) %>%
    filter(year(acquisition_date) >= 2015)
# quarterly cohort analysis
qtr_cohort <- cohort_details %>%
    group_by(acquisition_year_quarter = paste(year(acquisition_date), quarter(acquisition_date), sep = "-Q"), 
             order_year_quarter = paste(year(completed_at), quarter(completed_at), sep = "-Q")) %>%
    summarise(Customers = n_distinct(email),
              Orders = n(),
              Sales = sum(sales_usd))
qtr_cohort %>%
    select(contains("year"), Customers) %>%
    spread(order_year_quarter, Customers, fill = 0)
# monthly cohort analysis
annual_cohort <- cohort_details %>%
    group_by(acquition_year = year(acquisition_date),
             order_year = year(completed_at)) %>%
    summarise(Customers = n_distinct(email),
              Orders = n(),
              Sales = sum(sales_usd))
annual_cohort %>%
    select(contains("year"), Customers) %>%
    spread(order_year, Customers, fill = 0)

