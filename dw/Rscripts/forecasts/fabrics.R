library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(rpart)
library(feather)

# Need to point this at the sales table in the DW eventually
source("~/code/analytics/etl/full_global.R")
fabric_usage <- read_csv("~/data/fabric_usage.csv")

produced_sales <- products_sold %>%
    filter(order_status != "Canceled" & year(order_date) >= 2016)

annual_fabrics <- produced_sales %>%
    group_by(order_year = year(order_date), product_id) %>%
    summarise(units_sold = sum(quantity)) %>%
    inner_join(fabric_usage, by = "product_id") %>%
    mutate(meters = usage*units_sold) %>%
    group_by(order_year, fabric) %>%
    summarise(meters = sum(meters)) %>%
    ungroup()

monthly_seasonality <- products_sold %>%
    group_by(order_year = year(order_date), 
             order_month = month(order_date)) %>%
    summarise(units_sold = sum(quantity)) %>%
    filter(order_year >= 2015) %>%
    ungroup() %>%
    mutate(overall_mean = mean(units_sold)) %>%
    spread(order_year, units_sold) %>%
    mutate(period_mean = ifelse(is.na(`2017`),
                                (`2015`+`2016`)/2,
                                (`2015`+`2016`+`2017`)/3),
           deseason = period_mean / overall_mean) %>%
    select(order_month, deseason)

monthly_fabrics <- produced_sales %>%
    group_by(order_year = year(order_date), 
             order_month = month(order_date),
             product_id) %>%
    summarise(units_sold = sum(quantity)) %>%
    inner_join(fabric_usage, by = "product_id") %>%
    mutate(meters = usage*units_sold) %>%
    group_by(order_year, order_month, fabric) %>%
    summarise(meters = sum(meters)) %>%
    semi_join(annual_fabrics %>% filter(meters > 500), by = "fabric") %>%
    filter(ifelse(order_year == year(today()),
                  order_month < month(today()), TRUE)) %>%
    inner_join(monthly_seasonality, by = "order_month") %>%
    group_by(fabric) %>%
    arrange(fabric, order_year, order_month) %>%
    mutate(prior_meters = lag(meters)) %>%
    arrange(fabric, order_month, order_year) %>%
    mutate(prior_yr_meters = lag(meters)) %>%
    filter(!is.na(prior_meters) & !is.na(prior_yr_meters)) %>%
    ungroup()

weekly_seasonality <- products_sold %>%
    group_by(order_year = year(order_date), 
             order_week = week(order_date)) %>%
    summarise(units_sold = sum(quantity)) %>%
    filter(order_year >= 2015) %>%
    ungroup() %>%
    mutate(overall_mean = mean(units_sold)) %>%
    spread(order_year, units_sold) %>%
    mutate(period_mean = ifelse(is.na(`2017`),
                                (`2015`+`2016`)/2,
                                (`2015`+`2016`+`2017`)/3),
           deseason = period_mean / overall_mean) %>%
    select(order_week, deseason)

weekly_fabrics <- produced_sales %>%
    filter(!(year(order_date) == year(today()) & week(order_date) == week(today()))) %>%
    group_by(order_year = year(order_date), 
             order_quarter = quarter(order_date),
             order_week = week(order_date),
             product_id) %>%
    summarise(units_sold = sum(quantity)) %>%
    inner_join(fabric_usage, by = "product_id") %>%
    mutate(meters = usage*units_sold) %>%
    group_by(order_year, order_week, fabric) %>%
    summarise(meters = sum(meters)) %>%
    semi_join(annual_fabrics %>% filter(meters > 500), by = "fabric") %>%
    left_join(weekly_seasonality, by = "order_week") %>%
    group_by(fabric) %>%
    arrange(fabric, order_year, order_week) %>%
    mutate(prior_meters = lag(meters)) %>%
    arrange(fabric, order_week, order_year) %>%
    mutate(prior_yr_meters = lag(meters)) %>%
    filter(!is.na(prior_meters) & !is.na(prior_yr_meters)) %>%
    ungroup()

# Short-term models
# weekly_st_lm <- lm(meters ~ (prior_meters * prior_yr_meters) / deseason, weekly_fabrics)
# Perhaps V2 could use this recursively to forecast long-term
monthly_st_lm <- lm(meters ~ (prior_meters * prior_yr_meters) / deseason , data = monthly_fabrics)
# Long-term model 
weekly_lt_lm <- lm(meters ~ prior_yr_meters / deseason, weekly_fabrics)

the_future <- data_frame(order_date = seq(today(), today() + 364, 1)) %>%
    transmute(order_year = year(order_date),
              order_month = month(order_date),
              order_week = week(order_date)) %>%
    unique() 

last_month <- today() %m+% months(-1)
this_month <- monthly_fabrics %>%
    filter(order_year == year(last_month) & order_month == month(last_month)) %>%
    transmute(order_year, order_month = 10, fabric, prior_meters = meters) %>%
    inner_join(monthly_fabrics %>%
                   transmute(order_year = order_year + 1, order_month, fabric, 
                             prior_yr_meters = meters),
               by = c("order_year","order_month","fabric")) %>%
    inner_join(monthly_seasonality, by = "order_month")
    
this_month_forecasts <- predict(
    monthly_st_lm, newdata = october,
    interval = "confidence", level = 0.75) %>%
    cbind(october) %>%
    as_data_frame() %>%
    filter(lwr > 0)

weekly_future <- the_future %>%
    filter(!(order_year == year(today()) & order_month == month(today()))) %>%
    inner_join(weekly_fabrics %>%
                   select(order_year, order_week, fabric, meters) %>%
                   mutate(next_year = order_year + 1) %>%
                   select(-order_year) %>%
                   rename(order_year = next_year, 
                          prior_yr_meters = meters),
               by = c("order_year","order_week")) %>%
    inner_join(weekly_seasonality, by = "order_week")

weekly_predictions <- predict(
    weekly_lt_lm, newdata = weekly_future,
    interval = "confidence", level = 0.99) %>%
    cbind(weekly_future) %>%
    as_data_frame() %>%
    filter(lwr > 0)

# For DW ETL pipeline
write_feather(fabric_usage, "feathers/fabric_usage.feather")
write_feather(weekly_predictions, "feathers/weekly_fabric_forecasts.feather")
write_feather(this_month, "feathers/this_month_fabric_forecasts.feather")
