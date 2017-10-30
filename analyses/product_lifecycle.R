library(tidyr)
library(dplyr)
library(dbplyr)
library(stringr)
library(lubridate)
library(readr)
library(ggplot2)

# ---- FUNCTIONS ----
year_month <- function(date_value){
    paste(year(date_value),
          formatC(month(date_value), width = 2, flag = "0"),
          sep = "-")
}

dollar <- function(number){
    paste0("$", round(number, digits = 2) %>%
               format(nsmall = 2, big.mark = ","))
}

percent <- function(number){
    paste0(format(100 * round(number, 2), big.mark = ","), "%")
}

short_dollar <- function(number){
    ifelse(number < 1000, paste0("$", round(number)),
           ifelse(number < 1000000, paste0("$", round(number / 1000, 1), "K"),
                  ifelse(number < 1000000000, paste0("$", round(number / 1000000, 1), "M"),
                         paste0("$", round(number / 1000000000, 1), "B"))))
}

short_number <- function(number){
    ifelse(number < 1000, round(number) %>% as.integer(),
           ifelse(number < 1000000, paste0(round(number / 1000, 1), "K"),
                  ifelse(number < 1000000000, paste0((round(number / 1000000, 1)), "M"),
                         paste0(round(number / 1000000000, 1), "B"))))
}

h3c <- function(x) h3(x, align = "center")

dress_image_url <- function(asset_id, attachment_file_name){
    paste0("https://d1msb7dh8kb0o9.cloudfront.net/spree/products/",
           asset_id,
           "/original/",
           attachment_file_name)
}

sql_convert_to_LA_time <- function(utc_time){
    paste0("(", utc_time, " at time zone 'UTC') at time zone 'America/Los_Angeles'")
}

# query conversion rates
aud_to_usd <- 0.74  # query_aud_to_usd()

# ---- QUERIES ----

dw <- src_postgres(dbname = "dw_dev", host = "localhost")

products_sold <- tbl(dw, "sales") %>% collect()

traffic <- read_csv("~/data/product traffic data.csv") %>%
    inner_join(products_sold %>%
                   select(product_id, style_number) %>%
                   unique(),
               by = "product_id")

# ---- YoY Weekly Sales ----
weekly_sales <- products_sold %>%
    filter(order_date >= as.Date("2015-01-01")
           & order_date <= as.Date("2017-10-21") 
           & payment_state == "paid") %>%
    group_by(order_year = year(order_date) %>% as.character(), 
             order_week = week(order_date)) %>%
    summarise(Units = sum(quantity),
              `Net Sales` = sum(sales_usd))
weekly_sales %>%
    ggplot(aes(x = order_week)) +
    geom_path(aes(y = Units, color = order_year), group = 4) +
    geom_point(aes(y = Units, color = order_year)) +
    scale_x_continuous(limits = c(1, 52)) +
    scale_y_continuous(labels = short_number) +
    xlab("Order Week")

weekly_sales %>%
    ggplot(aes(x = order_week)) +
    geom_path(aes(y = `Net Sales`, color = order_year), group = 4) +
    geom_point(aes(y = `Net Sales`, color = order_year)) +
    scale_x_continuous(limits = c(1, 52)) +
    scale_y_continuous(labels = short_dollar) +
    xlab("Order Week")

# by Country
products_sold %>%
    filter(ship_country %in% c("United States","Australia")
           & order_date >= as.Date("2015-01-01")
           & order_date <= as.Date("2017-10-21")  
           & payment_state == "paid") %>%
    group_by(order_year = year(order_date) %>% as.character(), 
             order_week = week(order_date),
             ship_country) %>%
    summarise(`Net Sales` = sum(sales_usd)) %>%
    ggplot(aes(x = order_week)) +
    geom_path(aes(y = `Net Sales`, color = order_year), group = 4) +
    geom_point(aes(y = `Net Sales`, color = order_year)) +
    facet_grid(ship_country~.) +
    scale_x_continuous(limits = c(1, 52)) +
    scale_y_continuous(labels = short_dollar) +
    xlab("Order Week")

# ---- YoY Percent Change ----
weekly_sales %>%
    select(-Units) %>%
    group_by(order_week) %>%
    spread(order_year, `Net Sales`) %>%
    mutate(`2015 to 2016` = (`2016` - `2015`) / `2015`,
           `2016 to 2017` = (`2017` - `2016`) / `2016`) %>%
    select(order_week, `2015 to 2016`, `2016 to 2017`) %>%
    gather(years_changed, percent_change, -order_week) %>%
    #filter(years_changed %>% str_detect("2017")) %>%
    ggplot(aes(x = order_week, y = percent_change, color = years_changed)) +
    geom_path() + geom_point() +
    scale_y_continuous(labels = percent, limits = c(-0.3,3)) +
    xlab("Order Week") + ylab("Percent Change") +
    theme(legend.title = element_blank())

# by Country
products_sold %>%
    filter(ship_country %in% c("United States","Australia")
           & order_date >= as.Date("2015-01-01")
           & order_date <= as.Date("2017-09-02") 
           & payment_state == "paid") %>%
    group_by(order_year = year(order_date) %>% as.character(), 
             order_week = week(order_date),
             ship_country) %>%
    summarise(`Net Sales` = sum(sales_usd)) %>%
    group_by(order_week, ship_country) %>%
    spread(order_year, `Net Sales`) %>%
    mutate(`2015 to 2016` = (`2016` - `2015`) / `2015`,
           `2016 to 2017` = (`2017` - `2016`) / `2016`) %>%
    select(-`2015`,-`2016`,-`2017`) %>%
    gather(years_changed, percent_change, -order_week, -ship_country) %>%
    ggplot(aes(x = order_week, y = percent_change, color = years_changed)) +
    geom_path() + geom_point() +
    facet_grid(ship_country~.) +
    scale_y_continuous(labels = percent, limits = c(-0.3,3)) +
    xlab("Order Week") + ylab("Percent Change") +
    theme(legend.title = element_blank())

# ---- YoY Weekly Shipments ----
weekly_shipments <- products_sold %>%
    filter(ship_date <= as.Date("2017-06-10") 
           & payment_state == "paid"
           & year(ship_date) > 2015) %>%
    group_by(ship_year = year(ship_date) %>% as.character(), 
             ship_week = week(ship_date)) %>%
    summarise(Units = sum(quantity),
              `Gross Revenue` = sum(gross_revenue_usd))
weekly_shipments %>%
    ggplot(aes(x = ship_week)) +
    geom_path(aes(y = Units, color = ship_year), group = 4) +
    geom_point(aes(y = Units, color = ship_year)) +
    scale_x_continuous(limits = c(1, 52)) +
    scale_y_continuous(labels = short_number) +
    xlab("Ship Week")

weekly_shipments %>%
    ggplot(aes(x = ship_week)) +
    geom_path(aes(y = `Gross Revenue`, color = ship_year), group = 4) +
    geom_point(aes(y = `Gross Revenue`, color = ship_year)) +
    scale_x_continuous(limits = c(1, 52)) +
    scale_y_continuous(labels = short_dollar) +
    xlab("Ship Week")

# ---- YoY Cumulative Sales ----
products_sold %>%
    group_by(order_year = year(order_date) %>% as.character(), 
             order_week = week(order_date)) %>%
    summarise(Units = sum(quantity)) %>%
    mutate(cumulative_units = cumsum(Units)) %>%
    ggplot(aes(x = order_week, y = cumulative_units, color = order_year)) +
    geom_line(group = 3) +
    scale_x_continuous(limits = c(1, 52)) +
    scale_y_continuous(labels = short_number, breaks = seq(1, 20000, 5000))

# ---- Summary Statistics for Product Quarterly Performance ----
product_first_sale_dates <- products_sold %>%
    group_by(product_id) %>%
    summarise(first_sale_date = min(order_date))

quarter_dates <- data_frame(
    date_value = seq(as.Date("2016-01-01"), as.Date("2017-12-31"), 1)) %>%
    group_by(year_quarter_value = paste(year(date_value), quarter(date_value))) %>%
    summarise(start_date = min(date_value))

product_rankings_per_quarter <- products_sold %>%
    filter(order_date < today() - 30 & order_date >= as.Date("2016-01-01")) %>%
    inner_join(product_first_sale_dates, by = "product_id") %>%
    group_by(style_number, 
             order_year_quarter = paste(year(order_date), quarter(order_date))) %>%
    inner_join(quarter_dates, by = c("order_year_quarter" = "year_quarter_value")) %>%
    filter(first_sale_date <= start_date) %>%
    summarise(units_ordered = sum(quantity),
              return_request_units = sum(return_requested),
              net_return_request_units = units_ordered - return_request_units) %>%
    group_by(order_year_quarter) %>%
    arrange(desc(net_return_request_units)) %>%
    mutate(return_request_rate = return_request_units / units_ordered,
           quarterly_ranking = dense_rank(-net_return_request_units),
           performance_percentile = ntile(-net_return_request_units, 1000) / 1000,
           performance_decile = ntile(-net_return_request_units, 10) %>% formatC(flag = "0", width = 2),
           performance_quintile = ntile(-net_return_request_units, 5)) %>%
    group_by(style_number) %>%
    arrange(style_number, order_year_quarter) %>%
    mutate(prior_quarter_net_units = lag(net_return_request_units)) %>%
    left_join(traffic, by = "style_number")

quarterly_summaries <- product_rankings_per_quarter %>%
    group_by(order_year_quarter, performance_decile) %>%
    summarise(styles = n_distinct(style_number),
              total_net_units = sum(net_return_request_units),
              best_style = max(net_return_request_units),
              worst_style = min(net_return_request_units),
              mean_per_style = mean(net_return_request_units),
              sd_per_style = sd(net_return_request_units),
              var_per_style = var(net_return_request_units),
              q_25 = quantile(net_return_request_units, 0.25)[[1]],
              q_75 = quantile(net_return_request_units, 0.75)[[1]],
              iqr = q_75 - q_25) %>%
    mutate(percent_of_sales = total_net_units / sum(total_net_units))

top_10p_summaries <- product_rankings_per_quarter %>%
    filter(performance_decile == "01") %>%
    mutate(top_10p_quartile = ntile(-net_return_request_units, 4)) %>%
    group_by(order_year_quarter, top_10p_quartile) %>%
    summarise(styles = n_distinct(style_number),
              total_net_units = sum(net_return_request_units),
              best_style = max(net_return_request_units),
              worst_style = min(net_return_request_units),
              mean_per_style = mean(net_return_request_units),
              sd_per_style = sd(net_return_request_units),
              var_per_style = var(net_return_request_units),
              q_25 = quantile(net_return_request_units, 0.25)[[1]],
              q_75 = quantile(net_return_request_units, 0.75)[[1]],
              iqr = q_75 - q_25) %>%
    mutate(percent_of_sales = total_net_units / sum(total_net_units))

style_lifecycle <- product_rankings_per_quarter %>%
    filter(order_year_quarter != "2017 2") %>%
    group_by(style_number) %>%
    summarise(quarters = n(),
              total_net_units = sum(net_return_request_units),
              mean_net_units = mean(net_return_request_units),
              sd_net_units = sd(net_return_request_units),
              q_25 = quantile(net_return_request_units, 0.25)[[1]],
              q_75 = quantile(net_return_request_units, 0.75)[[1]],
              iqr = q_75 - q_25)

product_rankings_per_quarter %>%
    filter(order_year_quarter == "2017 1" & performance_decile != "01") %>%
    ggplot(aes(x = performance_decile, y = sessions)) +
    geom_boxplot() +
    scale_y_continuous(labels = short_number, limits = c(0, 2000))