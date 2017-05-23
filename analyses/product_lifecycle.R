source("~/code/analytics/etl/full_global.R")
setwd("~/data")

# ---- YoY Weekly Sales ----
weekly_sales <- products_sold %>%
    filter(order_date <= as.Date("2017-05-20") & payment_state == "paid") %>%
    group_by(order_year = year(order_date) %>% as.character(), 
             order_week = week(order_date)) %>%
    summarise(Units = sum(quantity),
              `Net Sales` = sum(sales_usd))
weekly_sales %>%
    ggplot(aes(x = order_week)) +
    geom_line(aes(y = Units, color = order_year), group = 4) +
    geom_point(aes(y = Units, color = order_year)) +
    scale_x_continuous(limits = c(1, 52), labels = short_number) +
    xlab("Order Week")
weekly_sales %>%
    ggplot(aes(x = order_week)) +
    geom_line(aes(y = `Net Sales`, color = order_year), group = 4) +
    geom_point(aes(y = `Net Sales`, color = order_year)) +
    scale_x_continuous(limits = c(1, 52), labels = short_number) +
    xlab("Order Week")


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
product_rankings_per_quarter <- products_sold %>%
    filter(order_date < today() - 30) %>%
    group_by(style_number, 
             order_year_quarter = paste(year(order_date), quarter(order_date))) %>%
    summarise(units_ordered = sum(quantity),
              return_request_units = sum(return_requested),
              net_return_request_units = units_ordered - return_request_units) %>%
    group_by(order_year_quarter) %>%
    arrange(desc(net_return_request_units)) %>%
    mutate(quarterly_ranking = dense_rank(-net_return_request_units),
           performance_percentile = ntile(-net_return_request_units, 1000) / 1000,
           performance_decile = ntile(-net_return_request_units, 10) %>% formatC(flag = "0", width = 2),
           performance_quintile = ntile(-net_return_request_units, 5))

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
              best_ranking = min(quarterly_ranking),
              worst_ranking = max(quarterly_ranking),
              best_percentile = min(performance_percentile),
              worst_percentile = max(performance_percentile),
              first_quarter = min(order_year_quarter),
              last_quarter = max(order_year_quarter))
