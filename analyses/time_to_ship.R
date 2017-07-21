med_ship_times <- products_sold %>%
    filter(is_shipped) %>%
    group_by(ship_year_month, product_id) %>%
    summarise(Units = sum(quantity),
              med_order_to_ship = as.integer(
                  median(difftime(ship_date, 
                                  order_date, 
                                  units = "days"),
                         na.rm = T))) %>%
    group_by(product_id) %>%
    arrange(ship_year_month) %>%
    mutate(prior_month_units = lag(Units),
           prior_months_bucket = ifelse(is.na(prior_month_units), "0 Units",
                                        ifelse(prior_month_units >= 50, "50+ Units",
                                               ifelse(prior_month_units >= 25, "25-50 Units",
                                                      "0-25 Units"))) %>%
               factor(levels = c("0 Units", "0-25 Units", "25-50 Units", "50+ Units"))) %>%
    filter(med_order_to_ship < 100)

med_ship_times %>%
    ggplot(aes(x = prior_months_bucket, y = med_order_to_ship)) +
    geom_boxplot() + 
    scale_y_log10() +
    xlab("Units of that Product Shipped in Prior Month") +
    ylab("Order to Ship Days")

products_sold %>%
    # filter(year(ship_date) == 2017) %>%
    mutate(order_to_ship = as.integer(difftime(ship_date, order_date, units = "days"))) %>%
    filter(is_shipped & order_to_ship %>% between(0, 50)) %>%
    ggplot(aes(x = order_to_ship)) +
    geom_histogram(binwidth = 1) +
    scale_x_continuous(breaks = seq(0, 100, 5)) +
    xlab("Order to Ship in Days") +
    ylab("Units") +
    theme_minimal()

cum_ship_times <- products_sold %>%
    mutate(order_to_ship = as.integer(difftime(ship_date, order_date, units = "days"))) %>%
    filter(is_shipped & order_to_ship %>% between(0, 50)) %>%
    group_by(ship_year_month, product_id) %>%
    summarise(Units = sum(quantity),
              med_order_to_ship = as.integer(median(order_to_ship))) %>%
    group_by(product_id) %>%
    arrange(ship_year_month) %>%
    mutate(lagging_cumsum_units = cumsum(Units) - Units,
           total_units_made_bucket = ifelse(lagging_cumsum_units == 0, "0 Units",
                                            ifelse(lagging_cumsum_units <= 10, "0-10 Units",
                                                   ifelse(lagging_cumsum_units <= 25, "10-25 Units",
                                                          ifelse(lagging_cumsum_units <= 50, "25-50 Units",
                                                                 "50+ Units")))) %>%
               factor(levels = c("0 Units","0-10 Units","10-25 Units","25-50 Units","50+ Units")))

cum_ship_times %>%
    ggplot(aes(x = total_units_made_bucket, y = med_order_to_ship)) +
    geom_boxplot() +
    scale_y_log10()

products_sold %>%
    mutate(order_to_ship = as.integer(difftime(ship_date, order_date, units = "days"))) %>%
    filter(is_shipped & order_to_ship %>% between(0, 50)) %>%
    group_by(collection) %>%
    summarise(mean_ship_time = mean(order_to_ship))