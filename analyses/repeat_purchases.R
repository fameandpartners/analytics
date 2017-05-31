source("~/code/analytics/etl/full_global.R")
setwd("~/data")

purchase_nums <- products_sold %>%
    filter(!str_detect(email, "fameandpartners")) %>%
    group_by(email) %>%
    mutate(purchase_num = dense_rank(order_date))

purchase_nums %>% 
    group_by(order_year_month) %>%
    summarise(total_customers = n_distinct(email),
              repeat_customers = n_distinct(ifelse(purchase_num > 1, email, NA),
                                            na.rm = TRUE)) %>%
    mutate(repeat_rate = repeat_customers / total_customers) %>%
    filter(!str_detect(order_year_month, "2015"))

purchase_dates <- purchase_nums %>%
    filter(purchase_num <= 5) %>%
    select(email, purchase_num, order_date) %>%
    unique() %>%
    spread(purchase_num, order_date)

purchase_dates %>%
    ungroup() %>%
    summarise(
        first_to_second = mean(`2` - `1`, na.rm = T),
        second_to_third = mean(`3` - `2`, na.rm = T),
        third_to_fourth = mean(`4` - `3`, na.rm = T),
        forth_to_fifth = mean(`5` - `4`, na.rm = T)
    )

time_to_repeat <- purchase_dates %>%
    filter(!is.na(`2`)) %>%
    mutate(first_to_second = `2` - `1`) %>%
    select(email, first_to_second)

time_to_repeat$first_to_second %>% quantile(seq(0, 1, 0.1))

time_to_repeat %>%
    ggplot(aes(first_to_second)) +
    geom_histogram(binwidth = 15) +
    scale_x_continuous(breaks = seq(0, 600, 30),
                       limits = c(0, 600)) +
    xlab("Days from First to Second Purchase") +
    ylab("Customers") +
    theme_minimal()