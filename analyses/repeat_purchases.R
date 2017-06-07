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

purchase_nums %>%
    group_by(Repeated = ifelse(purchase_num > 1, "Repeat", "New"), 
             Cohort = coalesce(assigned_cohort, "Not Assigned")) %>%
    filter(Cohort != "Not Assigned") %>%
    summarise(Customers = n_distinct(email)) %>%
    spread(Repeated, Customers) %>%
    mutate(`Likelihood to Repeat` = Repeat / New,
           `Percent of Repeat Customers` = Repeat / sum(Repeat))

purchase_sizes <- purchase_nums %>%
    filter(purchase_num <= 5 & !is.na(us_size)) %>%
    select(email, purchase_num, us_size) %>%
    group_by(email, purchase_num) %>%
    filter(n_distinct(us_size) == 1) %>%
    group_by(email) %>%
    unique() %>%
    arrange(email, purchase_num) %>%
    spread(purchase_num, us_size) %>%
    filter(!is.na(`1`) & !is.na(`2`)) %>%
    ungroup() %>%
    mutate(first_and_second_size_match = `1` == `2`,
           size_abs_delta = abs(`1` - `2`),
           size_delta = `2` - `1`)

purchase_sizes %>% count(first_and_second_size_match)

purchase_sizes %>% count(size_abs_delta) %>% mutate(n / sum(n))

purchase_sizes %>% count(size_delta) %>% mutate(n / sum(n)) %>%
    filter(n > 10) %>%
    ggplot(aes(x = size_delta, y = n)) +
    geom_bar(stat = "identity") +
    scale_x_continuous(breaks = seq(-6,6,2)) +
    xlab("Difference in Size from 1st to 2nd Purchase") +
    ylab("Customers") +
    theme_minimal()
