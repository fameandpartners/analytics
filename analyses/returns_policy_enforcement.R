library(dplyr)
library(readr)
library(ggplot2)

return_shipped <- read_csv(
    "~/data/days_to_return_shipped.csv",
    col_types = cols(
        order_number = col_character(),
        shipped_to_customer = col_date(format = ""),
        return_shipped = col_date(format = ""),
        days_to_return_shipped = col_integer())) %>%
    filter(days_to_return_shipped %>% between(0, 90))

percent_return_shipped <- return_shipped %>%
    count(within_policy = days_to_return_shipped <= 30) %>%
    mutate(`%`=scales::percent(n / sum(n))) %>%
    filter(within_policy) %>%
    transmute(`%`, x = 10, y = 75)

return_shipped %>%
    ggplot(aes(days_to_return_shipped)) +
    geom_histogram(binwidth = 1) +
    geom_vline(xintercept = 30) +
    geom_text(data = percent_return_shipped, 
              aes(x=x,y=y,label=`%`), 
              color="white", size = 10) +
    scale_x_continuous(breaks = seq(0, 90, 10)) +
    xlab("Days to Ship Return") +
    ylab("Returned Orders") +
    theme_minimal()
# 97.1% of returns are shipped within the 30 days policy
