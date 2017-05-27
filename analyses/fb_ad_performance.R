library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

fb <- read_csv("~/code/analytics/ecommerce-performance/static-data/fb.csv",
               col_types = cols(
                   .default = col_number(),
                   Date = col_date(format = ""),
                   Platform = col_character(),
                   utm_campaign = col_character()))

p1 <- fb %>% 
    filter(Date < today() - 24) %>%
    group_by(ad_year = year(Date) %>% as.character(), 
             ad_week = week(Date)) %>%
    summarise(CPC = sum(Amount_Spent_AUD * 0.75) / sum(Unique_Clicks)) %>%
    ggplot(aes(x = ad_week, y = CPC, color = ad_year)) +
    geom_line(group = 2) + geom_point() +
    scale_x_continuous(breaks = seq(0, 50, 10), limits = c(1,51)) +
    scale_y_continuous(labels = scales::dollar, limits = c(0,1)) +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank()) 

p2 <- fb %>% 
    filter(Date < today() - 24) %>%
    group_by(ad_year = year(Date) %>% as.character(), 
             ad_week = week(Date)) %>%
    summarise(sample_size = n(),
              CAC = sum(Amount_Spent_AUD * 0.75) / sum(Purchases)) %>%
    ggplot(aes(x = ad_week, y = CAC, color = ad_year)) +
    geom_line(group = 2) + geom_point() +
    scale_x_continuous(breaks = seq(0, 50, 10), limits = c(1, 51)) +
    scale_y_continuous(labels = scales::dollar, limits = c(0, 250)) +
    theme(legend.title = element_blank()) +
    xlab("Week")

library(gridExtra)
grid.arrange(p1, p2)
