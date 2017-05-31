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
             ad_month = month(Date)) %>%
    summarise(CPC = sum(Amount_Spent_AUD * 0.75) / sum(Unique_Clicks)) %>%
    ggplot(aes(x = ad_month, y = CPC, color = ad_year)) +
    geom_line(group = 2) + geom_point() +
    scale_x_continuous(breaks = seq(0, 12, 1), limits = c(1,11)) +
    scale_y_continuous(labels = scales::dollar, limits = c(0,1)) +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank()) 

p2 <- fb %>% 
    filter(Date < today() - 24) %>%
    group_by(ad_year = year(Date) %>% as.character(), 
             ad_month = month(Date)) %>%
    summarise(sample_size = n(),
              CAC = sum(Amount_Spent_AUD * 0.75) / sum(Purchases)) %>%
    ggplot(aes(x = ad_month, y = CAC, color = ad_year)) +
    geom_line(group = 2) + geom_point() +
    scale_x_continuous(breaks = seq(1, 12, 1), limits = c(1, 12)) +
    scale_y_continuous(labels = scales::dollar, limits = c(0, 250)) +
    theme(legend.title = element_blank()) +
    xlab("month")

library(gridExtra)
grid.arrange(p1, p2)

fb %>%
    filter(Date < today() - 24) %>%
    summarise(sample_size = n(),
              CTR = sum(Unique_Clicks) / sum(Reach))