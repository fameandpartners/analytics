library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(stringr)

fb <- read_csv("~/code/analytics/ecommerce-performance/static-data/fb.csv",
               col_types = cols(
                   .default = col_number(),
                   Date = col_date(format = ""),
                   Platform = col_character(),
                   utm_campaign = col_character())) %>%
    filter(!str_detect(tolower(utm_campaign), "dance"))

p1 <- fb %>% 
    filter(Date < today() - 24) %>%
    group_by(ad_year = year(Date) %>% as.character(), 
             ad_month = month(Date)) %>%
    summarise(CPC = sum(Amount_Spent_AUD * 0.75) / sum(Unique_Clicks)) %>%
    ggplot(aes(x = ad_month, y = CPC, color = ad_year)) +
    geom_path() + geom_point() +
    scale_x_continuous(breaks = seq(0, 12, 1), limits = c(1,12)) +
    scale_y_continuous(labels = scales::dollar, limits = c(0,1)) +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Facebook Acquisitions")

p2 <- fb %>% 
    filter(Date < today() - 24) %>%
    group_by(ad_year = year(Date) %>% as.character(), 
             ad_month = month(Date)) %>%
    summarise(sample_size = n(),
              CAC = sum(Amount_Spent_AUD * 0.75) / sum(Purchases)) %>%
    ggplot(aes(x = ad_month, y = CAC, color = ad_year)) +
    geom_path() + geom_point() +
    scale_x_continuous(breaks = seq(1, 12, 1), limits = c(1, 12)) +
    scale_y_continuous(labels = scales::dollar, limits = c(0, 250)) +
    theme(legend.title = element_blank()) +
    xlab("Month")

library(gridExtra)
grid.arrange(p1, p2)

p3 <- fb %>%
    filter(Date < today() - 24) %>%
    group_by(ad_year = year(Date) %>% as.character(), 
             ad_week = week(Date)) %>%
    summarise(clicks = sum(Unique_Clicks),
              reach = sum(Reach), 
              CTR = sum(Unique_Clicks) / sum(Reach)) %>%
    ggplot(aes(x = ad_week, y = CTR, color = ad_year)) +
    geom_path() + geom_point() +
    scale_y_continuous(breaks = 0:2 / 100,
                       labels = scales::percent, 
                       limits = c(0, 0.025)) +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Facebook CTR & Conversions")

p4 <- fb %>%
    filter(Date < today() - 24) %>%
    group_by(ad_year = year(Date) %>% as.character(), 
             ad_week = week(Date)) %>%
    summarise(sample_size = n(),
              `Conversion Rate` = sum(Purchases) / sum(Unique_Clicks)) %>%
    ggplot(aes(x = ad_week, y = `Conversion Rate`, color = ad_year)) +
    geom_path() + geom_point() +
    scale_y_continuous(breaks = 0:2 / 100,
                       labels = scales::percent, 
                       limits = c(0, 0.025)) +
    theme(legend.title = element_blank()) +
    xlab("Month") +
    ylab("Conversion Rate (purchases / clicks)")

grid.arrange(p3, p4)

fb %>%
    filter(Date < today() - 24) %>%
    group_by(ad_year = year(Date) %>% as.character(), 
             ad_month = month(Date)) %>%
    summarise(Clicks = sum(Unique_Clicks),
              Reach = sum(Reach),
              CTR = sum(Unique_Clicks) / sum(Reach)) %>%
    write_csv("~/data/monthly_ctr_yoy.csv")

fb %>%
    filter(Date < today() - 24) %>%
    group_by(ad_year = year(Date) %>% as.character(), 
             ad_month = month(Date)) %>%
    summarise(Clicks = sum(Unique_Clicks),
              Purchases = sum(Purchases),
              `Conversion Rate` = sum(Purchases) / sum(Unique_Clicks)) %>%
    write_csv("~/data/monthly_conversions_yoy.csv")

fb %>%
    filter(Date < today() - 24) %>%
    group_by(ad_year = year(Date) %>% as.character(), 
             ad_month = month(Date)) %>%
    summarise(Comments = sum(Post_Comments),
              Reactions = sum(Post_Reactions),
              Shares = sum(Post_Shares),
              Reach = sum(Reach)) %>%
    write_csv("~/data/Facebook Monthly Engagement.csv")