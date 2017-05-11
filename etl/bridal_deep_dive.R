library(readr)
library(dplyr)
library(stringr)
library(lubridate)

#source("~code/analytics/etl/all_touches.R")

ga_by_segment <- read_csv(
    "~/data/Analytics All Web Site Data Daily Marketing Export 20170101-20170507.csv",
    skip = 6,
    col_types = cols(
        .default = col_number(),
        Source = col_character(),
        Medium = col_character(),
        Campaign = col_character(),
        `Date Range` = col_character(),
        Segment = col_character(),
        Date = col_date(format = "%Y%m%d"))
)

bronto <- read_csv("~/data/Contacts-All_Contacts.csv",
                   skip = 2,
                   col_types = cols(.default = col_character())) %>%
    mutate(confirm_datetime = confirm_time %>% ymd_hms())

# Segment Filter Criteria

# 2017 Weddings FB (AU)
# UTM SOURCE:
# Matches REGEX facebook|fbps|igps|adquadrant|psocial|facebook-paid|instagram-paid|CPC
# Does not match REGEX referral|prom|contemporary|google|bing|polyvore|marchoffer|paydayflash|weddinggiveaway|flashsale|not set
# UTM CAMPAIGN:
# Matches AU
# Does not match REGEX prom|contemporary|retargeting|not set|shop-now-button|22817

# 2017 Weddings FB (US)
# UTM SOURCE:
# Matches REGEX facebook|fbps|igps|adquadrant|psocial|facebook-paid|instagram-paid|CPC
# Does not match REGEX referral|prom|contemporary|google|bing|polyvore|marchoffer|paydayflash|weddinggiveaway|flashsale|not set
# UTM CAMPAIGN:
# Matches US
# Does not match REGEX prom|contemporary|retargeting|not set|shop-now-button|22817

# 2017 Weddings Pinterest (ALL)
# UTM SOURCE:
# Matches REGEX pinterest|pinterestps

# 2017 Weddings PPC (ALL)
# UTM MEDIUM:
# Matches REGEX google / cpc|bing / cpc
# Does not match REGEX polyvore|shopping|trademark
# UTM CAMPAIGN:
# Matches REGEX non-brand
# Matches REGEX wedding

# 2017 Weddings FB
merged_touches <- all_touches %>%
    select(email, touch_time, utm_source, utm_medium, utm_campaign, touch_type) %>%
    rbind(bronto %>%
              filter(!is.na(confirm_datetime)) %>%
              filter(!is.na(utm_source) | !is.na(utm_medium) | !is.na(utm_campaign)) %>%
              transmute(email = `Email Address`, 
                        touch_time = confirm_datetime,
                        touch_type = "Bronto",
                        utm_source,utm_medium, utm_campaign)) %>%
    unique() %>%
    filter(email %in% bronto$`Email Address`)

weddings_fb <- merged_touches %>%
    filter(touch_time >= as.Date("2017-01-01")) %>%
    filter(month(touch_time) == 4) %>%
    filter(str_detect(tolower(utm_source), "facebook|fbps|igps|adquadrant|psocial|facebook-paid|instagram-paid|CPC")
           & !str_detect(tolower(paste(utm_source, utm_medium)), "referral|prom|contemporary|google|bing|polyvore|marchoffer|paydayflash|weddinggiveaway|flashsale|not set")
           & !str_detect(tolower(utm_campaign), "prom|contemporary|retargeting|not set|shop-now-button|22817"))

emails_by_touches <- merged_touches %>%
    filter(email %in% weddings_fb$email & !is.na(email) & email != "mollie.meyer@gmail.com") %>%
    group_by(email) %>%
    arrange(touch_time) %>%
    summarise(n = n(), 
              touch_types = paste(touch_type, collapse = ","), 
              sales_cycle = difftime(max(touch_time), min(touch_time), units = "days")) %>%
    filter(sales_cycle > 0) %>%
    filter(touch_types %>% str_detect("Order")) %>%
    arrange(desc(n))

write_csv(emails_by_touches, "FB Emails with Touches April.csv")
summary(emails_by_touches$sales_cycle %>% as.numeric())

# 2017 Weddings FB (AU)
weddings_fb_au <- weddings_fb %>%
    filter(str_detect(utm_campaign, "AU"))

weddings_fb_au %>%
    group_by(utm_source, utm_medium, utm_campaign) %>%
    summarise(signups = n(),
              orders = sum(touch_type == "Order")) %>%
    mutate(signup_to_order = orders / signups) %>%
    #View("2017 Weddings FB (AU)")
    write_csv("2017 Weddings FB (AU) April.csv")

# 2017 Weddings FB (US)
weddings_fb_us <- weddings_fb %>%
    filter(str_detect(utm_campaign, "US"))

weddings_fb_us %>%
    group_by(utm_source, utm_medium, utm_campaign) %>%
    summarise(signups = n(),
              orders = sum(touch_type == "Order")) %>%
    mutate(signup_to_order = orders / signups) %>%
    #View("2017 Weddings FB (US)")
    write_csv("2017 Weddings FB (US) April.csv")

# 2017 Weddings Pinterest (ALL)
weddings_pin <- merged_touches %>%
    filter(touch_time >= as.Date("2017-01-01")) %>%
    filter(month(touch_time) == 4) %>%
    filter(str_detect(tolower(paste(utm_source,utm_medium)), "pinterest|pinterestps"))

weddings_pin %>%
    group_by(utm_source, utm_medium, utm_campaign) %>%
    summarise(signups = n(),
              orders = sum(touch_type == "Order")) %>%
    mutate(signup_to_order = orders / signups) %>%
    arrange(utm_source) %>%
    #View("2017 Weddings Pinterest (ALL)") %>%
    write_csv("2017 Weddings Pinterest (ALL) April.csv")

weddings_pin %>%
    group_by(month = month(touch_time)) %>%
    summarise(signups = n(),
              orders = sum(touch_type == "Order")) %>%
    mutate(signup_to_order = orders / signups) %>%
    #View("2017 Weddings Pinterest - Monthly") %>%
    write_csv("2017 Weddings Pinterest - Monthly April.csv")

# 2017 Weddings PPC (ALL)
weddings_ppc <- merged_touches %>%
    filter(touch_time >= as.Date("2017-01-01")) %>%
    filter(month(touch_time) == 4) %>%
    filter(str_detect(tolower(utm_medium), "google|cpc|bing|cpc")
           & str_detect(tolower(utm_campaign), "non-brand")
           & !str_detect(tolower(utm_medium), "polyvore|shopping|trademark")
           & str_detect(tolower(utm_campaign), "wedding"))

weddings_ppc %>%
    group_by(utm_source, utm_medium, utm_campaign) %>%
    summarise(signups = n(),
              orders = sum(touch_type == "Order")) %>%
    mutate(signup_to_order = orders / signups) %>%
    #View("2017 Weddings PPC (ALL)") %>%
    write_csv("2017 Weddings PPC (ALL) April.csv")

