library(dplyr)
library(scales)
library(readr)
library(stringr)
library(lubridate)

source("~/code/analytics/ecommerce-performance/fp_init.R")

br_csv <- read_csv("~/data/Contacts-Master_List.csv",
                   col_types = cols(
                       .default = col_character()),
                   skip = 3) %>%
    mutate(CONFIRM_TIME = substr(`Date Created`, 1, 10) %>%
               as.Date(format = "%m/%d/%Y") %>%
           as.POSIXct())

mc_csv <- read_csv("~/data/subscribed_members_export_3ccca61159.csv",
                   col_types = cols(
                       .default = col_character(),
                       Birthday = col_date(format = ""),
                       `Facebook UID` = col_double(),
                       MEMBER_RATING = col_integer(),
                       OPTIN_TIME = col_datetime(format = ""),
                       CONFIRM_TIME = col_datetime(format = ""),
                       LAST_CHANGED = col_datetime(format = ""),
                       LEID = col_integer()
                   ))

mc <- bind_rows(list(
    mc_csv %>%
        rename(email = `Email Address`, event_type_d = `Event Type`) %>%
        select(email, event_type_d, utm_source, utm_medium, utm_campaign, CONFIRM_TIME),
    br_csv %>%
        rename(email = `Email Address`, event_type_d = `EventType`) %>%
        select(email, event_type_d, utm_source, utm_medium, utm_campaign, CONFIRM_TIME))) %>%
    filter(!duplicated(email)) %>%
    mutate(mc_cohort = ifelse(str_detect(tolower(event_type_d), "bridal"),
                              "Bridal", 
                              ifelse(str_detect(tolower(event_type_d), "formal|cocktail|wedding guest|daytime|work"), 
                                     "Contemporary",
                                     ifelse(event_type_d == "Prom", "Prom", NA))))

# order carts that were created from 2016 to present
orders <- tbl(fp_con, sql(paste(
    "SELECT",
        "o.id order_id,",
        "o.number order_number,",
        "o.email,",
        "o.user_id,",
        "o.user_first_name || ' ' || o.user_last_name customer_name,",
        "TO_CHAR(li.created_at, 'YYYY-MM-DD HH24:MI:SS') added_to_cart_at_char,",
        "TO_CHAR(o.completed_at, 'YYYY-MM-DD HH24:MI:SS') completed_at_char,",
        "o.state,",
        "o.currency,",
        "o.total",
    "FROM spree_line_items li",
    "INNER JOIN spree_orders o ON li.order_id = o.id",
    "WHERE li.created_at >= '2016-01-01'",
        "AND o.total IS NOT NULL",
        "AND o.total > 0"))) %>%
    collect(n = Inf) %>%
    mutate(ordered_at = ymd_hms(completed_at_char),
           added_to_cart_at = ymd_hms(added_to_cart_at_char)) %>%
    select(-completed_at_char, -added_to_cart_at_char) %>%
    group_by(order_id, order_number, email, user_id, customer_name, state, currency) %>%
    summarise(ordered_at = max(ordered_at),
              added_to_cart_at = max(added_to_cart_at),
              total = max(total)) %>% ungroup() %>%
    mutate(revenue_usd = total * ifelse(currency == "AUD", 0.75, 1),
           order_year_month = paste(year(ordered_at), 
                                    formatC(month(ordered_at), 
                                            width = 2, flag = "0"), 
                                    sep = "-"),
           cart_year_month = paste(year(added_to_cart_at), 
                                   formatC(month(added_to_cart_at), 
                                           width = 2, flag = "0"), 
                                   sep = "-"))

# utm touches created from 2016 to present
utm <- tbl(fp_con, sql(paste(
    "SELECT",
        "TO_CHAR(motp.created_at, 'YYYY-MM-DD HH24:MI:SS') touch_time_char,",
        "motp.order_id,",
        "NULL user_id,",
        "motp.utm_source,",
        "motp.utm_medium,",
        "motp.utm_campaign,",
        "'Order' touch_type",
    "FROM marketing_order_traffic_parameters motp",
    "INNER JOIN (",
        "SELECT DISTINCT order_id",
        "FROM spree_line_items",
        "WHERE created_at >= '2016-01-01'",
    ") li ON li.order_id = motp.order_id",
    "WHERE created_at >= '2016-01-01'",
    "AND motp.order_id IS NOT NULL",
    "UNION",
    "SELECT",
        "TO_CHAR(muv.created_at, 'YYYY-MM-DD HH24:MI:SS'),",
        "NULL order_id,",
        "muv.spree_user_id,",
        "muv.utm_source,",
        "muv.utm_medium,",
        "muv.utm_campaign,",
        "'Visit' touch_type",
    "FROM marketing_user_visits muv",
    "WHERE created_at >= '2016-01-01'",
        "AND spree_user_id IS NOT NULL"))) %>%
    collect(n = Inf) %>%
    mutate(touch_time = ymd_hms(touch_time_char))

# build cohorts based on utm and mailchimp data

utm_cohorts <- 
    utm %>% 
    mutate(full_utm = paste(utm_source, utm_medium, utm_campaign, sep = " | ") %>% tolower()) %>% 
    filter(str_detect(full_utm, "prom|wedding|bridal|contemporary")) %>%
    mutate(utm_cohort = ifelse(str_detect(full_utm, "prom"), "Prom",
                               ifelse(str_detect(full_utm, "wedding|bridal"), "Bridal",
                                      ifelse(str_detect(full_utm, "contemporary"), "Contemporary", NA))))

user_cohorts <-
    utm_cohorts %>%
    filter(!is.na(user_id)) %>%
    group_by(user_id) %>%
    summarise(utm_cohort_u = utm_cohort[1])

order_cohorts <-
    utm_cohorts %>%
    filter(!is.na(order_id)) %>%
    group_by(order_id) %>%
    summarise(utm_cohort_o = utm_cohort[1])

utm_cohort_assignments <- 
    orders %>%
    left_join(user_cohorts, by = "user_id") %>%
    left_join(order_cohorts, by = "order_id") %>%
    mutate(utm_cohort = ifelse(is.na(utm_cohort_u), utm_cohort_o, utm_cohort_u)) %>%
    select(-utm_cohort_o, -utm_cohort_u) %>%
    left_join(mc %>% 
                  select(email, mc_cohort) %>% 
                  filter(!is.na(mc_cohort)),
              by = "email") %>%
    mutate(cohort = ifelse(is.na(mc_cohort), utm_cohort, mc_cohort)) %>%
    select(user_id, email, contains("cohort")) %>%
    filter(!is.na(cohort) & (!is.na(user_id) | !is.na(email))) %>%
    unique()

cohort_assignments <-
    rbind(utm_cohort_assignments %>% select(-user_id),
          mc %>% 
              filter(!is.na(mc_cohort)) %>%
              anti_join(utm_cohort_assignments, by = "email") %>%
              transmute(email,
                        utm_cohort = NA, 
                        mc_cohort,
                        cohort = mc_cohort))

touches <- bind_rows(list(
    # user_id touches
    orders %>% select(order_id, user_id, email) %>%
        filter(!is.na(user_id)) %>%
        unique() %>%
        inner_join(utm %>% select(-order_id), by = "user_id"),
    # order_id touches
    orders %>% select(order_id, user_id, email) %>%
        filter(!is.na(order_id)) %>%
        unique() %>%
        inner_join(utm %>% select(-user_id), by = "order_id"),
    # Mailchimp/email touches
    orders %>% select(order_id, user_id, email) %>%
        filter(!is.na(email)) %>%
        unique() %>%
        inner_join(mc %>%
                       filter(!is.na(utm_source) | !is.na(utm_medium) | !is.na(utm_campaign)) %>%
                       transmute(email,
                                 touch_time = CONFIRM_TIME,
                                 utm_source,
                                 utm_medium,
                                 utm_campaign,
                                 touch_type = "Mailchimp"), 
                   by = "email")))

step_state_map <- data_frame(state = c("complete","cart","payment","address","canceled","resumed"),
                             step = c("Purchase","Cart","Checkout","Checkout","Checkout","Checkout"))

all_touches <-
    touches %>%
    full_join(orders %>% select(-email, -user_id), by = "order_id") %>%
    left_join(cohort_assignments, by = "email") %>%
    mutate(cohort = coalesce(cohort, "Not Assigned"),
           utm_s = coalesce(utm_source, "No UTM Source")) %>%
    left_join(step_state_map, by = "state")

write_csv(all_touches, "~/code/analytics/ecommerce-performance/static-data/all_touches.csv", na = "")
