wedding_emails <- read_csv("~/data/wedding_emails.csv")

wedding_email_sales_cycle <- products_sold %>% 
    filter(order_date >= as.Date("2017-02-01")) %>%
    group_by(email) %>% 
    summarise(first_purchase_date = min(order_date)) %>% 
    inner_join(wedding_emails, by = "email") %>% 
    mutate(purchase_to_wedding_date = difftime(wedding_date, first_purchase_date, "days"))

wedding_email_sales_cycle$purchase_to_wedding_date %>% mean
# Time difference of 145.1579 days
wedding_email_sales_cycle$purchase_to_wedding_date %>% median
# Time difference of 102 days
wedding_email_sales_cycle$purchase_to_wedding_date %>% quantile(seq(0,1,0.1))
# Time differences in days
#    0%    10%    20%    30%    40%    50%    60%    70%    80%    90%   100% 
# -53.0   26.4   50.8   65.0   78.6  102.0  130.4  165.8  200.4  268.6 2634.0

products_sold %>% 
    filter(order_date >= as.Date("2017-02-01")) %>%
    mutate(wedding_app_user = email %in% wedding_email_sales_cycle$email) %>%
    group_by(wedding_app_user, collection) %>%
    summarise(units_sold = sum(quantity),
              net_sales = sum(sales_usd)) %>%
    arrange(wedding_app_user, desc(net_sales)) %>%
    mutate(percent_of_total_sales = net_sales / sum(net_sales))

wedding_app_sales_cycle <- products_sold %>% 
    filter(order_date >= as.Date("2017-02-01")) %>%
    group_by(email) %>% 
    summarise(first_purchase_date = min(order_date)) %>% 
    inner_join(wedding_emails, by = "email") %>% 
    mutate(purchase_to_wedding_date = difftime(wedding_date, first_purchase_date, "days"))

wedding_app_sales_cycle$purchase_to_wedding_date %>% mean
# Time difference of 145.1579 days
wedding_app_sales_cycle$purchase_to_wedding_date %>% median
# Time difference of 102 days
wedding_app_sales_cycle$purchase_to_wedding_date %>% quantile(seq(0,1,0.1))
# Time differences in days
#    0%    10%    20%    30%    40%    50%    60%    70%    80%    90%   100% 
# -53.0   26.4   50.8   65.0   78.6  102.0  130.4  165.8  200.4  268.6 2634.0 

engagement_cadence <- wedding_emails %>% 
    filter(dresses > 0) %>%
    mutate(days_till_first_dress_add = first_dress_add_date - board_create_date,
           days_till_last_dress_add = last_dress_add_date - board_create_date)

quantile(engagement_cadence$days_till_first_dress_add, seq(0,1,0.01))
quantile(engagement_cadence$days_till_last_dress_add, seq(0,1,0.01))

engagement_cadence %>%
    filter(days_till_first_dress_add > 0 | days_till_last_dress_add > 0) %>%
    inner_join(wedding_app_sales_cycle, by = "email")