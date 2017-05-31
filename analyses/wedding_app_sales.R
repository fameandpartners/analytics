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

# Weekly App Sales and Engagement
wedding_app_users <- tbl(fp_con, sql(paste(
    "SELECT DISTINCT",
    "er.signup_date,",
    "uer.user_id",
    "FROM wedding_atelier_users_event_roles uer",
    "INNER JOIN (",
        "SELECT id, MIN(created_at::DATE) signup_date",
        "FROM wedding_atelier_event_roles",
        "GROUP BY id) er",
    "ON er.id = uer.event_role_id"))) %>%
    collect()

wedding_app_customers <- tbl(fp_con, sql(paste(
    "SELECT DISTINCT",
    "w.user_id,",
    "o.completed_at::DATE order_date",
    "FROM spree_line_items li",
    "INNER JOIN spree_orders o",
    "ON o.id = li.order_id",
    "INNER JOIN spree_variants v",
    "ON v.id = li.variant_id",
    "INNER JOIN spree_products p",
    "ON p.id = v.product_id",
    "INNER JOIN (",
        "SELECT DISTINCT user_id",
        "FROM wedding_atelier_users_event_roles) w",
    "ON w.user_id = o.user_id",
    "WHERE completed_at IS NOT NULL",
    "AND p.id IN (1316,1317,1318,1319,1320,1321,1322,1323)",
    "AND o.email NOT LIKE '%fameandpartners.com'",
    "AND o.email != 'gradient@gmail.com'"))) %>%
    collect() %>%
    mutate(converted = 1)

# Signup to Conversion
wedding_app_users %>%
    left_join(wedding_app_customers %>% 
                  select(-order_date) %>%
                  unique(), by = "user_id") %>%
    group_by(signup_date) %>%
    summarise(signups = n(),
              signups_that_converted_in_the_app = sum(coalesce(converted, 0))) %>%
    mutate(conversion_rate = signups_that_converted_in_the_app / signups) %>%
    write_csv("~/data/signup_to_conversion_in_wedding_app_by_day.csv")

# Sales on App vs Site
products_sold %>%
    inner_join(wedding_app_users, by = "user_id") %>%
    filter(order_date >= signup_date) %>%
    group_by(collection) %>%
    summarise(sales = sum(sales_usd)) %>%
    arrange(desc(sales)) %>%
    mutate(percent_of_sales = sales / sum(sales)) %>%
    write_csv("~/data/wedding_app_user_sales.csv")

# Repeat Wedding App Users
repeat_customer_sales <- products_sold %>%
    inner_join(wedding_app_customers, by = "user_id") %>%
    inner_join(wedding_app_users, by = "user_id") %>%
    filter(order_date >= signup_date) %>%
    group_by(email, order_id, order_date) %>%
    summarise(items = sum(quantity),
              returns_requested = sum(return_requested * quantity),
              total_sales_usd = sum(sales_usd)) %>%
    group_by(email) %>%
    mutate(repeat_orders = n_distinct(order_date) - 1,
           order_num = dense_rank(order_date)) %>%
    filter(repeat_orders > 0) %>%
    arrange(email, order_date)

repeat_customer_sales$total_sales_usd %>% mean()

repeat_customer_sales %>%
    select(email, order_date, order_num) %>%
    unique() %>%
    spread(order_num, order_date) %>%
    mutate(time_to_second_order = `2` - `1`)

# Engagement and Sales Cadence
sales_cadence <- wedding_app_users %>%
    inner_join(wedding_app_customers %>% select(-converted), by = "user_id") %>%
    select(email, order_date, signup_date) %>%
    unique() %>%
    mutate(signup_to_purchase = order_date - signup_date)

sales_cadence$signup_to_purchase %>% mean

sales_cadence %>% 
    ggplot(aes(signup_to_purchase)) + 
    geom_histogram(binwidth = 1) + 
    ylab("Wedding App Customers") + 
    xlab("Days from Signup to Purchase") + 
    scale_x_continuous(breaks = seq(0, 100, 5)) +
    theme_minimal()