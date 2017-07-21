products_sold %>%
    filter(collection == "2017 - Bridal 4.2" 
           & between(order_date, as.Date("2017-05-20"), today())) %>%
    select(email) %>%
    inner_join(all_touches %>%
                   filter(!is.na(email)),
               by = "email") %>%
    select(email, touch_time, utm_source, utm_medium, utm_campaign, step) %>%
    unique() %>%
    write_csv("~/data/bridal_customer_touchpoints.csv")