source("~/code/analytics/etl/full_global.R")

bridesmaid_sales <- products_sold %>%
    filter(order_date %>% between(as.Date("2017-04-15"), as.Date("2017-06-28"))
           & collection %in% c("2017 - Bridesmaids 4.2",
                               "2017 - PRE-MADE BRIDESMAID")
           & order_status %in% c("Shipped","Refund Requested","Returned"))

bridesmaid_sales %>%
    group_by(collection, year(order_date), week(order_date)) %>%
    summarise(Units = sum(quantity)) %>%
    summarise(mean(Units))

products_sold %>%
    filter(order_status %in% c("Shipped","Refund Requested","Returned")
           & collection == "2016 - Bridesmaid"
           & order_status %in% c("Shipped","Refund Requested","Returned")
           & year(order_date) == 2016) %>%
    group_by(order_year = year(order_date), order_week = week(order_date)) %>%
    summarise(Units = sum(quantity)) %>%
    summarise(mean(Units))

sales_comparisons <- bridesmaid_sales %>%
    group_by(collection, 
             style = substr(style_number, 1, 6),
             return_requested) %>%
    summarise(Units = sum(quantity)) %>%
    mutate(platform = ifelse(collection %>% str_detect("PRE"),
                             "Pre-Made", "Wedding App"),
           status = ifelse(return_requested, "Returned", "Shipped")) %>%
    inner_join(products %>%
                   transmute(style = style_number, style_name),
               by = "style")

style_sales <- sales_comparisons %>%
    group_by(style_name) %>%
    summarise(sales = sum(Units)) %>%
    arrange(sales)

sales_comparisons$style_name <- factor(
    sales_comparisons$style_name,
    levels = style_sales$style_name
)

sales_comparisons %>%
    ggplot(aes(x = style_name, y = Units, fill = status)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    facet_grid(.~platform) +
    scale_fill_brewer(palette = "Set1") +
    theme_bw(base_size = 16) +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank())

color_comps <- bridesmaid_sales %>%
    filter(!is.na(color)) %>%
    group_by(collection, 
             color,
             status = ifelse(return_requested, "Returned", "Shipped")) %>%
    summarise(Units = sum(quantity)) %>%
    mutate(platform = ifelse(collection %>% str_detect("PRE"),
                             "Pre-Made", "Wedding App"))

color_sales <- color_comps %>%
    group_by(color) %>%
    summarise(sales = sum(Units)) %>%
    arrange(sales)

color_comps$color <- factor(
    color_comps$color,
    levels = color_sales$color
)

color_comps %>%
    ggplot(aes(x = color, y = Units, fill = status)) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(palette = "Set1") +
    coord_flip() +
    facet_grid(.~platform) +
    theme_bw(base_size = 16) +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank())

bridesmaid_sales %>%
    group_by(collection, order_week = week(order_date)) %>%
    summarise(Units = sum(quantity)) %>%
    summarise(avg_weekly_units = mean(Units))

bridesmaid_sales %>%
    group_by(collection) %>%
    summarise(`Net Sales` = sum(sales_usd),
              ASP = mean(sales_usd),
              `Gross Margin` = 
                  (sum(gross_revenue_usd) + 
                       sum(adjustments_usd) - 
                       (sum(coalesce(manufacturing_cost, 70)) + 
                            sum(li_shipping_cost) + 
                            sum(payment_processing_cost)) - 
                       sum(coalesce(refund_amount_usd, 0))) /
                  (sum(gross_revenue_usd) + 
                       sum(adjustments_usd) - 
                       sum(coalesce(refund_amount_usd, 0))))

twilio <- read_csv("~/data/messages_data.csv",
                   col_types = cols(
                       .default = col_character(),
                       date_created = col_datetime(format = ""),
                       user_id = col_integer()
                   )) %>%
    filter(!(author %in% c("Gustavo Robles","K F","Gage Shannon","jojo mojo")))

twilio %>%
    filter(!is.na(user_id) 
           & message_type == "simple" 
           & user_id != 55748
           & author != "BridalBot") %>%
    group_by(channel_sid) %>%
    summarise(messages = n(),
              users = n_distinct(user_id)) %>%
    group_by(users_on_board = ifelse(users < 3, users, "3+")) %>%
    summarise(boards = n(),
              avg_messages_per_board = round(mean(messages))) %>%
    mutate(percent_of_total_boards = percent(boards / sum(boards)))

# fandp_web_production=> select count(distinct user_id) from wedding_atelier_users_event_roles;
# count
# -------
#     7952
# (1 row)

# Percent of signups that engaged with the app's chat interface
engaged <- (twilio %>% filter(
        !is.na(user_id) 
        & user_id != 55748 
        & message_type == "simple"))$user_id %>% 
    n_distinct()
engaged / 7952

# Odds that the stylist will reply ever
channels %>%
    filter(at_stylist == 1) %>%
    count(with_stylist) %>%
    mutate(odds = n / sum(n))

# Given that you're a wedding app customer what are the odds that you used
# our messaging interface
send_messages <- twilio %>%
    filter(!is.na(user_id) & message_type == "simple") %>%
    select(user_id) %>%
    unique()

products_sold %>%
    filter(collection == "2017 - Bridesmaids 4.2" 
           & !str_detect(email, "fameandpartners")) %>%
    select(user_id) %>%
    unique() %>%
    mutate(used_chat = user_id %in% send_messages$user_id) %>%
    count(used_chat) %>%
    mutate(odds = n / sum(n))

# Messages deep dives
three_user_channels <- (channels %>% filter(users >= 3) %>% select(channel_sid))$channel_sid
message_lists <- three_user_channels %>%
    as.list() %>%
    lapply(function(x){
        channel_messages <- twilio %>% 
            filter(channel_sid == x) %>% 
            arrange(date_created)
        channel_messages$sid %>%
            as.list() %>%
            lapply(function(y){
                message <- twilio %>% filter(sid == y)
                return(list("channel" = message$channel_sid,
                            list("author" = message$author,
                                 "message" = message$content,
                                 "time" = message$date_created)))
            })
    })

wedding_app_users <- tbl(fp_con, sql("select distinct user_id, email from wedding_atelier_users_event_roles r inner join spree_users u on u.id = r.user_id where created_at < '2017-07-06'")) %>% collect()

customer_aquisitions <- read_csv("static-data/customer_aquisitions.csv",
                                 col_types = cols(
                                     email = col_character(),
                                     date = col_date(format = "")))

wedding_app_user_acquisitions <- wedding_app_users %>%
    left_join(customer_aquisitions %>%
                  rbind(products_sold %>%
                            group_by(email) %>%
                            summarise(date = min(order_date))) %>%
                  unique(), 
              by = "email")

wedding_app_user_acquisitions %>%
    count(date > as.Date("2017-02-01")) %>%
    mutate(n / sum(n))

products_sold %>%
    inner_join(wedding_app_users, by = "email") %>%
    group_by(`Customer Type` = ifelse(order_date < as.Date("2017-02-01"),
                                      "Before Launch", "After Launch") %>%
                 factor(levels = c("Before Launch","After Launch")),
             Collection = collection) %>%
    summarise(Units = sum(quantity)) %>%
    mutate(`% of Units` = Units / sum(Units)) %>%
    arrange(`Customer Type`, desc(Units))

wedding_app_user_acquisitions %>%
    filter(date > as.Date("2017-02-01")) %>%
    inner_join(tbl(fp_con, sql("select id user_id, created_at::DATE signup_date from spree_users")) %>% collect(n = Inf),
               by = "user_id") %>%
    inner_join(products_sold %>% 
                   filter(collection == "2017 - Bridesmaids 4.2") %>%
                   select(email) %>%
                   unique(),
               by = "email") %>%
    mutate(signup_to_purchase = difftime(date, signup_date, units = "days")) %>%
    group_by(sort = floor(signup_to_purchase / 25) * 25,
             `Sales Cycle` = paste(floor(signup_to_purchase / 25) * 25, "days to",
                                  (floor(signup_to_purchase / 25) * 25) + 25, "days")) %>%
    summarise(Customers = n_distinct(email)) %>%
    ungroup() %>%
    filter(sort >= 0) %>%
    select(-sort) %>%
    mutate(`% of Customers` = Customers / sum(Customers)) %>%
    write_csv("~/data/wedding_app_sales_cycle.csv")