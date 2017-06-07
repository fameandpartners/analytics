source("~/code/analytics/etl/full_global.R")
setwd("~/data")

# ---- YoY Weekly Sales ----
weekly_sales <- products_sold %>%
    filter(#order_date <= as.Date("2017-05-27") & 
               payment_state == "paid") %>%
    group_by(order_year = year(order_date) %>% as.character(), 
             order_week = week(order_date)) %>%
    summarise(Units = sum(quantity),
              `Net Sales` = sum(sales_usd))

net_sales <- weekly_sales %>%
    ggplot(aes(x = order_week)) +
    geom_path(aes(y = Units, color = order_year)) +
    geom_point(aes(y = Units, color = order_year)) +
    scale_x_continuous(limits = c(0, 53), breaks = seq(0, 50, 10)) +
    scale_y_continuous(labels = short_dollar) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.x = element_blank()) +
    ylab("Units YoY")

# ---- Weekly GA Sessions and Transactions ----
daily_ga <- read_csv("~/data/daily_ga_conversions.csv",
                      col_types = cols(
                          .default = col_character(),
                          Sessions = col_number(),
                          Transactions = col_number())) %>%
    mutate(Date = as.Date(`Day Index`, format = "%m/%d/%y"))

weekly_ga <- daily_ga %>%
    group_by(Week = week(Date), Platform = Segment) %>%
    summarise(Sessions = sum(Sessions),
              Transactions = sum(Transactions)) %>%
    mutate(Conversion_Rate = Transactions / Sessions)

sessions <- weekly_ga %>%
    filter(Sessions > 0) %>%
    ggplot(aes(x = Week, y = Sessions, color = Platform)) + 
    geom_path() + geom_point() +
    scale_y_continuous(labels = short_number) +
    scale_x_continuous(limits = c(0, 53), breaks = seq(0, 50, 10)) +
    theme(legend.position = "bottom") +
    scale_color_brewer(palette = "Set3") +
    ylab("Sessions YTD") +
    theme(legend.title = element_blank())

library(gridExtra)
grid.arrange(net_sales, sessions)

# --- Snapchat lead conversions ----
sc <- read_csv("~/data/snapchat_emails.csv", col_types = "c") %>%
    rename(email = `Email Address`)

sc %>% inner_join(products_sold, by = "email")
# only