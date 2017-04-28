setwd("~/code/analytics/ecommerce-performance/")
source("~/code/analytics/ecommerce-performance/global.R")
setwd("~/data")

shipment_data_2017 <- read_csv("ben line item ship dates 2017-04-19.csv",
                                col_types = cols(
                                    .default = col_character(),
                                    `SENT DATE` = col_date(format = "%m/%d/%y"),
                                    LINE = col_integer())) %>%
    rename(order_number = `ORDER NO.`,
           line_item_id = LINE,
           ship_date = `SENT DATE`)

# shit don't match with spree
products_sold %>%
    filter(!is.na(ship_date)) %>%
    inner_join(shipment_data_2017, by = "line_item_id") %>%
    mutate(delta_from_spree = abs(ship_date.x - ship_date.y)) %>%
    count(delta_from_spree) %>%
    mutate(percent_of_total = n / sum(n))
# A tibble: 33 × 3
# delta_from_spree  n percent_of_total
# <time> <int>      <dbl>
# 0 days  3261      0.390913450
# 1 days  2688      0.322224886
# 2 days   925      0.110884680
# 3 days   480      0.057540158
# 4 days   215      0.025773196
# 5 days   240      0.028770079
# 6 days    82      0.009829777
# 7 days    81      0.009709902
# 8 days    62      0.007432270
# 9 days    51      0.006113642
# ... with 23 more rows
products_sold %>%
    filter(is.na(ship_date) & order_date >= as.Date("2017-01-01") & order_date <= as.Date("2017-04-01")) %>%
    inner_join(shipment_data_2017 %>% select(-order_number) %>% rename(correct_ship_date = ship_date), 
               by = "line_item_id")

# ---- DATA TO UPDATE ----
products_sold %>%
    filter(year(ship_date) == 2017 | is.na(ship_date)) %>%
    left_join(shipment_data_2017 %>% select(-order_number) %>% rename(correct_ship_date = ship_date), by = "line_item_id") %>%
    mutate(delta_from_spree = abs(ship_date - correct_ship_date)) %>%
    filter((delta_from_spree > 0 | is.na(delta_from_spree)) & !is.na(correct_ship_date)) %>%
    select(line_item_id, order_id, ship_date, correct_ship_date) %>%
    write_csv("Ship Date Updates 2017-04-25.csv")