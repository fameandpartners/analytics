library(readr)
library(dplyr)
library(stringr)
library(tidyr)

ga <- read_csv("/Users/Peter 1/Dropbox (Team Fame)/csv_data/misc/conversions_by_second_page.csv",
               col_types = cols(
                 `Second Page` = col_character(),
                 Sessions = col_integer(),
                 Transactions = col_integer(),
                 `Ecommerce Conversion Rate` = col_double())) %>%
    mutate(temp_page = substr(`Second Page`, 2, 250)) %>%
    separate(temp_page, c("page_root"), sep = "/", extra = "drop") %>%
    replace(. == "", NA)

ga %>% count(round(Transactions / Sessions, 4) == `Ecommerce Conversion Rate`)
# Ecommerce Conversion Rate = Transactions / Sessions

ga %>%
    group_by(page_root) %>%
    summarise(sessions = sum(Sessions),
              transactions = sum(Transactions)) %>%
    mutate(ecommerce_conversion_rate = round(transactions / sessions, 4)) %>%
    arrange(desc(sessions))

ga %>%
    summarise(sessions = sum(Sessions), transactions = sum(Transactions)) %>%
    mutate(ecommerce_conversion_rate = round(transactions / sessions, 4))