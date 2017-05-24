library(dplyr)
library(readr)

assembly_csv1 <- read_csv(
    "~/data/Payments Data/assembly_payments_Jan_to_April_2017.csv",
    col_types = cols(
        .default = col_character(),
        `Batch Post Day` = col_date(format = "")))
assembly_csv2 <- read_csv(
    "~/data/Payments Data/assembly_payments_May_partial.csv",
    col_types = cols(
        .default = col_character(),
        `Batch Post Day` = col_date(format = "")))
assembly_csv <- bind_rows(list(assembly_csv1, assembly_csv2)) %>%
    mutate(response_code24 = `Merchant Order #`)

assembly_csv$`Purchase Amt` %>% as.numeric() %>% sum(na.rm = T) %>% scales::dollar()

assembly_dupes <- assembly_csv %>% 
    filter(!is.na(response_code24)) %>%
    count(response_code24) %>% 
    filter(n > 1) %>%
    inner_join(assembly_csv, by = "response_code24") %>%
    select(-n) %>%
    arrange(response_code24)

assembly_dupes %>% 
    filter(duplicated(response_code24)) %>% 
    summarise(extra_from_dupes = `Purchase Amt` %>% 
                  as.numeric() %>% 
                  sum() %>% 
                  scales::dollar())
# extra_from_dupes
# $16,712

# connect to local dev db
# fp_con <- src_postgres(dbname = "fame_website_development", user = "peterh")
# connect to read replica
source("~/code/R/fp_init.R")

payments <- tbl(fp_con, sql(paste(
    "SELECT id payment_id, order_id, response_code, amount, state",
    "FROM spree_payments",
    "WHERE response_code IS NOT NULL"))) %>%
    collect()


missing_from_db <- assembly_csv %>%
    filter(!is.na(response_code24) & !duplicated(response_code24)) %>%
    filter(!is.na(response_code24)) %>%
    anti_join(payments %>%
                   mutate(response_code24 = substr(response_code, 1, 24)), 
               by = "response_code24")

missing_from_db$`Purchase Amt` %>% as.numeric() %>% sum() %>% scales::dollar()

matched_payments <- assembly_csv %>%
    filter(!is.na(response_code24) & !duplicated(response_code24)) %>%
    inner_join(payments %>%
                  mutate(response_code24 = substr(response_code, 1, 24)), 
              by = "response_code24")

matched_payments %>%
    group_by(`Purchase Currency`, state) %>%
    summarise(assembly_amount = `Purchase Amt` %>% as.numeric() %>% sum %>% scales::dollar(),
              db_amount = amount %>% sum() %>% scales::dollar())
# `Purchase Currency`     state assembly_amount  db_amount
#                USD  completed      $2,715,579 $2,715,579
# Match!
matched_payments %>%
    mutate(amount_diff = abs(`Purchase Amt` %>% as.numeric() - amount)) %>%
    count(amount_diff)

matched_payments %>% count(payment_id) %>% filter(n > 1)
# No dupes!

write_csv(assembly_dupes, "~/data/Assembly Duplicates Jan to May 2017.csv", na = "")
write_csv(missing_from_db, "~/data/Assembly Missing from DB Jan to May 2017.csv", na = "")
write_csv(matched_payments, "~/data/Assembly Matched with DB Jan to May 2017.csv", na = "")