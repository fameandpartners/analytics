library(dplyr)
library(stringr)

# ---- SPREE ----
setwd("~/code/analytics/ecommerce-performance")
source("~/code/analytics/ecommerce-performance/global.R")
setwd("~/data")

# ---- NPS ----
nps <- read_csv("~/Dropbox (Team Fame)/data/nps/delighted-trend-total-nps_21-July-2017.csv",
                col_types = cols(
                    .default = col_character(),
                    `Response ID` = col_integer(),
                    Score = col_integer()))

# ---- Company Health Check ----
monthly_actuals_2017

nps_sales <- nps %>%
    rename(email = Email) %>%
    inner_join(customer_aquisitions %>%
                   bind_rows(list(
                       products_sold %>%
                           group_by(email) %>%
                           summarise(date = min(order_date)))) %>%
                   unique() %>%
                   rename(aquisition_date = date), 
               by = "email") %>%
    mutate(nps_group = ifelse(Score < 7, "Detractor",
                              ifelse(Score < 9, "Passive", "Promoter")))

monthly_nps_responses <- nps_sales %>%
    group_by(acquisition_year = year(aquisition_date),
             acquisition_quarter = quarter(aquisition_date),
             acquisition_month = month(aquisition_date),
             nps_group) %>%
    summarise(responses = n_distinct(email)) %>%
    spread(nps_group, responses, fill = 0)

nps_summary <- function(df){
    df %>%
        summarise(total_responses = sum(Promoter + Passive + Detractor),
                  nps_score = 100 * (sum(Promoter) 
                                     / sum(Promoter + Passive + Detractor))
                            - 100 * (sum(Detractor) 
                                     / sum(Promoter + Passive + Detractor))) %>%
        filter(total_responses > 50)
}

# Annual NPS
monthly_nps_responses %>%
    group_by(acquisition_year) %>%
    nps_summary()

monthly_nps_responses %>%
    group_by(acquisition_year, acquisition_quarter) %>%
    nps_summary()

monthly_nps_responses %>%
    group_by(acquisition_year, acquisition_quarter, acquisition_month) %>%
    nps_summary()