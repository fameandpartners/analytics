library(dplyr)
library(stringr)

# ---- SPREE ----
setwd("~/code/analytics/ecommerce-performance")
source("~/code/analytics/ecommerce-performance/global.R")
setwd("~/data")

# ---- DELIGHTED ----
nps <- read_csv("~/Dropbox (Team Fame)/data/nps/delighted-trend-total-nps_21-July-2017.csv",
                col_types = cols(
                    .default = col_character(),
                    `Response ID` = col_integer(),
                    Score = col_integer()))

# ---- Company Health Check ----
products_shipped <- products_sold %>%
    inner_join(customer_aquisitions %>%
                   rename(acquisition_date = date), 
               by = "email") %>%
    filter(is_shipped & order_state != "canceled" & year(ship_date) >= 2016)

# Direct Gross Revenue and Margin
# Annual
gross_annual <- monthly_actuals_2017 %>%
    group_by(`Ship Year` = ship_year) %>%
    summarise(`Gross Revenue` = sum(gross_revenue),
              `Gross Margin` = sum(net_sales - returns - cogs) 
                             / sum(net_sales - returns))
# Quarterly
gross_quarterly <- monthly_actuals_2017 %>%
    group_by(`Ship Year` = ship_year, 
             `Ship Quarter` = ceiling(ship_month / 3)) %>%
    summarise(`Gross Revenue` = sum(gross_revenue),
              `Gross Margin` = sum(net_sales - returns - cogs) 
                             / sum(net_sales - returns))
# Monthly
monthly_actuals_2017 %>%
    ungroup() %>%
    transmute(`Ship Year` = ship_year, `Ship Month` = ship_month,
              `Gross Revenue` = gross_revenue, `Gross Margin` = gross_margin)

# NPS
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
# Annual
monthly_nps_responses %>%
    group_by(acquisition_year) %>%
    nps_summary()
# Quarterly
monthly_nps_responses %>%
    group_by(acquisition_year, acquisition_quarter) %>%
    nps_summary()
# Monthly
monthly_nps_responses %>%
    group_by(acquisition_year, acquisition_quarter, acquisition_month) %>%
    nps_summary()

# Repeat Rate
customer_aquisitions <- customer_aquisitions %>%
    rbind(products_shipped %>% 
              filter(!return_requested) %>%
              group_by(email) %>%
              summarise(date = min(order_date))) %>%
    unique()
repeat_filterjoin <- function(df){
    df %>%
        filter(!return_requested) %>%
        inner_join(customer_aquisitions %>%
                       rename(acquisition_date = date),
                   by = "email")
}
repeat_summary <- function(df){
    df %>%
        summarise(Customers = n_distinct(email)) %>%
        spread(New_Repeat, Customers) %>%
        mutate(`Repeat Rate` = `Repeat Customers` 
                             / (`New Customers` + `Repeat Customers`))
}

# Annual
products_shipped %>%
    repeat_filterjoin() %>%
    group_by(`Ship Year` = year(ship_date),
             New_Repeat = ifelse(order_date <= acquisition_date, 
                                 "New Customers","Repeat Customers")) %>%
    repeat_summary()
    
# Quarterly
products_shipped %>%
    repeat_filterjoin() %>%
    group_by(`Ship Year` = year(ship_date),
             `Ship Quarter` = quarter(ship_date),
             New_Repeat = ifelse(order_date <= acquisition_date, 
                                 "New Customers","Repeat Customers")) %>%
    repeat_summary()
# Monthly
products_shipped %>%
    repeat_filterjoin() %>%
    group_by(`Ship Year` = year(ship_date),
             `Ship Quarter` = quarter(ship_date),
             `Ship Month` = month(ship_date),
             New_Repeat = ifelse(order_date <= acquisition_date, 
                                 "New Customers","Repeat Customers")) %>%
    repeat_summary()
# Quarterly Retention
products_shipped %>%
    filter(year(acquisition_date) >= 2016 & !return_requested) %>%
    group_by(`Acquisition Year Quarter` = paste(year(acquisition_date), 
                                                quarter(acquisition_date)),
             ship_yq = paste(year(ship_date), quarter(ship_date))) %>%
    summarise(Customers = n_distinct(email)) %>%
    mutate(qtrs_since_acquisition = dense_rank(ship_yq) - 1,
           retention = Customers / max(Customers)) %>%
    select(-ship_yq, -Customers) %>%
    spread(qtrs_since_acquisition, retention, fill = 0)