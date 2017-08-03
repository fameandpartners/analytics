library(dplyr)
library(stringr)

# ---- QUERY DATA ----
setwd("~/code/analytics/ecommerce-performance")
source("~/code/analytics/ecommerce-performance/global.R")
setwd("~/data")

data_folder <- "~/Dropbox (Team Fame)/data/"
nps <- read_csv(paste0(data_folder,
                       "nps/delighted-trend-total-nps_21-July-2017.csv"),
                col_types = cols(
                    .default = col_character(),
                    `Response ID` = col_integer(),
                    Score = col_integer()))

all_refunds <- read_csv(paste0(data_folder, 
                               "finance/returns_reconciled_2017-07-26.csv"))

wholesale <- read_csv(paste0(data_folder, "wholesale/wholesale_revenue.csv"),
                      col_types = cols(
                          .default = col_number(),
                          Customer = col_character(),
                          Metric = col_character())) %>%
    gather(month_abbr, value, -Customer, -Metric) %>%
    filter(value > 0) %>%
    spread(Metric, value) %>%
    inner_join(data_frame(
            month_abbr = c('Jan','Feb','Mar','Apr','May','Jun',
                           'Jul','Aug','Sep','Oct','Nov','Dec'),
            Month = 1:12,
            Year = rep(2017, 12)
        ),
        by = "month_abbr") %>%
    select(-month_abbr)

# Marketing
# Monthly
monthly_marketing <- data_frame(
    `Marketing Spend` = c(78749.1737971831,69829.4937352113,325853.681397183,
              408186.17028169,251748.655515493,60422.3189521127,
              35733.1414129941),
    Month = 1:7,
    Year = 2017)
# Annual
annual_marketing <- monthly_marketing %>%
    group_by(Year) %>%
    summarise(`Marketing Spend` = sum(`Marketing Spend`))
# Quarterly
quarterly_marketing <- monthly_marketing %>%
    group_by(Year, Quarter = ceiling(Month / 3)) %>%
    summarise(`Marketing Spend` = sum(`Marketing Spend`))

order_refunds <- all_refunds %>%
    group_by(order_id) %>%
    summarise(amount = sum(abs(amount)))

item_refunds <- products_sold %>%
    inner_join(order_refunds, by = "order_id") %>%
    group_by(order_id) %>%
    mutate(items = sum(quantity)) %>%
    ungroup() %>%
    transmute(line_item_id, 
              reconciled_returns_usd = conversion_rate * (amount / items))

# ---- ANNUAL QUARTERLY AND MONTHLY KPIs ----
products_shipped <- products_sold %>%
    left_join(item_refunds, by = "line_item_id") %>%
    filter(is_shipped 
           & (order_state != "canceled" | !is.na(reconciled_returns_usd))
           & year(ship_date) >= 2016) %>%
    rename(spree_returns = refund_amount_usd) %>%
    mutate(refund_amount_usd = coalesce(reconciled_returns_usd, spree_returns))

# Direct Gross Revenue, Gross Margin and Return Rate
finance_summary <- function(df){
    df %>%
        summarise(gross_revenue = sum(gross_revenue_usd),
                  net_sales = sum(sales_usd),
                  units_shipped = sum(quantity),
                  cogs = sum(coalesce(manufacturing_cost, 70))
                  + sum(li_shipping_cost)
                  + sum(payment_processing_cost),
                  returns = sum(coalesce(refund_amount_usd, 0)),
                  total_adjustments = sum(adjustments_usd),
                  orders = n_distinct(order_id),
                  work_days = sum(difftime(ship_date, order_date, "days")))
}
direct_summary <- function(df){
    df %>%
        summarise(`Direct Gross Revenue` = sum(gross_revenue),
                  `Direct Gross Margin` = sum(net_sales - returns - cogs) 
                                 / sum(net_sales - returns),
                  `Return Rate` = sum(returns) / sum(net_sales),
                  Orders = sum(orders),
                  AOV = sum(gross_revenue) / sum(orders),
                  `Avg. Unit Cost` = sum(cogs) / sum(units_shipped),
                  `Avg. Days to Ship` = sum(work_days) / sum(units_shipped),
                  `Direct Net Sales` = sum(net_sales),
                  `Direct COGS` = sum(cogs),
                  `Direct Returns` = sum(returns))
}
# Annual
annual_direct <- products_shipped %>%
    group_by(ship_year = year(ship_date), ship_month = month(ship_date)) %>%
    finance_summary() %>%
    group_by(`Ship Year` = ship_year) %>%
    direct_summary()
# Quarterly
quarterly_direct <- products_shipped %>%
    group_by(ship_year = year(ship_date), ship_month = month(ship_date)) %>%
    finance_summary() %>%
    group_by(`Ship Year` = ship_year, 
             `Ship Quarter` = ceiling(ship_month / 3)) %>%
    direct_summary()
# Monthly
monthly_direct <- products_shipped %>%
    group_by(ship_year = year(ship_date), ship_month = month(ship_date)) %>%
    finance_summary() %>%
    group_by(`Ship Year` = ship_year,
             `Ship Month` = ship_month) %>%
    direct_summary()
# Monthly by Cohort
cohort_direct <- products_shipped %>%
    filter(!is.na(assigned_cohort) & assigned_cohort != "Not Assigned") %>%
    group_by(assigned_cohort) %>%
    finance_summary() 

# NPS
nps_sales <- nps %>%
    rename(email = Email) %>%
    inner_join(customer_aquisitions %>%
                   bind_rows(list(
                       products_sold %>%
                           group_by(email) %>%
                           summarise(date = min(order_date)))) %>%
                   unique() %>%
                   rename(acquisition_date = date), 
               by = "email") %>%
    mutate(nps_group = ifelse(Score < 7, "Detractor",
                              ifelse(Score < 9, "Passive", "Promoter")))

monthly_nps_responses <- nps_sales %>%
    group_by(acquisition_year = year(acquisition_date),
             acquisition_quarter = quarter(acquisition_date),
             acquisition_month = month(acquisition_date),
             nps_group) %>%
    summarise(responses = n_distinct(email)) %>%
    spread(nps_group, responses, fill = 0)

nps_summary <- function(df){
    df %>%
        summarise(`NPS Responses` = sum(Promoter + Passive + Detractor),
                  `NPS Score` = 100 * (sum(Promoter) 
                                      / sum(Promoter + Passive + Detractor))
                              - 100 * (sum(Detractor) 
                                      / sum(Promoter + Passive + Detractor))) %>%
        filter(`NPS Responses` > 50)
}
# Annual
annual_nps <- monthly_nps_responses %>%
    group_by(acquisition_year) %>%
    nps_summary()
# Quarterly
quarterly_nps <- monthly_nps_responses %>%
    group_by(acquisition_year, acquisition_quarter) %>%
    nps_summary()
# Monthly
monthly_nps <- monthly_nps_responses %>%
    group_by(acquisition_year, acquisition_month) %>%
    nps_summary()

# Repeat Rate
repeat_filterjoin <- function(df){
    df %>%
        filter(!return_requested) %>%
        mutate(New_Repeat = ifelse(order_date <= acquisition_date, 
                                   "New Customers","Repeat Customers")) %>%
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
annual_repeats <- products_shipped %>%
    repeat_filterjoin() %>%
    group_by(`Ship Year` = year(ship_date), New_Repeat) %>%
    repeat_summary()
# Quarterly
quarterly_repeats <- products_shipped %>%
    repeat_filterjoin() %>%
    group_by(`Ship Year` = year(ship_date),
             `Ship Quarter` = quarter(ship_date),
             New_Repeat) %>%
    repeat_summary()
# Monthly
monthly_repeats <- products_shipped %>%
    repeat_filterjoin() %>%
    group_by(`Ship Year` = year(ship_date),
             `Ship Month` = month(ship_date),
             New_Repeat) %>%
    repeat_summary()

# Wholesale
wholesale_summary <- function(df){
    df %>%
        summarise(`Wholesale Gross Revenue` = sum(revenue))
}
# Annual
wholesale_annual <- wholesale %>%
    group_by(Year) %>%
    wholesale_summary()
# Quarterly
wholesale_quarterly <- wholesale %>%
    group_by(Year, Quarter = ceiling(Month / 3)) %>%
    wholesale_summary()
# Monthly
wholesale_monthly <- wholesale %>%
    group_by(Year, Month) %>%
    wholesale_summary()

# ---- MERGE KPIs by Year, Quarter and Month ----
contribution_margin <- function(df){
    df %>%
        mutate(`Contribution Margin` = (`Direct Net Sales` - `Direct Returns` - `Direct COGS` - `Marketing Spend`)
                                     / (`Direct Net Sales` - `Direct Returns`))
}
# Annual
annual_kpis <- annual_direct %>%
    rename(Year = `Ship Year`) %>%
    left_join(annual_nps %>% rename(Year = acquisition_year), by = "Year") %>%
    left_join(annual_repeats %>% rename(Year = `Ship Year`), by = "Year") %>%
    left_join(wholesale_annual, by = "Year") %>%
    left_join(annual_marketing, by = "Year") %>%
    contribution_margin()
# Quarterly
quarterly_kpis <- quarterly_direct %>%
    rename(Year = `Ship Year`, Quarter = `Ship Quarter`) %>%
    left_join(quarterly_nps %>% rename(Year = acquisition_year,
                                       Quarter = acquisition_quarter),
               by = c("Year", "Quarter")) %>%
    left_join(quarterly_repeats %>% rename(Year = `Ship Year`, 
                                           Quarter = `Ship Quarter`),
               by = c("Year","Quarter")) %>%
    left_join(wholesale_quarterly, by = c("Year","Quarter")) %>%
    left_join(quarterly_marketing, by = c("Year","Quarter")) %>%
    contribution_margin()
# Monthly
monthly_kpis <- monthly_direct %>%
    rename(Year = `Ship Year`, Month = `Ship Month`) %>%
    left_join(monthly_nps %>% rename(Year = acquisition_year,
                                     Month = acquisition_month),
              by = c("Year","Month")) %>%
    left_join(monthly_repeats %>% rename(Year = `Ship Year`, 
                                         Month = `Ship Month`),
              by = c("Year","Month")) %>%
    left_join(wholesale_monthly, by = c("Year","Month")) %>%
    left_join(monthly_marketing, by = c("Year","Month")) %>%
    contribution_margin()
# Cohort
cohort_kpis <- cohort_direct

# ---- WRITE TO CSV ----
board_folder <- "~/Dropbox (Team Fame)/data/board/"
write_csv(annual_kpis, paste0(board_folder, "annual_kpis.csv"), na = "")
write_csv(quarterly_kpis, paste0(board_folder, "quarterly_kpis.csv"), na = "")
write_csv(monthly_kpis, paste0(board_folder, "monthly_kpis.csv"), na = "")
write_csv(cohort_kpis, paste0(board_folder, "cohort_kpis.csv"), na = "")
write_csv(wholesale, paste0(board_folder, "wholesale_monthly_accounts.csv", na = ""))

# ---- VISUALIZATIONS ---
