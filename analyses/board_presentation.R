# ---- QUERY DATA ----
setwd("~/code/analytics/ecommerce-performance")
source("~/code/analytics/ecommerce-performance/global.R")
setwd("~/data")      

products_shipped <- products_sold %>%
    filter(is_shipped 
           & order_state != "canceled"
           & year(ship_date) >= 2016) 

data_folder <- "~/Dropbox (Team Fame)/data/"
nps <- read_csv(paste0(data_folder,
                       "nps/delighted-data_11-September-2017.csv"),
                col_types = cols(
                    .default = col_character(),
                    `Response ID` = col_integer(),
                    Score = col_integer()))

# Estimate returns through June 2017 for 2017-08 meeting
ship_month_returns <- returns_reconciled %>%
    rename(returns = adjusted_returns) %>%
    rbind(data_frame(
        ship_year = rep(2017, 2),
        ship_quarter = rep(2, 2),
        ship_month = 5:6,
        returns = c(309963.42, 106002.97) # 90% of current return request volume
    ))

order_month_returns <- read_csv(paste0(data_folder, "finance/returns_reconciled_2017-07-26.csv")) %>%
    mutate(estimated_order_date = coalesce(last_order_date, date - 30),
           amount_usd = ifelse(currency == "AUD", abs(amount) * 0.74, abs(amount))) %>%
    left_join(products_sold %>%
                  group_by(order_id) %>%
                  summarise(manufacturing_cost = sum(manufacturing_cost)),
              by = "order_id") %>%
    group_by(order_year = year(estimated_order_date),
             order_quarter = quarter(estimated_order_date),
             order_month = month(estimated_order_date)) %>%
    summarise(adjusted_returns = sum(amount_usd),
              inventory_returns = sum(coalesce(manufacturing_cost, 70))) %>%
    filter(order_year >= 2017 & order_month <= 4) %>%
    rename(returns = adjusted_returns) %>%
    ungroup() %>%
    rbind(products_shipped %>%
              group_by(order_year = year(order_date),
                       order_quarter = quarter(order_date),
                       order_month = month(order_date)) %>%
              mutate(manufacturing_cost = coalesce(manufacturing_cost, 70)) %>%
              summarise(returns = sum(return_requested * sales_usd * 0.9),
                        inventory_returns = sum(return_requested * manufacturing_cost * 0.9)) %>%
              ungroup() %>%
              filter(order_year >= 2017 & order_month %in% c(5,6)))

plan <- read_csv(paste0(data_folder, "finance/financial_model_2017-08.csv"),
                 col_types = cols(.default = col_character())) %>%
    gather(month_name, month_value, -Metric) %>%
    spread(Metric, month_value) %>%
    select(-month_name) %>%
    mutate(Year = `Year Month` %>% as.Date() %>% year(),
           Month = `Year Month` %>% as.Date() %>% month(),
           `Month Name` = `Year Month` %>% as.Date() %>% month(label = T)) %>%
    arrange(as.Date(`Year Month`))

# ---- ANNUAL QUARTERLY AND MONTHLY KPIs ----

# Direct Gross Revenue, Gross Margin and Return Rate
finance_summary <- function(df){
    df %>%
        summarise(gross_revenue = sum(gross_revenue_usd),
                  net_sales = sum(sales_usd),
                  units_shipped = sum(quantity),
                  cogs = sum(coalesce(manufacturing_cost, 70))
                  + sum(li_shipping_cost)
                  + sum(payment_processing_cost),
                  total_adjustments = sum(adjustments_usd),
                  orders = n_distinct(order_id))
}
direct_summary <- function(df){
    df %>%
        summarise(`Direct Gross Revenue` = sum(gross_revenue),
                  `Direct Gross Margin` = sum(net_sales - returns - cogs) 
                                 / sum(net_sales - returns),
                  `Return Rate` = sum(returns) / sum(gross_revenue),
                  Orders = sum(orders),
                  AOV = sum(gross_revenue) / sum(orders),
                  `Avg. Unit Cost` = sum(cogs) / sum(units_shipped),
                  `Direct Net Sales` = sum(net_sales),
                  `Direct COGS` = sum(cogs),
                  `Direct Returns` = sum(returns))
}
# Annual
annual_direct <- products_shipped %>%
    group_by(ship_year = year(ship_date), ship_month = month(ship_date)) %>%
    finance_summary() %>%
    left_join(ship_month_returns %>% select(-ship_quarter), by = c("ship_year","ship_month")) %>%
    group_by(`Ship Year` = ship_year) %>%
    direct_summary()
# Quarterly
quarterly_direct <- products_shipped %>%
    group_by(ship_year = year(ship_date), ship_month = month(ship_date)) %>%
    finance_summary() %>%
    left_join(ship_month_returns %>% select(-ship_quarter), by = c("ship_year","ship_month")) %>%
    group_by(`Ship Year` = ship_year, 
             `Ship Quarter` = ceiling(ship_month / 3)) %>%
    direct_summary()
# Monthly
monthly_direct <- products_shipped %>%
    group_by(ship_year = year(ship_date), ship_month = month(ship_date)) %>%
    finance_summary() %>%
    left_join(ship_month_returns %>% select(-ship_quarter), by = c("ship_year","ship_month")) %>%
    group_by(`Ship Year` = ship_year,
             `Ship Month` = ship_month) %>%
    direct_summary()

# Marketing
contribution_margin <- function(df){
    df %>%
        mutate(`Contribution Margin` = (net_sales - returns + inventory_returns - cogs - `Marketing Spend`) / (net_sales - returns),
               CAC = `Marketing Spend` / orders)
}
# Monthly
monthly_marketing <- data_frame(
    # Total marketing expenses
    `Marketing Spend` = c(164993,182330,417584,555331,322961,147465,114354,87488),
    Month = 1:8,
    Year = 2017) %>%
    left_join(products_sold %>%
                  group_by(Year = year(order_date), Month = month(order_date)) %>%
                  finance_summary() %>%
                  inner_join(order_month_returns %>% 
                                 transmute(Year = order_year,
                                           Month = order_month,
                                           inventory_returns,
                                           returns),
                             by = c("Year","Month")),
              by = c("Year", "Month")) %>%
    contribution_margin()%>%
    select(Year, Month, returns, inventory_returns, `Marketing Spend`, `CAC`, `Contribution Margin`)

monthly_direct_demand <- products_sold %>%
    group_by(order_year = year(order_date), order_month = month(order_date)) %>%
    finance_summary() %>%
    rename(Year = order_year, Month = order_month) %>%
    left_join(monthly_marketing, by = c("Year","Month"))

# Annual
annual_marketing <- monthly_marketing %>%
    group_by(Year) %>%
    summarise(`Marketing Spend` = sum(`Marketing Spend`)) %>%
    left_join(products_sold %>%
                  group_by(Year = year(order_date)) %>%
                  finance_summary() %>%
                  inner_join(order_month_returns %>%
                                 group_by(Year = order_year) %>%
                                 summarise(returns = sum(returns),
                                           inventory_returns = sum(inventory_returns)),
                             by = "Year"),
              by = "Year") %>%
    contribution_margin()%>%
    select(Year, returns, inventory_returns, `Marketing Spend`, `CAC`, `Contribution Margin`)

# Quarterly
quarterly_marketing <- monthly_marketing %>%
    group_by(Year, Quarter = ceiling(Month / 3)) %>%
    summarise(`Marketing Spend` = sum(`Marketing Spend`)) %>%
    left_join(products_sold %>%
                  group_by(Year = year(order_date), Quarter = quarter(order_date)) %>%
                  finance_summary() %>%
                  inner_join(order_month_returns %>%
                                 group_by(Year = order_year,
                                          Quarter = order_quarter) %>%
                                 summarise(returns = sum(returns),
                                           inventory_returns = sum(inventory_returns)),
                             by = c("Year","Quarter")),
              by = c("Year","Quarter")) %>%
    contribution_margin()%>%
    select(Year, Quarter, returns, inventory_returns, `Marketing Spend`, `CAC`, `Contribution Margin`)
# Cohort
cohort_direct <- products_sold %>%
    filter(year(order_date) == 2017) %>%
    filter(!is.na(assigned_cohort) & assigned_cohort != "Not Assigned") %>%
    group_by(assigned_cohort) %>%
    summarise(gross_revenue = sum(gross_revenue_usd),
              net_sales = sum(sales_usd),
              units_shipped = sum(quantity),
              cogs = sum(coalesce(manufacturing_cost, 70))
              + sum(li_shipping_cost)
              + sum(payment_processing_cost),
              total_adjustments = sum(adjustments_usd),
              orders = n_distinct(order_id),
              returns = sum(coalesce(refund_amount_usd, return_requested * sales_usd * 0.3)),
              inventory_returns = sum(coalesce(manufacturing_cost, 70) * !is.na(refund_amount_usd)))

quarterly_cohorts <- products_shipped %>%
    group_by(order_year_qtr = paste(year(ship_date), quarter(ship_date)), assigned_cohort) %>%
    summarise(gross_revenue = sum(gross_revenue_usd)) %>%
    filter(!is.na(assigned_cohort)) %>%
    spread(assigned_cohort, gross_revenue, fill = 0) 

# NPS
nps_sales <- nps %>%
    rename(email = Email) %>%
    inner_join(products_shipped %>%
                   group_by(email) %>%
                   summarise(acquisition_date = min(order_date)),  
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
customer_acquisitions <- read_csv(
    "~/code/analytics/ecommerce-performance/static-data/customer_acquisitions.csv",
    col_types = cols(
        email = col_character(),
        date = col_date(format = ""))) %>%
    bind_rows(list(
        products_sold %>%
            filter(!return_requested) %>%
            group_by(email) %>%
            summarise(date = min(order_date)))) %>%
    unique() %>%
    rename(acquisition_date = date)

repeat_filterjoin <- function(df){
    df %>%
        filter(!return_requested) %>%
        left_join(customer_acquisitions, by = "email") %>%
        mutate(New_Repeat = ifelse(order_date <= coalesce(acquisition_date,
                                                          order_date), 
                                   "New Customers","Repeat Customers"))
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
# Monthly by Cohort
monthly_cohort_repeats <- products_shipped %>%
    repeat_filterjoin() %>%
    group_by(`Ship Year` = year(ship_date),
             `Ship Month` = month(ship_date),
             New_Repeat, assigned_cohort) %>%
    repeat_summary()

# ---- Avg. Make Times ----
monthly_avg_make_times <- products_shipped %>%
    group_by(Year = year(order_date), Month = month(order_date)) %>%
    summarise(avg_make_time = mean(difftime(ship_date, order_date, units = "days")))

# ---- MERGE KPIs by Year, Quarter and Month ----

# Annual
annual_kpis <- annual_direct %>%
    rename(Year = `Ship Year`) %>%
    left_join(annual_nps %>% rename(Year = acquisition_year), by = "Year") %>%
    left_join(annual_repeats %>% rename(Year = `Ship Year`), by = "Year") %>%
    left_join(annual_marketing, by = "Year")
# Quarterly
quarterly_kpis <- quarterly_direct %>%
    rename(Year = `Ship Year`, Quarter = `Ship Quarter`) %>%
    left_join(quarterly_nps %>% rename(Year = acquisition_year,
                                       Quarter = acquisition_quarter),
               by = c("Year", "Quarter")) %>%
    left_join(quarterly_repeats %>% rename(Year = `Ship Year`, 
                                           Quarter = `Ship Quarter`),
               by = c("Year","Quarter")) %>%
    left_join(quarterly_marketing, by = c("Year","Quarter"))
# Monthly
monthly_kpis <- monthly_direct %>%
    rename(Year = `Ship Year`, Month = `Ship Month`) %>%
    left_join(monthly_nps %>% rename(Year = acquisition_year,
                                     Month = acquisition_month),
              by = c("Year","Month")) %>%
    left_join(monthly_repeats %>% rename(Year = `Ship Year`, 
                                         Month = `Ship Month`),
              by = c("Year","Month")) %>%
    left_join(monthly_marketing, by = c("Year","Month")) %>%
    left_join(monthly_avg_make_times, by = c("Year","Month"))

# Cohort
cohort_direct$assigned_cohort <- as.character(cohort_direct$assigned_cohort)
cohort_assignments$assigned_cohort <- as.character(cohort_assignments$assigned_cohort)
products_shipped$assigned_cohort <- as.character(products_shipped$assigned_cohort)
cohort_kpis <- cohort_direct %>%
    left_join(plan %>% 
                  select(Year, Month, contains("Gross Revenue Retail")) %>%
                  gather(metric_name, metric_value, -Year, -Month)%>%
                  filter(Year == year(today())) %>%
                  mutate(assigned_cohort = ifelse(
                      str_detect(metric_name, "Bridal|Bridesmaid"), "Bridal",
                      ifelse(str_detect(metric_name, "Contem"), "Contemporary",
                             ifelse(str_detect(metric_name, "Prom"), "Prom", NA)))) %>%
                  filter(Month <= month(today())) %>%
                  group_by(assigned_cohort) %>%
                  summarise(gross_revenue_plan = sum(as.numeric(metric_value))),
              by = "assigned_cohort") %>%
    left_join(nps_sales %>%
                  inner_join(cohort_assignments %>%
                                 filter(assigned_cohort != "Not Assigned"),
                             by = "email") %>%
                  group_by(assigned_cohort, nps_group) %>%
                  summarise(responses = n()) %>%
                  mutate(perc_responses = responses / sum(responses)) %>%
                  select(-responses) %>%
                  spread(nps_group, perc_responses) %>%
                  ungroup() %>%
                  transmute(assigned_cohort, NPS = (Promoter - Detractor) * 100),
              by = "assigned_cohort") %>%
    left_join(products_shipped %>%
                  repeat_filterjoin() %>%
                  group_by(assigned_cohort, New_Repeat) %>%
                  repeat_summary() %>%
                  ungroup() %>%
                  select(assigned_cohort, `Repeat Rate`),
              by = "assigned_cohort") %>%
    mutate(gross_margin = (net_sales - returns - cogs*0.7)/(net_sales - returns))

# ---- Product Sales Distribution ----
active_products <- products %>% 
    filter(!hidden 
           & (is.na(deleted_at) | deleted_at > today())
           & available_on <= today())

products_sold_2017 <- products_sold %>%
    filter(year(order_date) == 2017)

no_sales_live <- active_products %>%
    anti_join(products_sold_2017, by = "product_id")

style_sales_distribution_2017 <- products_sold_2017 %>%
    group_by(product_id) %>%
    summarise(units_ordered = sum(quantity),
              return_request_units = sum(return_requested),
              net_return_request_units = units_ordered - return_request_units) %>%
    bind_rows(list(no_sales_live %>%
                       transmute(product_id, 
                                 units_ordered = 0,
                                 return_request_units = 0,
                                 net_return_request_units = 0))) %>%
    mutate(quintile = ntile(net_return_request_units, 5)) %>%
    group_by(quintile) %>%
    summarise(products = n_distinct(product_id),
              total_net_return_units = sum(net_return_request_units)) %>%
    mutate(percent_of_total_units = total_net_return_units / sum(total_net_return_units)) %>%
    arrange(desc(total_net_return_units))

monthly_style_sales_distribution_2017 <- products_sold_2017 %>%
    group_by(order_year_month, product_id) %>%
    summarise(units_ordered = sum(quantity),
              return_request_units = sum(return_requested),
              net_return_request_units = units_ordered - return_request_units) %>%
    mutate(quintile = ntile(net_return_request_units, 5)) %>%
    group_by(order_year_month, quintile) %>%
    summarise(products = n_distinct(product_id),
              total_net_return_units = sum(net_return_request_units)) %>%
    mutate(percent_of_total_units = total_net_return_units / sum(total_net_return_units)) %>%
    select(order_year_month, quintile, percent_of_total_units) %>%
    spread(quintile, percent_of_total_units) %>%
    rename(`Top 20%` = `5`, `60% to 80%` = `4`, `40% to 60%` = `3`,
           `20% to 40%` = `2`, `Bottom 20%` = `1`)

# ---- Customization Rate Trend ----
weekly_customization_rates <- products_sold %>%
    mutate(order_year_week = paste(year(order_date), 
                                   formatC(week(order_date), width = 2, flag = "0"), 
                                   sep = " W")) %>%
    group_by(order_year_week) %>% 
    summarise(`Week Ending` = as.character(max(order_date)),
              Units = sum(physically_customized * quantity) / sum(quantity)) %>%
    filter(year(`Week Ending`) == 2017)


# ---- WRITE TO CSV ----
board_folder <- "~/Dropbox (Team Fame)/data/board/output/"
write_csv(annual_kpis, paste0(board_folder, "annual_kpis.csv"), na = "")
write_csv(quarterly_kpis, paste0(board_folder, "quarterly_kpis.csv"), na = "")
write_csv(monthly_kpis, paste0(board_folder, "monthly_kpis.csv"), na = "")
write_csv(cohort_kpis, paste0(board_folder, "cohort_kpis.csv"), na = "")
write_csv(plan, paste0(board_folder, "plan.csv"), na = "")
write_csv(weekly_customization_rates, paste0(board_folder, "weekly_customizations.csv"), na = "")
write_csv(style_sales_distribution_2017, paste0(board_folder, "style_sales_distribution_2017.csv"), na = "")
write_csv(monthly_direct_demand, paste0(board_folder, "monthly_direct_demand.csv"),na = "")
write_csv(monthly_style_sales_distribution_2017, paste0(board_folder, "monthly_style_sales_distribution_2017.csv", na=""))
write_csv(quarterly_cohorts, paste0(board_folder, "quarterly_cohorts.csv"), na="")
