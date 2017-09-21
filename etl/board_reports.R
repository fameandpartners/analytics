# Short term solution to get the 2017-09 board meeting reports done for Nyree
setwd("~/code/analytics/ecommerce-performance")
source("~/code/analytics/ecommerce-performance/global.R")
setwd("~/data")      

products_sold$Cohort <- products_sold$assigned_cohort %>%
    as.character() %>%
    coalesce("Not Assigned")

# ---- 3PL ----
tpl <- read_csv("/Users/Peter 1/Dropbox (Team Fame)/data/3PL/3PL Orders - COMBINED.csv",
                col_types = cols(
                    order_number = col_character(),
                    ship_date = col_date(format = "")
                ))

# ---- MONTHLY KPIs ----

products_shipped <- products_sold %>%
    filter(order_state != "canceled") %>%
    mutate(refulfilled = order_number %in% tpl$order_number)

monthly_direct_demand <- products_shipped %>%
    group_by(Year = year(order_date), 
             Month = month(order_date),
             Cohort) %>%
    summarise(`Gross Revenue` = sum(gross_revenue_usd),
              `Net Sales` = sum(sales_usd),
              Orders = n_distinct(order_id),
              `Units` = sum(quantity),
              `Customized Units` = sum(coalesce(as.double(customized * quantity), 0)),
              `Re-fulfilled Units` = sum(coalesce(refulfilled*quantity), 0),
              COGS = sum(coalesce(manufacturing_cost, 70))
                    + sum(coalesce(li_shipping_cost, 20))
                    + sum(payment_processing_cost),
              `Packaging Materials` = sum(packaging_cost),
              `Product Cost` = sum(coalesce(manufacturing_cost, 70)),
              Discounts = sum(coalesce(promotions_usd, 0)),
              Shipping = sum(coalesce(li_shipping_cost, 20)),
              Taxes = sum(coalesce(taxes_usd, 0)),
              `Other Adjustments` = sum(coalesce(other_adjustments_usd, 0)),
              Transactions = n_distinct(order_id)) %>%
    filter(Year >= 2016)

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

# Monthly
monthly_repeats <- products_shipped %>%
    filter(!return_requested) %>%
    left_join(customer_acquisitions, by = "email") %>%
    mutate(new_repeat = ifelse(order_date <= coalesce(acquisition_date,
                                                      order_date), 
                               "New Customers","Repeat Customers")) %>%
    group_by(`Year` = year(order_date),
             `Month` = month(order_date),
             Cohort,
             new_repeat) %>%
    summarise(Customers = n_distinct(email)) %>%
    spread(new_repeat, Customers, fill = 0) %>%
    filter(Year >= 2016)

# ---- Monthly Factory Direct ----
monthly_factory_direct <- products_shipped %>%
    filter(year(order_date) >= 2017) %>%
    group_by(year_month = order_year_month,
             Factory = factory_name) %>%
    summarise(Units = sum(quantity),
              `Avg. Make Time` = mean(difftime(ship_date, 
                                               order_date, 
                                               units = "days")) %>%
                  as.numeric())

# ---- MERGE KPIs by Year, Quarter and Month ----

monthly_direct <- monthly_direct_demand %>%
    left_join(monthly_repeats, by = c("Year","Month","Cohort")) %>%
    ungroup() %>%
    mutate(year_month = paste(Year, formatC(Month, width=2, flag="0"), sep="-")) %>%
    select(-Year, -Month)

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


# ---- DEMAND BASED RETURNS ----
reconciled_returns <- read_csv("/Users/Peter 1/Dropbox (Team Fame)/data/finance/returns_reconciled_2017-07-26.csv",
                               col_types = cols(
                                   response_code = col_character(),
                                   amount = col_double(),
                                   currency = col_character(),
                                   date = col_date(format = ""),
                                   order_id = col_integer(),
                                   response_code_source = col_character(),
                                   refund_requested = col_integer(),
                                   refund_processed = col_integer(),
                                   last_order_date = col_date(format = ""),
                                   last_ship_date = col_date(format = ""),
                                   original_sales_amount = col_double(),
                                   db_refund_amount = col_double(),
                                   estimated_ship_date = col_date(format = ""),
                                   match_status = col_character(),
                                   payment_processor = col_character()
                               ))
demand_returns <- reconciled_returns %>%
    mutate(estimated_order_date = coalesce(last_order_date, date - 30),
           amount_usd = ifelse(currency == "AUD", abs(amount) * 0.74, abs(amount))) %>%
    left_join(products_sold %>%
                  group_by(order_id,order_number) %>%
                  summarise(manufacturing_cost = mean(coalesce(manufacturing_cost, 70)),
                            Cohort = Cohort[1]),
              by = "order_id") %>%
    mutate(refulfilled = order_number %in% tpl$order_number) %>%
    group_by(order_year = year(estimated_order_date),
             order_month = month(estimated_order_date),
             Cohort = coalesce(Cohort, "Not Assigned")) %>%
    summarise(adjusted_returns = sum(amount_usd),
              inventory_returns = sum(coalesce(manufacturing_cost, 70)),
              refulfilled_return_units = sum(refulfilled)) %>%
    filter(order_year >= 2017 & order_month <= 4) %>%
    rename(returns = adjusted_returns) %>%
    ungroup() %>%
    rbind(products_shipped %>%
              group_by(order_year = year(order_date),
                       order_month = month(order_date),
                       Cohort = coalesce(Cohort, "Not Assigned")) %>%
              mutate(manufacturing_cost = coalesce(manufacturing_cost, 70)) %>%
              summarise(returns = sum(return_requested * sales_usd * 0.9),
                        inventory_returns = sum(return_requested * coalesce(manufacturing_cost, 70) * 0.9),
                        refulfilled_return_units = 0) %>%
              ungroup() %>%
              filter(order_year >= 2017 & order_month %in% c(5,6))) %>%
    mutate(year_month = paste(order_year, 
                              formatC(order_month, width=2, flag="0"),
                              sep = "-")) %>%
    select(-order_year,-order_month)

# ---- Return Reasons ----
return_reasons <- products_shipped %>%
    filter(year(order_date) == 2017) %>%
    filter(!is.na(return_reason) & return_reason != "No reason") %>%
    group_by(`Return Reason` = return_reason) %>%
    summarise(Units = n())

# ---- Customization Rate Trend ----
customization_trend <- products_sold %>%
    filter(year(order_date) >= 2017) %>%
    mutate(order_year_week = paste(year(order_date), 
                               formatC(week(order_date), width = 2, flag = "0"), 
                               sep = " W")) %>%
    group_by(order_year_week) %>% 
    summarise(`Week Ending` = max(order_date),
              `Customization Rate` = sum(physically_customized * quantity) / sum(quantity)) %>%
    select(-order_year_week)


board_inputs <- "/Users/Peter 1/Dropbox (Team Fame)/data/board/inputs/"
write_csv(monthly_direct, paste0(board_inputs,"monthly_direct.csv"), na="")
write_csv(monthly_factory_direct, paste0(board_inputs, "monthly_factory_direct.csv", na=""))
write_csv(customer_acquisitions, paste0(board_inputs,"customer_acquisitions.csv", na=""))
write_csv(cohort_assignments %>% filter(assigned_cohort != "Not Assigned"),
          paste0(board_inputs, "cohort_assignments.csv", na=""))
write_csv(monthly_style_sales_distribution_2017, paste0(board_inputs,"monthly_style_distribution.csv"),na="")
write_csv(demand_returns, paste0(board_inputs, "reconciled_demand_returns.csv"))
write_csv(return_reasons, paste0(board_inputs, "return_reasons.csv"))
write_csv(customization_trend, paste0(board_inputs, "customization_trend.csv"))
