suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(readr))
suppressMessages(library(lubridate))

dw <- src_postgres(dbname = "dw_dev",host = "localhost")

products_sold <- tbl(dw, "sales") %>% 
    filter(order_date >= "2015-12-15") %>%
    collect() %>%
    mutate(packaging_cost = 2.5)

products_sold$Cohort <- products_sold$assigned_cohort %>%
    as.character() %>%
    coalesce("Not Assigned")

# ---- 3PL ----
tpl <- read_csv("Rscripts/static-data/3PL Orders - COMBINED.csv",
                col_types = cols(
                    order_number = col_character(),
                    ship_date = col_date(format = "")
                ))

# ---- DAILY KPIs ----

confirmed_sales <- products_sold %>%
    filter(order_state != "canceled") %>%
    mutate(refulfilled = order_number %in% tpl$order_number)

daily_direct_demand <- confirmed_sales %>%
    group_by(order_date) %>%
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
    filter(year(order_date) >= 2016)

# Repeat Rate
customer_acquisitions <- tbl(dw, "sales") %>%
    group_by(email) %>%
    summarise(acquisition_date = min(order_date)) %>%
    collect()

# Daily
daily_repeats <- confirmed_sales %>%
    filter(!return_requested) %>%
    left_join(customer_acquisitions, by = "email") %>%
    mutate(new_repeat = ifelse(order_date <= coalesce(acquisition_date,
                                                      order_date), 
                               "New Customers","Repeat Customers")) %>%
    group_by(order_date, new_repeat) %>%
    summarise(Customers = n_distinct(email)) %>%
    spread(new_repeat, Customers, fill = 0) %>%
    filter(year(order_date) >= 2016) %>%
    ungroup()

# ---- DEMAND BASED RETURNS ----
reconciled_returns <- read_csv("Rscripts/static-data/returns_reconciled_2017-07-26.csv",
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
daily_demand_returns <- reconciled_returns %>%
    mutate(estimated_order_date = coalesce(last_order_date, date - 30),
           amount_usd = ifelse(currency == "AUD", abs(amount) * 0.74, abs(amount))) %>%
    left_join(products_sold %>%
                  group_by(order_id) %>%
                  summarise(manufacturing_cost = mean(coalesce(manufacturing_cost, 70))),
              by = "order_id") %>%
    mutate(refulfilled = order_number %in% tpl$order_number) %>%
    group_by(order_date = estimated_order_date) %>%
    summarise(adjusted_returns = sum(amount_usd),
              inventory_returns = sum(coalesce(manufacturing_cost, 70)),
              refulfilled_return_units = sum(refulfilled)) %>%
    filter(year(order_date) >= 2017 & month(order_date) <= 4) %>%
    rename(returns = adjusted_returns) %>%
    ungroup() %>%
    rbind(confirmed_sales %>%
              group_by(order_date) %>%
              mutate(manufacturing_cost = coalesce(manufacturing_cost, 70)) %>%
              summarise(returns = sum(return_requested * sales_usd * 0.9),
                        inventory_returns = sum(return_requested * coalesce(manufacturing_cost, 70) * 0.9),
                        refulfilled_return_units = 0) %>%
              ungroup() %>%
              filter(year(order_date) >= 2017 & month(order_date) %in% c(5,6))) %>%
    filter(order_date <= today()) %>%
    rename(Returns = returns, 
           `Inventory Returns` = inventory_returns,
           `Refulfilled Return Units` = refulfilled_return_units)

# ---- MERGE DAILY KPIs ----
daily_direct <- daily_direct_demand %>%
    left_join(daily_repeats, by = "order_date") %>%
    left_join(daily_demand_returns, by = "order_date") %>%
    rename(Date = order_date) %>%
    replace(is.na(.), 0)

static_data <- "Rscripts/static-data/"
write_csv(daily_direct, paste0(static_data, "daily_direct.csv"))
