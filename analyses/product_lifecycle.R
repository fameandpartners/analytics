source("~/code/analytics/etl/full_global.R")
setwd("~/data")

# ---- YoY Weekly Sales ----
products_sold %>%
    group_by(order_year = year(order_date) %>% as.character(), order_week = week(order_date)) %>%
    summarise(Units = n()) %>%
    ggplot(aes(x = order_week)) +
    geom_line(aes(y = Units, color = order_year), group = 4) +
    scale_x_continuous(limits = c(1, 52))
# ---- ^ LOOK AT AFTER THIS WEEK ENDS ^ -----

top_10_dresses_by_quarter <- products_sold %>%
    group_by(style_number, style_name, order_date) %>%
    summarise(gross_revenue_usd = sum(gross_revenue_usd),
              sales_usd = sum(sales_usd),
              promotions_usd = sum(coalesce(promotions_usd, 0)),
              price_usd = sum(price_usd),
              units_ordered = sum(quantity)) %>%
    ungroup() %>%
    group_by(order_year = year(order_date), 
             order_quarter = quarter(order_date), 
             style_name) %>%
    summarise(net_sales = sum(sales_usd)) %>%
    arrange(order_year, order_quarter, desc(net_sales)) %>%
    top_n(10, net_sales)

daily_product_sales <- products_sold %>%
    filter(product_live == "Yes") %>%
    group_by(style_number, style_name, order_date) %>%
    summarise(gross_revenue_usd = sum(gross_revenue_usd),
              sales_usd = sum(sales_usd),
              promotions_usd = sum(coalesce(promotions_usd, 0)),
              price_usd = sum(price_usd),
              units_ordered = sum(quantity)) %>%
    ungroup()

monthly_product_sales <- daily_product_sales %>%
    group_by(style_number, 
             style_name,
             order_year = year(order_date),
             order_month = month(order_date)) %>%
    summarise(monthly_units = sum(units_ordered)) %>%
    ungroup()

monthly_product_sales %>%
    group_by(style_number, style_name) %>%
    summarise(best_month_units = max(monthly_units),
              worst_month_units = min(monthly_units),
              monthly_avg = mean(monthly_units),
              monthly_med = median(monthly_units),
              monthly_sd = sd(monthly_units),
              total_units_sold = sum(monthly_units)) %>%
    inner_join(products_sold %>%
                   group_by(style_number) %>%
                   summarise(first_sale_date = min(order_date)),
               by = "style_number") %>%
    mutate(days_on_sale = difftime(today(), first_sale_date, "days"))