source("~/Documents/Code/R/fp_init.R")

tbl(fp_con, sql(paste(
    "SELECT COALESCE(c.has_cust, 0) has_customizations, COUNT(*)",
    "FROM spree_orders o",
    "INNER JOIN spree_line_items li",
        "ON li.order_id = o.id",
    "INNER JOIN spree_variants v",
        "ON v.id = li.variant_id",
    "INNER JOIN spree_products p",
        "ON p.id = v.product_id",
    "LEFT JOIN (",
        "SELECT DISTINCT product_id, 1 has_cust",
        "FROM customisation_values",
    ") c ON c.product_id = p.id",
    "WHERE o.completed_at IS NOT NULL",
        "AND o.completed_at >= '2016-01-01'",
        "AND o.payment_state = 'paid'",
        "AND o.total > 0",
    "GROUP BY has_customizations"))) %>%
    collect() %>%
    mutate(count / sum(count))
# 0.995072897
# 99.5% of dresses sold can be customized


setwd("~/Documents/Code/analytics/ecommerce-performance")
start_time <- now()
source("~/Documents/Code/analytics/ecommerce-performance/global.R")
now() - start_time
setwd("~/Documents/Data")



# Almost identical Gross Revenue figures
# Customized dresses are no more profitable than non-customized dresses

source("~/Documents/Code/R/customization_value parsing.R")

customized_sales <- products_sold %>%
    filter(year(order_date) == 2016) %>%
    left_join(line_item_customizations, by = "line_item_id") %>%
    mutate(customization_price_usd = customization_price * ifelse(currency == "AUD", 0.76, 1)) %>%
    group_by(cust_code = ifelse(physically_customized == 1, "Customized","Not Customized"))

customized_sales %>%
    summarise(Units = n(),
              `Average Gross Revenue` = mean(gross_revenue_usd),
              `Average Retail Price` = mean(price_usd),
              `Average Customization Price` = mean(coalesce(customization_price_usd, 0))) %>%
    ungroup() %>%
    gather(metric, value, -cust_code) %>%
    spread(cust_code, value) %>%
    mutate(Difference = Customized - `Not Customized`) %>%
    write_csv("Customization Averages.csv")

customized_sales %>%
    summarise(`Gross Revenue` = sum(gross_revenue_usd),
              `Manufacturing Cost` = sum(coalesce(manufacturing_cost, 70)),
              `Net Manufacturing Cost` = `Gross Revenue` - `Manufacturing Cost`,
              `Margin` = `Net Manufacturing Cost` / `Gross Revenue`) %>%
    ungroup() %>%
    gather(metric, value, -cust_code) %>%
    spread(cust_code, value) %>%
    write_csv("Customization Margins.csv")

# Paid customizations

paid_customization_sales <- products_sold %>%
    filter(year(order_date) >= 2016) %>%
    left_join(line_item_customizations, by = "line_item_id") %>%
    mutate(customization_price_usd = customization_price * ifelse(currency == "AUD", 0.76, 1)) %>%
    filter(customization_price > 0 | physically_customized == 0) %>%
    group_by(order_year = year(order_date),
             cust_code = ifelse(physically_customized == 1 & coalesce(customization_price_usd, 0) > 0, 
                                "Customized","Not Customized"))
paid_customization_sales %>%
    summarise(Units = n(),
              `Average Gross Revenue` = mean(gross_revenue_usd),
              `Average Retail Price` = mean(price_usd),
              `Average Customization Price` = mean(coalesce(customization_price_usd, 0))) %>%
    ungroup() %>%
    gather(metric, value, -cust_code, -order_year) %>%
    spread(cust_code, value) %>%
    mutate(Difference = Customized - `Not Customized`) %>%
    write_csv("Paid Customization Averages.csv")

paid_customization_sales %>%
    summarise(`Total Gross Revenue` = sum(gross_revenue_usd),
              `Total Sales` = sum(sales_usd),
              `Customization Revenue` = sum(coalesce(customization_price_usd, 0)),
              `Total Units` = n(),
              `Manufacturing Cost` = sum(coalesce(manufacturing_cost, 70)),
              `Net Manufacturing Cost` = `Total Sales` - `Manufacturing Cost`,
              `Net Manufacturing Margin` = `Net Manufacturing Cost` / `Total Sales`,
              `Per Unit Cost` = `Manufacturing Cost` / n()) %>%
    ungroup() %>%
    gather(metric, value, -cust_code, -order_year) %>%
    mutate(rounded_value = round(value, 4)) %>%
    select(-value) %>%
    spread(cust_code, rounded_value) %>%
    write_csv("Paid Customization Margins.csv")

products_sold %>%
    inner_join(line_item_customizations, by = "line_item_id") %>%
    count(style_name, customization) %>%
    mutate(percent_of_customizations = n / sum(n)) %>%
    View("Customization Distrobution by Style")

# Customizations don't really have a material impact on our
# business' performance.

# in 2017
customized_sales <- products_sold %>%
    filter(year(order_date) == 2017) %>%
    left_join(line_item_customizations, by = "line_item_id") %>%
    mutate(customization_price_usd = customization_price * ifelse(currency == "AUD", 0.76, 1)) %>%
    group_by(cust_code = ifelse(physically_customized == 1, "Customized","Not Customized"))

products_sold %>% 
    left_join(customization_values %>%
                  rename(customization_price = price), 
              by = "product_id") %>%
    filter(!is.na(customization_price)) %>% 
    group_by(style_number) %>%
    summarise(min_customization_price = min(customization_price),
              max_customization_price = max(customization_price),
              customizations_available = n_distinct(presentation),
              last_sales_date = max(order_date)) %>%
    summarise(at_least_1_free_customization = sum(min_customization_price == 0),
              all_customizations_free = sum(max_customization_price == 0),
              avg_number_of_customizations = mean(customizations_available),
              total_styles = n())
# 26.5% of custom styles have free customizations

products_sold %>% 
    left_join(customization_values %>%
                  rename(customization_price = price), 
              by = "product_id") %>%
    filter(!is.na(customization_price) & physically_customized) %>% 
    count(customization_price) %>% 
    mutate(percent(n / sum(n)))
# 40% of customized dress sales have free customizations

products_sold %>% filter(year(order_date) >= 2016 & order_date < as.Date("2017-04-01")) %>%
    left_join(line_item_customizations, by = "line_item_id") %>%
    mutate(customization_price_usd = customization_price * ifelse(currency == "AUD", 0.76, 1)) %>%
    group_by(`Order Month` = order_year_month) %>%
    summarise(`Customization Rate` = sum(physically_customized) / n(),
              `Paid Customization Rate` = sum(physically_customized & coalesce(customization_price_usd, 0) > 0)/n()) %>%
    ggplot(aes(x = `Order Month`)) +
    geom_line(aes(y = `Customization Rate`, color = "Customization Rate"), group = 1) +
    geom_line(aes(y = `Paid Customization Rate`, color = "Paid Customization Rate"), group = 1) +
    scale_y_continuous(labels = percent, limits = c(0, 0.3)) +
    theme(axis.title.y = element_blank(), legend.title = element_blank()) +
    theme_minimal()