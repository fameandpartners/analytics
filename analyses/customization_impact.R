# Connect to DB and Pull Data
setwd("~/Documents/Code/analytics/ecommerce-performance")
source("~/Documents/Code/analytics/ecommerce-performance/global.R")
setwd("~/Documents/Data")
source("~/Documents/Code/R/customization_value parsing.R")

# How many dresses can be customized?
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

customization_values %>% count(price) %>% mutate(n / sum(n))
# 24% of customizations are free

customization_values %>% group_by(product_id) %>% summarise(customizations = n()) %>% count(customizations) %>% mutate(n / sum(n))
# 98% of products have 3 customizations available

# Paid Customizations vs. Free Customizations timeseries
products_sold %>% 
    filter(year(order_date) >= 2016 & order_date < as.Date("2017-04-01")) %>%
    left_join(line_item_customizations, by = "line_item_id") %>%
    mutate(customization_price_usd = customization_price * ifelse(currency == "AUD", 0.76, 1)) %>%
    group_by(`Order Month` = order_year_month) %>%
    summarise(`Customization Rate` = sum(physically_customized) / n(),
              `Paid Customization Rate` = sum(physically_customized & coalesce(customization_price_usd, 0) > 0)/n(),
              `Free Customization Rate` = sum(physically_customized & coalesce(customization_price_usd, 0) <= 0)/n()) %>%
    ggplot(aes(x = `Order Month`)) +
    geom_line(aes(y = `Customization Rate`, color = "Customization Rate"), group = 1) +
    geom_line(aes(y = `Paid Customization Rate`, color = "Paid Customization Rate"), group = 1) +
    geom_line(aes(y = `Free Customization Rate`, color = "Free Customization Rate", group = 1)) + 
    scale_y_continuous(labels = percent, limits = c(0, 0.3)) +
    theme_minimal() +
    theme(axis.title.y = element_blank(), 
          legend.title = element_blank())

customization_values %>% 
    filter(!is.na(price)) %>%
    rename(customization_price = price) %>%
    group_by(product_id) %>%
    summarise(free_customizations_available = sum(customization_price == 0)) %>%
    inner_join(products_sold %>%
                   group_by(product_id) %>%
                   summarise(units = n(),
                             customized_units = sum(physically_customized)),
               by = "product_id") %>%
    group_by(free_customizations_available) %>%
    summarise(products = n(),
              customization_rate = sum(customized_units) / sum(units))%>%
    write_csv("~/Documents/Data/Free Customization Distro.csv")
# 49% of products have at least 1 free customization
# 18% of products have 2 or more free customizations
# 4% of products have 3 or more free customizations

products_sold %>% 
    left_join(line_item_customizations, by = "line_item_id") %>%
    filter(!is.na(customization_price)) %>% 
    count(customization_price) %>% 
    mutate(percent(n / sum(n))) %>%
    write_csv("~/Documents/Data/Customization Price Distro.csv")
# 37% of customized sales have only free customizations
# 47% of customized sales have $10 customizations
# 10% of customized sales have $15 customizations

paid_customization_sales <- products_sold %>%
    filter(year(order_date) >= 2016) %>%
    left_join(line_item_customizations, by = "line_item_id") %>%
    mutate(customization_price_usd = customization_price * ifelse(currency == "AUD", 0.76, 1)) %>%
    filter(customization_price > 0 | physically_customized == 0) %>%
    group_by(order_year = year(order_date),
             cust_code = ifelse(physically_customized == 1 & coalesce(customization_price_usd, 0) > 0, 
                                "Customized","Not Customized"))
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

style_rankings <- 
    products_sold %>% filter(sales_usd > 0) %>%
    filter(!is.na(manufacturing_cost)) %>%
    mutate(net_revenue = gross_revenue_usd,
           cogs = manufacturing_cost) %>%
    group_by(style_number) %>%
    summarise(Margin = (sum(net_revenue) - sum(cogs)) / sum(net_revenue),
              Sales = sum(net_revenue))

style_rankings %>%
    filter(between(Margin, 0.001, 0.999)) %>%
    ggplot(aes(x = Margin)) +
    geom_histogram(binwidth = 0.01) +
    scale_x_continuous(limits = c(0.5, 0.8), 
                       labels = percent, 
                       breaks = seq(0.3, 0.8, 0.1)) +
    theme_minimal() +
    ylab("Products") +
    xlab("Net Manufacturing Cost Margin")

style_rankings %>%
    filter(between(Margin, 0.001, 0.999)) %>%
    ggplot(aes(x = Margin, y = Sales)) +
    geom_point()