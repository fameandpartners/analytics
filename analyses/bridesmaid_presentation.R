source("~/code/analytics/etl/full_global.R")

bridesmaid_sales <- products_sold %>%
    filter(order_date %>% between(as.Date("2017-04-15"), as.Date("2017-06-28"))
           & collection %in% c("2017 - Bridesmaids 4.2",
                               "2017 - PRE-MADE BRIDESMAID")
           & order_status %in% c("Shipped","Refund Requested","Returned"))

sales_comparisons <- bridesmaid_sales %>%
    group_by(collection, 
             style = substr(style_number, 1, 6),
             return_requested) %>%
    summarise(Units = sum(quantity)) %>%
    mutate(platform = ifelse(collection %>% str_detect("PRE"),
                             "Pre-Made", "Wedding App"),
           status = ifelse(return_requested, "Returned", "Shipped")) %>%
    inner_join(products %>%
                   transmute(style = style_number, style_name),
               by = "style")

style_sales <- sales_comparisons %>%
    group_by(style_name) %>%
    summarise(sales = sum(Units)) %>%
    arrange(sales)

sales_comparisons$style_name <- factor(
    sales_comparisons$style_name,
    levels = style_sales$style_name
)

sales_comparisons %>%
    ggplot(aes(x = style_name, y = Units, fill = status)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    facet_grid(.~platform) +
    scale_fill_brewer(palette = "Set1") +
    theme_bw(base_size = 16) +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank())

color_comps <- bridesmaid_sales %>%
    filter(!is.na(color)) %>%
    group_by(collection, 
             color,
             status = ifelse(return_requested, "Returned", "Shipped")) %>%
    summarise(Units = sum(quantity)) %>%
    mutate(platform = ifelse(collection %>% str_detect("PRE"),
                             "Pre-Made", "Wedding App"))

color_sales <- color_comps %>%
    group_by(color) %>%
    summarise(sales = sum(Units)) %>%
    arrange(sales)

color_comps$color <- factor(
    color_comps$color,
    levels = color_sales$color
)

color_comps %>%
    ggplot(aes(x = color, y = Units, fill = status)) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(palette = "Set1") +
    coord_flip() +
    facet_grid(.~platform) +
    theme_bw(base_size = 16) +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank())

bridesmaid_sales %>%
    group_by(collection, order_week = week(order_date)) %>%
    summarise(Units = sum(quantity)) %>%
    summarise(avg_weekly_units = mean(Units))

bridesmaid_sales %>%
    group_by(collection) %>%
    summarise(`Net Sales` = sum(sales_usd),
              ASP = mean(sales_usd),
              `Gross Margin` = 
                  (sum(gross_revenue_usd) + 
                       sum(adjustments_usd) - 
                       (sum(coalesce(manufacturing_cost, 70)) + 
                            sum(li_shipping_cost) + 
                            sum(payment_processing_cost)) - 
                       sum(coalesce(refund_amount_usd, 0))) /
                  (sum(gross_revenue_usd) + 
                       sum(adjustments_usd) - 
                       sum(coalesce(refund_amount_usd, 0))))