library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(rpart)

source("~/code/analytics/etl/full_global.R")

fabrics <- tbl(fp_con, sql(paste(
    "SELECT",
        "product_id,",
        "value fabric_description",
    "FROM spree_product_properties",
    "WHERE property_id = 6",
        "AND value IS NOT NULL",
        "AND value != ''"))) %>%
    collect() %>%
    rename(fd = fabric_description) %>%
    mutate(fabric_description = fd %>%
               str_replace("Lining", "\nLining") %>%
               str_replace("Contrast", "\nContrast") %>%
               str_replace("Trim", "\nTrim")) %>%
    select(-fd) %>%
    separate(fabric_description, 
             into = paste0("fabric_line_",1:8),
             sep = "\n|<br>|<p>",
             fill = "right",
             remove = FALSE)

name_lookup <- read_csv("~/data/fabric_names_lookup.csv")

cleaned_fabrics <- fabrics %>%
    semi_join(products_sold, by = "product_id") %>%
    select(product_id, contains("fabric_line")) %>%
    gather(fabric_line, fabric_value, -product_id) %>%
    filter(!is.na(fabric_value) & fabric_value != "") %>%
    mutate(fabric_title = fabric_value %>% str_extract("^(.*?):|^(.*?);"),
           fabric_column = 
               ifelse(fabric_value == "Contrast: 2: 100% polyester milky yarn", "contrast_2",
               ifelse(fabric_title %in% c("Contrast 1:","Contrast:"), "contrast_1",
               ifelse(fabric_title == "Contrast 2:", "contrast_2",
               ifelse(fabric_title == "Contrast 3:", "contrast_3",
               ifelse(fabric_title %in% c("Lining 1:","Lining:","Lining;"), "lining_1",
               ifelse(fabric_title == "Lining 2:", "lining_2",
               ifelse(fabric_title == "Trim:", "trim",
               ifelse(fabric_title == "Main:", "main", "other")))))))),
           fabric_content = fabric_value %>% 
               str_replace("^(.*?):|^(.*?);","") %>%
               str_trim()) %>%
    left_join(name_lookup %>% 
                  rename(fabric_name = `Fabric Name`,
                         fabric_content = `Fabric Content`), 
              by = "fabric_content") %>%
    select(product_id, fabric_column, fabric_name) %>%
    replace(is.na(.), "other") %>%
    unique() %>%
    spread(fabric_column, fabric_name) %>%
    select(product_id, main, lining_1, lining_2, contrast_1, contrast_2,
           contrast_3, trim) %>%
    left_join(fabrics %>%
                  transmute(product_id, 
                            original_fabric_description = fabric_description),
              by = "product_id")

# ---- Forecasting ----

# Top 10 Main Fabrics
top10 <- products_sold %>%
    filter(order_status != "Canceled" & year(order_date) >= 2016) %>%
    inner_join(cleaned_fabrics %>%
                   select(product_id, main), 
               by = "product_id") %>%
    filter(!str_detect(main, "other|TBD")) %>%
    group_by(main) %>%
    summarise(units_sold = sum(quantity)) %>%
    arrange(desc(units_sold)) %>%
    top_n(10, units_sold)

# Main forecasts
# Regression
# Monthly
monthly_main_sales <- products_sold %>%
    filter(order_status != "Canceled" & year(order_date) >= 2016) %>%
    inner_join(cleaned_fabrics %>%
                   select(product_id, main), 
               by = "product_id") %>%
    semi_join(top10, by = "main") %>%
    group_by(order_year = year(order_date), 
             order_month = month(order_date),
             main, color = coalesce(color, "Missing")) %>%
    summarise(units_sold = sum(quantity)) %>%
    arrange(main, color, order_year, order_month) %>%
    group_by(main, color) %>%
    mutate(prior_units_sold = lag(units_sold)) %>%
    filter(!is.na(prior_units_sold)) %>%
    ungroup()

monthly_main_sales %>%
    ggplot(aes(x=prior_units_sold, y=units_sold)) +
    geom_point()

basic_model <- lm(units_sold ~ prior_units_sold, data = monthly_main_sales)

monthly_main_sales$basic_prediction <- predict(basic_model)
monthly_main_sales$basic_upr <- as_data_frame(predict(basic_model, interval = "confidence"))$upr
monthly_main_sales$basic_lwr <- as_data_frame(predict(basic_model, interval = "confidence"))$lwr

log_model <- lm(log10(units_sold) ~ log10(prior_units_sold), data = monthly_main_sales)

monthly_main_sales$log_prediction <- 10**predict(log_model)
monthly_main_sales$log_upr <- as_data_frame(10**predict(log_model, interval = "confidence"))$upr
monthly_main_sales$log_lwr <- as_data_frame(10**predict(log_model, interval = "confidence"))$lwr

summary(basic_model)
summary(log_model)

monthly_main_sales %>%
    ggplot(aes(x=prior_units_sold,y=units_sold)) +
    geom_point() +
    geom_line(aes(y=basic_prediction,color="basic")) +
    geom_line(aes(y=log_prediction,color="log10"))
# Weekly
weekly_main_sales <- products_sold %>%
    filter(order_status != "Canceled" & year(order_date) >= 2016) %>%
    inner_join(cleaned_fabrics %>%
                   select(product_id, main), 
               by = "product_id") %>%
    semi_join(top10, by = "main") %>%
    group_by(order_year = year(order_date), 
             order_week = week(order_date),
             main) %>%
    summarise(units_sold = sum(quantity)) %>%
    arrange(main, order_year, order_week) %>%
    group_by(main) %>%
    mutate(prior_units_sold = lag(units_sold)) %>%
    filter(!is.na(prior_units_sold)) %>%
    ungroup()

basic_model <- lm(units_sold ~ prior_units_sold, data = weekly_main_sales)

weekly_main_sales$basic_prediction <- predict(basic_model)
weekly_main_sales$basic_residual <- residuals(basic_model)

summary(basic_model)

# Residuals vs. Fitted Values
weekly_main_sales %>%
    ggplot(aes(x=basic_prediction,y=basic_residual)) +
    geom_point()

# Decision Tree
rpart_model <- rpart(units_sold ~ prior_units_sold, data = weekly_main_sales)

weekly_main_sales$rpart_prediction <- predict(rpart_model)
weekly_main_sales$rpart_residual <- residuals(rpart_model)

weekly_main_sales %>%
    ggplot(aes(x=rpart_prediction,y=rpart_residual)) +
    geom_point()

mse <- function(actual, prediction){
    mean((actual - prediction)**2)
}
weekly_main_sales %>%
    filter(prior_units_sold < 100) %>%
    summarise(sample_size = n(),
              basic_mse = mse(units_sold, basic_prediction),
              rpart_mse = mse(units_sold, rpart_prediction))


weekly_main_sales %>%
    ggplot(aes(x=prior_units_sold, y=units_sold)) +
    geom_point() +
    geom_line(aes(x=prior_units_sold, y=basic_prediction,color="basic"))



