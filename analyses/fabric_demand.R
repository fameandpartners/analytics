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
    geom_point() +
    xlim(0, 200)

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
    filter(prior_units_sold < 200) %>%
    summarise(sample_size = n(),
              basic_mse = mse(units_sold, basic_prediction),
              rpart_mse = mse(units_sold, rpart_prediction))


weekly_main_sales %>%
    ggplot(aes(x=prior_units_sold, y=units_sold)) +
    geom_point() +
    geom_line(aes(x=prior_units_sold, y=basic_prediction,color="basic")) +
    xlim(0, 200) + ylim(0, 200)

# With new fabric data
fabric_usage <- read_csv("~/data/fabric_usage.csv")

fabric_percentiles <- products_sold %>%
    filter(order_status != "Canceled" & year(order_date) >= 2016) %>%
    group_by(order_year = year(order_date), 
             order_week = week(order_date),
             product_id) %>%
    summarise(units_sold = sum(quantity)) %>%
    inner_join(fabric_usage, by = "product_id") %>%
    mutate(meters = usage*units_sold) %>%
    group_by(fabric) %>%
    summarise(meters = sum(meters)) %>%
    mutate(percentile = ntile(meters, 10)) 

top_fabrics <- products_sold %>%
    filter(order_status != "Canceled" & year(order_date) >= 2017) %>%
    group_by(order_year = year(order_date), 
             order_week = week(order_date),
             product_id) %>%
    summarise(units_sold = sum(quantity)) %>%
    inner_join(fabric_usage, by = "product_id") %>%
    mutate(meters = usage*units_sold) %>%
    group_by(fabric) %>%
    summarise(meters = sum(meters)) %>%
    top_n(25, meters)

weekly_meds <- products_sold %>%
    filter(order_status != "Canceled" & year(order_date) >= 2016) %>%
    group_by(order_year = year(order_date), 
             order_week = week(order_date),
             product_id) %>%
    summarise(units_sold = sum(quantity)) %>%
    inner_join(fabric_usage, by = "product_id") %>%
    inner_join(fabric_percentiles, by = "fabric") %>%
    mutate(meters = usage*units_sold) %>%
    group_by(order_week, percentile) %>%
    summarise(med_meters = max(meters)) %>%
    ungroup()

weekly_fabrics <- products_sold %>%
    filter(order_status != "Canceled" & year(order_date) >= 2016) %>%
    group_by(order_year = year(order_date), 
             order_week = week(order_date),
             product_id) %>%
    summarise(units_sold = sum(quantity)) %>%
    inner_join(fabric_usage, by = "product_id") %>%
    mutate(meters = usage*units_sold) %>%
    group_by(order_year, order_week, fabric) %>%
    summarise(meters = sum(meters)) %>%
    arrange(fabric, order_year, order_week) %>%
    group_by(fabric) %>%
    mutate(prior_meters = lag(meters)) %>%
    filter(!is.na(prior_meters)) %>%
    ungroup()

weekly_fabrics %>%
    semi_join(top_fabrics, by = "fabric") %>%
    ggplot(aes(x=order_week, y=meters)) +
    geom_point() +
    geom_path(data = weekly_meds, 
              aes(y = med_meters,
                  color = as.factor(percentile)))

weekly_fabrics %>%
    ggplot(aes(x = prior_meters, y = meters)) +
    geom_point() +
    xlim(0, 1000) + ylim(0, 1000)

fabrics_model <- lm(meters ~ prior_meters, weekly_fabrics)
summary(fabrics_model)

weekly_fabrics$lm_pred = predict(fabrics_model)
weekly_fabrics$lm_lwr = as.data.frame(predict(fabrics_model, interval = "confidence"))$lwr
weekly_fabrics$lm_upr = as.data.frame(predict(fabrics_model, interval = "confidence"))$upr
weekly_fabrics$lm_resid = residuals(fabrics_model)
weekly_fabrics$null_pred = weekly_fabrics$prior_meters

weekly_fabrics %>%
    summarise(sample_size = n(),
              lm_mse = mse(meters, lm_pred),
              null_mse = mse(meters, null_pred))

weekly_fabrics %>%
    ggplot(aes(x=prior_meters, y=meters)) +
    geom_point() +
    geom_line(aes(y=lm_pred)) +
    geom_line(aes(y=lm_upr)) +
    geom_line(aes(y=lm_lwr))
    
weekly_fabrics %>%
    ggplot(aes(lm_pred)) +
    geom_histogram(binwidth = 100)

weekly_fabrics %>%
    ggplot(aes(lm_resid)) +
    geom_histogram(binwidth = 10) +
    xlim(-100,100)

weekly_fabrics %>%
    filter(order_year == 2017) %>%
    group_by(order_week) %>%
    summarise(meters = sum(meters), pred_meters = sum(lm_pred)) %>%
    ggplot(aes(x=order_week)) +
    geom_line(aes(y=meters, color="Actual")) +
    geom_line(aes(y=pred_meters, color="Predicted"))

weekly_fabrics %>%
    filter(order_year == 2017) %>%    
    semi_join(top_fabrics[5:10,], by = "fabric") %>%
    ggplot(aes(x=order_week)) +
    geom_line(aes(y=meters, color="Actual")) +
    geom_line(aes(y=lm_pred, color="Predicted")) +
    facet_grid(.~fabric)
# Try to simulate what running a regression recursively on one week
# of data to predict total volume for that month
weekly_predict <- function(prior_meters){
    new <- data.frame(prior_meters = prior_meters)
    predict(fabrics_model, newdata = new)
}

week_based_monthly <- products_sold %>%
    filter(order_status != "Canceled" & year(order_date) >= 2016) %>%
    group_by(order_year = year(order_date), 
             order_month = month(order_date),
             order_week = week(order_date),
             product_id) %>%
    summarise(units_sold = sum(quantity)) %>%
    inner_join(fabric_usage, by = "product_id") %>%
    mutate(meters = usage*units_sold) %>%
    group_by(order_year, order_month, order_week, fabric) %>%
    summarise(meters = sum(meters)) %>%
    arrange(fabric, order_year, order_week) %>%
    group_by(fabric) %>%
    mutate(prior_meters = lag(meters)) %>%
    filter(!is.na(prior_meters)) %>%
    ungroup() %>%
    group_by(order_year, order_month) %>%
    filter(order_week == min(order_week)) %>%
    mutate(wlmpred = 
               weekly_predict(prior_meters) # Next week
           + prior_meters %>% weekly_predict() %>% weekly_predict() # The week after
           + prior_meters %>% weekly_predict() %>% weekly_predict() %>% weekly_predict() # The week after that
    ) %>%
    select(order_year, order_month, fabric, wlmpred) %>%
    unique()

monthly_fabrics2 <- monthly_fabrics %>%
    inner_join(week_based_monthly, by = c("order_year","order_month","fabric"))

mixed_model1 <- lm(meters ~ prior_meters / season, data = monthly_fabrics2)
summary(mixed_model1)

mixed_model2 <- lm(meters ~ wlmpred + prior_meters / season, data = monthly_fabrics2)
summary(mixed_model2)

just_wlm <- lm(meters ~ wlmpred, data = monthly_fabrics2)
summary(just_wlm)

monthly_fabrics2$mixed_pred1 = predict(mixed_model1, newdata = monthly_fabrics2)
monthly_fabrics2$mixed_pred2 = predict(mixed_model2)
monthly_fabrics2$just_wlm_pred = predict(just_wlm)

monthly_fabrics2 %>%
    summarise(mse(meters, wlmpred),
              mse(meters, mixed_pred1),
              mse(meters, mixed_pred2),
              mse(meters, just_wlm_pred)) %>%
    gather(model, mse) %>%
    arrange(mse) %>%
    mutate(sqrt(mse))

seasonality <- products_sold %>%
    filter(order_status != "Canceled" & year(order_date) >= 2015) %>%
    group_by(order_year = year(order_date), 
             order_month = month(order_date)) %>%
    summarise(units_sold = sum(quantity)) %>%
    filter(ifelse(order_year == 2017 & order_month == 10, F, T)) %>%
    group_by(order_month) %>%
    mutate(period_mean = mean(units_sold)) %>%
    ungroup() %>%
    mutate(overall_mean = mean(units_sold)) %>%
    select(-order_year, -units_sold) %>%
    unique() %>%
    mutate(deseason = period_mean / overall_mean) %>%
    select(-contains("mean"))

monthly_meters <- products_sold %>%
    filter(year(order_date) >= 2015) %>%
    group_by(order_year = year(order_date), 
             order_quarter = quarter(order_date),
             order_month = month(order_date),
             product_id) %>%
    summarise(units_sold = sum(quantity)) %>%
    inner_join(fabric_usage, by = "product_id") %>%
    mutate(meters = usage*units_sold) %>%
    group_by(order_year, order_month, order_quarter, fabric) %>%
    summarise(meters = sum(meters))

monthly_fabrics <- monthly_meters %>%
    filter(ifelse(order_year == year(today()),
           order_month < month(today()), TRUE)) %>%
    inner_join(seasonality, by = "order_month") %>%
    group_by(fabric) %>%
    arrange(fabric, order_year, order_month) %>%
    mutate(prior_meters = lag(meters)) %>%
    arrange(fabric, order_month, order_year) %>%
    mutate(prior_yr_meters = lag(meters)) %>%
    ungroup()

monthly_lm1 <- lm(meters ~ prior_meters, data = monthly_fabrics)
summary(monthly_lm1)

monthly_lm2 <- lm(meters ~ deseason, data = monthly_fabrics)
summary(monthly_lm2)

monthly_lm3 <- lm(meters ~ prior_meters / deseason, data = monthly_fabrics)
summary(monthly_lm3)

monthly_lm4 <- lm(meters ~ prior_meters + Q1 + Q2 + Q3, data = monthly_fabrics)
summary(monthly_lm4)

monthly_lm5 <- lm(meters ~ (prior_meters * prior_yr_meters) / deseason , data = monthly_fabrics)
summary(monthly_lm5)

long_term_mlm <-lm(meters ~ prior_yr_meters / deseason, data = monthly_fabrics) 
summary(long_term_mlm)

monthly_fabrics$m1lm_pred <- predict(monthly_lm1)
monthly_fabrics$m2lm_pred <- predict(monthly_lm2)
monthly_fabrics$m3lm_pred <- predict(monthly_lm3)
monthly_fabrics$m4lm_pred <- predict(monthly_lm4)
monthly_fabrics$m5lm_pred <- predict(monthly_lm5)
monthly_fabrics$mmixed_pred  <- predict(mixed_model2, newdata = monthly_fabrics)

model_performance_by_scale <- monthly_fabrics %>%
    group_by(meters_q = ntile(prior_meters, 5)) %>%
    summarise(m1 = mse(meters, m1lm_pred),
              m2 = mse(meters, m2lm_pred),
              m3 = mse(meters, m3lm_pred),
              m4 = mse(meters, m4lm_pred),
              m5 = mse(meters, m5lm_pred),
              max_pm = max(prior_meters)) %>%
    gather(model, mse, -meters_q, -max_pm) %>%
    arrange(meters_q, mse) %>%
    group_by(meters_q) %>%
    mutate(ee=sqrt(mse), winner = as.numeric(mse == min(mse)))

model_performance_by_scale %>%
    select(meters_q, model, winner, max_pm, ee) %>% 
    filter(winner == 1)
# # A tibble: 5 x 5
# # Groups:   meters_q [5]
# meters_q model winner  max_pm         ee
# 1        1    m1      1    10.5   85.21385
# 2        2    m1      1    30.0   83.58873
# 3        3    m1      1    94.0  111.01240
# 4        4    m4      1   371.0  186.63079
# 5        5    m3      1 15754.5 1212.04855
# Model 1 - the simplest - works best when prior meters is lower than 94 
# Model 4 - the one using dummy vars for Q1-Q3 to account for seasonality - works best from 94 to 371
# Model 3 - the one using the fancy deseason stat works best at large scales

monthly_fabrics %>%
    semi_join(top_fabrics %>% top_n(10, meters), by = "fabric") %>%
    filter(order_year >= 2017) %>%
    ggplot(aes(x=order_month)) +
    geom_path(aes(y=meters, color="Actual")) +
    geom_path(aes(y=m5lm_pred, color="Predicted")) +
    facet_wrap(~fabric, nrow = 2)


fabrics_with_data <- monthly_fabrics2 %>%
    filter(order_year >= 2017) %>%
    group_by(fabric) %>%
    summarise(meters = sum(meters)) %>%
    arrange(desc(meters)) %>%
    select(fabric) 

monthly_fabrics2$fabric <- factor(
    monthly_fabrics2$fabric,
    levels = fabrics_with_data$fabric
)

monthly_fabrics2 %>%
    filter(order_year >= 2017) %>%
    semi_join(monthly_fabrics2 %>%
                  filter(order_year >= 2017) %>%
                  group_by(fabric) %>%
                  summarise(meters = sum(meters)) %>%
                  arrange(desc(meters)) %>%
                  top_n(5, meters), 
              by = "fabric") %>%
    ggplot(aes(x=order_month)) +
    geom_path(aes(y=meters, color="Actual")) +
    geom_path(aes(y=m3lm_pred, color="Basic LM")) +
    geom_path(aes(y=mixed_pred, color="Mixed LM")) +
    facet_grid(.~fabric)

monthly_fabrics2 %>%
    filter(order_year >= 2017) %>%
    semi_join(monthly_fabrics2 %>%
                  filter(order_year >= 2017) %>%
                  group_by(fabric) %>%
                  summarise(meters = sum(meters)) %>%
                  arrange(desc(meters)) %>%
                  top_n(5, meters), 
              by = "fabric") %>%
    group_by(fabric) %>%
    summarise(m3lm_mse = mse(meters, m3lm_pred),
              wlm_mse = mse(meters, wlmpred),
              mixed_mse = mse(meters, mixed_pred))

seasonality1 <- products_sold %>%
    group_by(order_year = year(order_date), 
             order_week = week(order_date)) %>%
    summarise(units_sold = sum(quantity)) %>%
    filter(order_year >= 2015) %>%
    ungroup() %>%
    mutate(overall_mean = mean(units_sold)) %>%
    spread(order_year, units_sold) %>%
    mutate(period_mean = ifelse(is.na(`2017`),
                                (`2015`+`2016`)/2,
                                (`2015`+`2016`+`2017`)/3),
           deseason = period_mean / overall_mean)

seasonality2 <- products_sold %>%
    group_by(order_year = year(order_date), 
             order_week = week(order_date)) %>%
    summarise(units_sold = sum(quantity)) %>%
    filter(order_year >= 2015) %>%
    mutate(seasonal_zscore = (units_sold - mean(units_sold)) / sd(units_sold)) %>%
    group_by(order_week) %>%
    summarise(mean_seasonal_zscore = mean(seasonal_zscore))

weekly_fabrics2 <- products_sold %>%
    filter(order_status != "Canceled" & year(order_date) >= 2016) %>%
    group_by(order_year = year(order_date), 
             order_quarter = quarter(order_date),
             order_week = week(order_date),
             product_id) %>%
    summarise(units_sold = sum(quantity)) %>%
    inner_join(fabric_usage, by = "product_id") %>%
    mutate(meters = usage*units_sold) %>%
    group_by(order_year, order_week, fabric) %>%
    summarise(meters = sum(meters)) %>%
    left_join(seasonality1, by = "order_week") %>%
    left_join(seasonality2, by = "order_week") %>%
    group_by(fabric) %>%
    arrange(fabric, order_year, order_week) %>%
    mutate(prior_meters = lag(meters)) %>%
    arrange(fabric, order_week, order_year) %>%
    mutate(prior_yr_meters = lag(meters)) %>%
    filter(!is.na(prior_meters) & !is.na(prior_yr_meters)) %>%
    ungroup()

seasonal_model1 <- lm(meters ~ (prior_meters * prior_yr_meters) / deseason, weekly_fabrics2)
summary(fabrics_model)
summary(seasonal_model1)

long_term_model <- lm(meters ~ prior_yr_meters / deseason, weekly_fabrics2)
summary(long_term_model)
predict(long_term_model, interval = "confidence", level = 0.999)

seasonal_model2 <- lm(meters ~ prior_meters + mean_seasonal_zscore, weekly_fabrics2)
summary(seasonal_model2)
anova(seasonal_model2)

anova(fabrics_model, seasonal_model2, test="F")

compare_df <- data_frame(
    order_year = weekly_fabrics$order_year,
    order_week = weekly_fabrics$order_week,
    meters = weekly_fabrics2$meters,
    fabric = weekly_fabrics2$fabric,
    basic_lm = predict(fabrics_model, newdata = weekly_fabrics2),
    seasonal_lm1 = predict(seasonal_model1),
    seasonal_lm2 = predict(seasonal_model2)
)

compare_df %>%
    summarise(mse(meters, basic_lm),
              mse(meters, seasonal_lm1),
              mse(meters, seasonal_lm2)) %>%
    gather(model, mse) %>%
    arrange(mse) %>%
    mutate(sqrt(mse))

compare_df %>%
    filter(fabric %in% arrange(top_fabrics, desc(meters))$fabric[1:5]) %>%
    ggplot(aes(x = order_week)) +
    geom_path(aes(y = meters, color = "Actual")) +
    geom_path(aes(y = seasonal_lm1, color = "Predicted")) +
    facet_grid(order_year~fabric)

write_csv(fabric_usage, "~/code/analytics/dw/Rscripts/static-data/fabric_usage.csv")

# Chosen Models:

# Short term / high confidence
# Next Week:
summary(seasonal_model1)
# Next Month:
summary(monthly_lm5)

# Long term / low confidence
# Weekly
summary(long_term_model)
# Extrapolate this out 12 weeks to forecast next 3 months

