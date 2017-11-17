library(readr)
library(RPostgreSQL)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(shiny)
library(DT)
library(httr)

# ---- FUNCTIONS ----
year_month <- function(date_value){
    paste(year(date_value),
          formatC(month(date_value), width = 2, flag = "0"),
          sep = "-")
}

dollar <- function(number){
    paste0("$", round(number, digits = 2) %>%
               format(nsmall = 2, big.mark = ","))
}

percent <- function(number){
    paste0(format(100 * round(number, 2), big.mark = ","), "%")
}

short_dollar <- function(number){
    ifelse(number < 1000, paste0("$", round(number)),
           ifelse(number < 1000000, paste0("$", round(number / 1000, 1), "K"),
                  ifelse(number < 1000000000, paste0("$", round(number / 1000000, 1), "M"),
                         paste0("$", round(number / 1000000000, 1), "B"))))
}

short_number <- function(number){
    ifelse(number < 1000, round(number) %>% as.integer(),
           ifelse(number < 1000000, paste0(round(number / 1000, 1), "K"),
                  ifelse(number < 1000000000, paste0((round(number / 1000000, 1)), "M"),
                         paste0(round(number / 1000000000, 1), "B"))))
}

h3c <- function(x) h3(x, align = "center")

query_aud_to_usd <- function(){
    resp <- GET("https://api.fixer.io/latest?base=AUD")
    if(resp$status_code == 200){
        return(content(resp)$rates$USD)
    } else { return(0.75) }
}

dress_image_url <- function(asset_id, attachment_file_name){
    paste0("https://d1msb7dh8kb0o9.cloudfront.net/spree/products/",
           asset_id,
           "/original/",
           attachment_file_name)
}

sql_convert_to_LA_time <- function(utc_time){
    paste0("(", utc_time, " at time zone 'UTC') at time zone 'America/Los_Angeles'")
}

# query conversion rates
aud_to_usd <- 0.74  # query_aud_to_usd()

# ---- COLLECTIONS ----
collections <- read_csv("static-data/collections_2.csv",
                        col_types = "iccccc") %>%
    transmute(product_id = `Product ID`,
              collection_na = Collection)

# ---- MANUFACTURING COSTS ----
factory_costs <- read_csv("static-data/factory_costs.csv",
                          col_types = cols(
                              StyleNumber = col_character(),
                              `Unit Price` = col_double())) %>%
    group_by(style_number = toupper(StyleNumber)) %>%
    summarise(manufacturing_cost = min(`Unit Price`))

# ---- SHIPPING COSTS ----
shipping_costs <- read_csv("static-data/avg_shipping_costs.csv",
                           col_types = cols(
                               YearNum = col_integer(),
                               MonthNum = col_integer(),
                               USD = col_double())) %>%
    rename(ship_year = YearNum, ship_month = MonthNum, avg_order_shipping_cost = USD) %>%
    mutate(ship_year_month = year_month(as.Date(paste(ship_year, ship_month, 1, sep = "-"))))

# ---- TOUCH POINTS ----
all_touches <- read_csv("static-data/all_touches.csv",
                        col_types = cols(
                            .default = col_character(),
                            order_id = col_integer(),
                            user_id = col_integer(),
                            touch_time = col_datetime(format = ""),
                            ordered_at = col_datetime(format = ""),
                            added_to_cart_at = col_datetime(format = ""),
                            total = col_double(),
                            revenue_usd = col_double(),
                            step = readr::col_factor(levels = c("Cart","Checkout","Purchase", ordered = TRUE)),
                            cohort = readr::col_factor(levels = c("Prom", "Bridal", "Contemporary", "Bridesmaid", "Not Assigned", ordered = TRUE))
                        )) %>%
    rename(sales_usd = revenue_usd)

# ---- CUSTOMER ACQUISITIONS ----
customer_acquisitions <- read_csv("static-data/customer_acquisitions.csv",
                                  col_types = cols(
                                      email = col_character(),
                                      date = col_date(format = "")))

# ---- CONNECT TO DW ----
# set db connection
source("fp_init.R")

products <- tbl(dw, "products") %>% 
    select(-id, -created_at) %>% 
    collect()

line_item_customizations <- tbl(dw, "line_item_customizations") %>%
    select(-id, -created_at) %>%
    collect()

customization_values <- tbl(dw, "customization_values") %>%
    select(-id, -created_at) %>%
    collect()

product_taxons <- tbl(dw, "product_taxons") %>% 
    select(-id, -created_at) %>% 
    collect()

products_sold <- tbl(dw, "sales") %>%
    filter(order_date >= "2015-12-15") %>%
    collect() %>%
    left_join(products %>% transmute(product_id, product_live = live),
              by = "product_id") %>%
    mutate(packaging_cost = 2.5)

products_sold$order_status <- factor(
    products_sold$order_status,
    levels = c("Paid","Shipped","Refund Requested","Returned","Canceled")
)

products_sold$size <- factor(
    products_sold$size,
    levels = paste0("US", seq(0,22,2), "/AU", seq(4,26,2))
)

products_sold$height <- factor(
    products_sold$height,
    levels = c("Petite", "Standard", "Tall", paste0("Length", 1:6))
)


# ---- GA & FB ----
fb_images <- tbl(dw, "facebook_images") %>% 
    select(-id, -created_at) %>%
    collect() %>%
    group_by(ad_name) %>%
    filter(!duplicated(str_replace_all(ad_image, "\\?(.*)", ""))) %>%
    summarise(ad_images = paste(paste0("<img height=200px width=350px src=", 
                                       ad_image, ">"), collapse = ""))

ga_fb <- read_csv("static-data/ga_fb.csv",
                  col_types = cols(
                      .default = col_number(),
                      utm_campaign = col_character(),
                      cohort = col_character(),
                      country = col_character(),
                      region = col_character(),
                      age = col_character(),
                      target = col_character(),
                      device_type = col_character(),
                      creative_type = col_character(),
                      creative_strategy = col_character(),
                      theme = col_character(),
                      ad_format = col_character(),
                      pic_source = col_character(),
                      copy_type = col_character(),
                      landing_page = col_character(),
                      product_category = col_character(),
                      products = col_character(),
                      creative = col_character(),
                      Platform = col_character(),
                      prospecting = col_logical(),
                      Date = col_date(format = ""))) %>%
    mutate(Amount_Spent_USD = Amount_Spent_AUD * aud_to_usd) %>%
    left_join(fb_images %>%
                  rename(utm_campaign = ad_name),
              by = "utm_campaign") %>%
    rename(creative_no_image = creative) %>%
    mutate(creative = coalesce(ad_images, creative_no_image))

comp_choices <- c("Spend (USD)","Purchases","CAC","CTR","CPAC","CPL",
                  "T.O.S.","Sessions","Total Carts","Bounce Rate")

# ---- BUDGET DATA ----

returns_reconciled <- read_csv("static-data/reconciled_returns.csv",
                               col_types = cols(.default = col_number())) %>%
    filter(ship_month <= 4)

monthly_actuals_2017 <- products_sold %>%
    filter(is_shipped & order_state != "canceled" & year(ship_date) >= 2016) %>%
    group_by(ship_year = year(ship_date),
             ship_month = month(ship_date)) %>%
    summarise(gross_revenue = sum(gross_revenue_usd),
              net_sales = sum(sales_usd),
              units_shipped = sum(quantity),
              cogs = sum(coalesce(manufacturing_cost, 70))
              + sum(li_shipping_cost)
              + sum(payment_processing_cost)
              + sum(packaging_cost),
              spree_returns = sum(coalesce(refund_amount_usd, 0)),
              total_adjustments = sum(adjustments_usd)) %>%
    left_join(returns_reconciled %>% select(-ship_quarter),
              by = c("ship_year","ship_month")) %>%
    mutate(returns = coalesce(adjusted_returns, spree_returns)) %>%
    mutate(average_selling_price = gross_revenue / units_shipped,
           average_unit_cogs = cogs / units_shipped,
           return_rate = returns / gross_revenue,
           gross_margin = (gross_revenue + total_adjustments - cogs - returns)
           / (gross_revenue + total_adjustments - returns),
           returns_per_unit = returns / units_shipped,
           average_discount = abs(total_adjustments) / gross_revenue) %>%
    select(-spree_returns, -adjusted_returns)
