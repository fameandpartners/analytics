suppressMessages(library(readr))
suppressMessages(library(dbplyr))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(lubridate))
suppressMessages(library(stringr))
suppressMessages(library(feather))

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

dress_image_url <- function(asset_id, attachment_file_name){
    paste0("https://d1msb7dh8kb0o9.cloudfront.net/spree/products/",
           asset_id,
           "/original/",
           attachment_file_name)
}

sql_convert_to_LA_time <- function(utc_time){
    paste0("(", utc_time, " at time zone 'UTC') at time zone 'America/Los_Angeles'")
}

read_sql <- function(file_location) {
    readLines(file_location) %>% str_trim() %>% paste(collapse = " ")
}

# query conversion rates
aud_to_usd <- 0.74  # query_aud_to_usd()

# ---- COLLECTIONS ----
collections <- read_csv("Rscripts/static-data/collections_2.csv",
                        col_types = "iccccc") %>%
    transmute(product_id = `Product ID`,
              collection_na = Collection)

# ---- MANUFACTURING COSTS ----
factory_costs <- read_csv("Rscripts/static-data/factory_costs.csv",
                          col_types = cols(
                              StyleNumber = col_character(),
                              `Unit Price` = col_double())) %>%
    group_by(style_number = toupper(StyleNumber)) %>%
    summarise(manufacturing_cost = min(`Unit Price`))

# ---- SHIPPING COSTS ----
shipping_costs <- read_csv("Rscripts/static-data/avg_shipping_costs.csv",
                           col_types = cols(
                               YearNum = col_integer(),
                               MonthNum = col_integer(),
                               USD = col_double())) %>%
    rename(ship_year = YearNum, ship_month = MonthNum, avg_order_shipping_cost = USD) %>%
    mutate(ship_year_month = year_month(as.Date(paste(ship_year, ship_month, 1, sep = "-"))))

# ---- CONNECT TO REPLICA ----
# set db connection
source("Rscripts/fp_init.R")

# ---- FB Ad Images ----
fb_images <- tbl(fp_con, sql(read_sql("queries/fb_images.sql"))) %>%
    collect() %>%
    group_by(ad_name) %>%
    filter(!duplicated(str_replace_all(ad_image, "\\?(.*)", ""))) %>%
    summarise(ad_images = paste(paste0("<img height=200px width=350px src=",
                                       ad_image, ">"), collapse = ""))

# ---- GA & FB ----
col_args <- function(){
    cols(.default = col_number(),
         utm_campaign = col_character(),
         Platform = col_character(),
         Date = col_date(format = ""))
}
ga_fb <- read_csv("Rscripts/static-data/ga_fb.csv",
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

# ---- COHORTS ----
cohort_assignments <- read_csv("Rscripts/static-data/cohort_assignments.csv",
                               col_types = cols(
                                   email = col_character(),
                                   assigned_cohort = col_character()
                               ))

comp_choices <- c("Spend (USD)","Purchases","CAC","CTR","CPAC","CPL",
                  "T.O.S.","Sessions","Total Carts","Bounce Rate")

# ---- SALES ----
ordered_units <- tbl(fp_con, sql(read_sql("queries/ordered_units.sql"))) %>%
    collect() %>%
    group_by(order_id) %>%
    mutate(gross_extra_attributed = (item_total - sum(price)) / n(),
           net_extra_attributed = (total - sum(price)) / n(),
           conversion_rate = ifelse(currency == "AUD", aud_to_usd, 1),
           sales_usd = (price + net_extra_attributed) * conversion_rate,
           gross_revenue_usd = (price + gross_extra_attributed) * conversion_rate,
           adjustments_total_percentage = o_adjustments / item_total,
           adjustments_usd = gross_revenue_usd * adjustments_total_percentage,
           order_date = as_date(completed_timestamp)) %>%
    ungroup()

# ---- CUSTOMIZATIONS + HEIGHTS ----
customizations <- tbl(fp_con, sql(read_sql("queries/customizations.sql"))) %>%
    collect()

line_item_customizations <- customizations %>%
    filter(str_detect(customisation_value_ids, "[0-9]")) %>%
    mutate(parsed_ids =
               str_replace_all(customisation_value_ids, "\n|'| |---", "") %>%
               substr(2, 20)) %>%
    select(line_item_id, parsed_ids) %>%
    separate(parsed_ids, sep = "-", paste0("id",seq(1,5)),
             extra = "warn", fill = "right") %>%
    gather(which_cust_id, customization_value_id_char, -line_item_id, na.rm = TRUE) %>%
    transmute(line_item_id,
              customization_value_id = as.integer(customization_value_id_char))

customization_values <- tbl(fp_con, sql(read_sql("queries/customization_values.sql"))) %>%
    collect()

# ---- PRODUCTS ----
products <- tbl(fp_con, sql(read_sql("queries/products.sql"))) %>%
    collect() %>%
    mutate(live = ifelse(!hidden
                         & (is.na(deleted_at) | deleted_at > today())
                         & (available_on <= today()),
                         "Yes", "No")) %>%
    select(-hidden, -deleted_at)

# ---- ADDRESSES ----
addresses <- tbl(fp_con, sql(read_sql("queries/addresses.sql"))) %>%
    collect()

# ---- SHIPMENTS ----
shipment_data <- tbl(fp_con, sql(read_sql("queries/shipment_data.sql"))) %>%
    collect()
o_shipments <- shipment_data %>%
    select(order_id, shipped_at)%>%
    group_by(order_id) %>%
    summarise(li_ship_date = max(shipped_at) %>% as.Date)
li_shipments <- shipment_data %>%
    filter(!is.na(ship_line_item_id)) %>%
    rename(line_item_id = ship_line_item_id) %>%
    select(line_item_id, shipped_at) %>%
    group_by(line_item_id) %>%
    summarise(o_ship_date = max(shipped_at) %>% as.Date)

# ---- SHIP DATE CORRECTIONS ----
correct_shipments <- read_csv(
    "Rscripts/static-data/Correct Ship Dates.csv",
    col_types = cols(LINE = col_number(),
                     `SENT DATE` = col_date(format = ""))) %>%
    rename(line_item_id = LINE,
           correct_ship_date = `SENT DATE`) %>%
    filter(!is.na(line_item_id))
correct_shipments$line_item_id <- as.integer(correct_shipments$line_item_id)

# ---- RETURNS ----
returns <- tbl(fp_con, "item_returns") %>%
    select(requested_at, refunded_at, line_item_id, refund_amount, comments,
           reason_category, reason_sub_category, acceptance_status,
           factory_fault, factory_fault_reason, uuid) %>%
    rename(item_return_uuid = uuid) %>%
    rename(return_comments = comments) %>%
    collect()

return_events <- tbl(fp_con, sql(read_sql("queries/return_events.sql"))) %>%
    collect()

# ---- PAYMENTS ----
payments <- tbl(fp_con, sql(read_sql("queries/payments.sql"))) %>%
    collect() %>%
    group_by(order_id) %>%
    summarise(order_payments = n(),
              total_payment_amount = sum(p_amount))

# ---- ADJUSTMENTS ----
adjustments <- tbl(fp_con, sql(read_sql("queries/adjustments.sql"))) %>%
    collect() %>%
    left_join(data_frame(originator_type = c("Spree::TaxRate","Spree::ShippingMethod","Spree::PromotionAction",NA),
                         adjustment_type = c("o_taxes","o_shipping","o_promotions","o_other_adjustments")),
              by = "originator_type") %>%
    select(-originator_type) %>%
    spread(adjustment_type, adjustments, fill = 0)

# ---- PROMOTIONS ----
promotions <- tbl(fp_con, sql(read_sql("queries/promotions.sql"))) %>%
    collect() %>%
    mutate(coupon_code = labels %>%
               str_replace_all("Promotion |\\(|\\)", "")) %>%
    select(-labels)

# ---- PRODUCT TAXONS ----
product_taxons <- tbl(fp_con, sql(read_sql("queries/product_taxons.sql"))) %>%
    collect()

# ---- DRESS IMAGES ----
dress_images <- tbl(fp_con, sql(read_sql("queries/dress_images.sql"))) %>%
    collect() %>%
    filter(!duplicated(product_id)) %>%
    mutate(
        dress_image_url = paste0(
            'https://d1msb7dh8kb0o9.cloudfront.net/spree/products/',
            asset_id,
            '/original/',
            attachment_file_name
        ),
        dress_image_tag = paste0(
            '<img src="',
            dress_image_url,
            '" height="100" width="',
            100*(attachment_width/attachment_height),
            '"></img>'
        )
    )

# ---- SLOW AND FAST MAKING ----
slow_fast_items <- tbl(fp_con, sql(read_sql("queries/slow_fast_items.sql"))) %>%
    collect()

# ---- MERGE + TRANSFORM QUERIES INTO MASTER SALES DF ----
products_sold <- ordered_units %>%
    left_join(customizations, by = "line_item_id") %>%
    left_join(products, by = "product_id") %>%
    left_join(addresses, by = "ship_address_id") %>%
    left_join(o_shipments, by = "order_id") %>%
    left_join(li_shipments, by = "line_item_id") %>%
    left_join(returns, by = "line_item_id") %>%
    left_join(return_events, by = "item_return_uuid") %>%
    left_join(payments, by = "order_id") %>%
    left_join(adjustments, by = "order_id") %>%
    left_join(promotions, by = "order_id") %>%
    left_join(product_taxons %>%
                  filter(taxon_name %>% tolower() %>% str_detect("mini|knee|petti|midi|ankle|maxi|long")
                         & !str_detect(taxon_name, " ")) %>%
                  group_by(product_id) %>%
                  summarise(length = paste0(taxon_name[1] %>% str_trim() %>% substr(1, 1) %>% toupper(),
                                            taxon_name[1] %>% str_trim() %>% substr(2, 10))),
              by = "product_id") %>%
    left_join(cohort_assignments %>% filter(!duplicated(email)), by = "email") %>%
    left_join(correct_shipments %>%
                  group_by(line_item_id) %>%
                  summarise(correct_ship_date = min(correct_ship_date)),
              by = "line_item_id") %>%
    left_join(slow_fast_items, by = "line_item_id") %>%
    group_by(order_id) %>%
    mutate(payments = coalesce(order_payments / n(), 0),
           item_total_usd = item_total * conversion_rate,
           promotions_usd = gross_revenue_usd * (o_promotions / item_total),
           shipping_usd = gross_revenue_usd * (o_shipping / item_total),
           taxes_usd = gross_revenue_usd * (o_taxes / item_total),
           other_adjustments_usd = gross_revenue_usd * (o_other_adjustments / item_total)) %>%
    ungroup() %>%
    mutate(ship_date = coalesce(correct_ship_date, li_ship_date, o_ship_date),
           refund_amount_usd = (refund_amount / 100) * ifelse(currency == "AUD", aud_to_usd, 1),
           price_usd = price * ifelse(currency == "AUD", aud_to_usd, 1),
           height = coalesce(paste0(substr(lip_height, 1, 1) %>% toupper(),
                                    substr(lip_height, 2, 250)),
                             paste0(substr(v_height, 1, 1) %>% toupper(),
                                    substr(v_height, 2, 250))),
           size = coalesce(g_size, lip_size),
           return_order_id = ifelse(!is.na(acceptance_status), order_id, NA),
           is_shipped = !is.na(ship_date),
           return_requested = !is.na(return_order_id),
           item_returned = !is.na(refund_amount),
           order_status = ifelse(order_state == "canceled", "Canceled",
                          ifelse(item_returned, "Returned",
                          ifelse(return_requested, "Refund Requested",
                          ifelse(is_shipped, "Shipped","Paid")))),
           return_reason = ifelse(tolower(reason_category) %in% c('n/a','na','not specified','not stated','not satisfied')
                                  | is.na(reason_category),
                                  "No Reason",
                                  paste0(toupper(substr(str_trim(reason_category), 1, 1)),
                                         substr(str_trim(reason_category), 2, 250))),
           estimated_ship_date = ifelse(is.na(ship_date), order_date + 10, ship_date) %>% as.Date(origin = "1970-01-01"),
           ship_year_month = year_month(estimated_ship_date),
           order_year_month = year_month(order_date),
           payment_processing_cost = (sales_usd * 0.029) + 0.3,
           physically_customized = ifelse(is.na(customized), 0, customized)) %>%
    left_join(collections %>%
                  group_by(product_id) %>%
                  summarise(collection_na = paste(unique(collection_na), collapse = ", ")),
              by = "product_id") %>%
    mutate(collection = ifelse(is.na(collection_na), "2014-2015 - Old", collection_na)) %>%
    select(-collection_na) %>%
    separate(size, c("us_size_str","au_size_str"), sep = "/", remove = FALSE,
             extra = "merge", fill = "left") %>%
    mutate(us_size = as.integer(str_replace_all(us_size_str, "US", ""))) %>%
    left_join(factory_costs %>%
                  group_by(style_number) %>%
                  summarise(manufacturing_cost = mean(manufacturing_cost)),
              by = "style_number") %>%
    # Filter out Lacey's PR Order discounted improperly
    filter(order_id != 26075489) %>%
    # Filter out mysterious Manual order that's not right
    filter(order_id != 32162864) %>%
    # Filter PR orders
    filter(email != "abbyv@fameandpartners.com") %>%
    # Filter out reggie's test order for Stripe
    filter(order_number != "R028450713") %>%
    filter(between(adjustments_total_percentage, -0.9, 1)) %>%
    filter(payments != 0) %>%
    left_join(shipping_costs %>% select(-ship_year, -ship_month),
              by = c("ship_year_month")) %>%
    group_by(order_id) %>%
    mutate(li_shipping_cost0 = coalesce(avg_order_shipping_cost / n(),
                                       median(shipping_costs$avg_order_shipping_cost) / n()),
           li_shipping_cost = ifelse(return_requested, li_shipping_cost0 + 5, li_shipping_cost0),
           packaging_cost = 2.5,
           units_in_order = sum(quantity)) %>%
    ungroup() %>%
    left_join(dress_images, by = "product_id") %>%
    rename(reason_dirty = return_reason) %>%
    mutate(reason_dirty1 = paste0(substr(reason_dirty %>%
                                                    str_replace_all("_"," "),
                                                1,1) %>%
                                             toupper(),
                                         substr(reason_dirty %>%
                                                    str_replace_all("_"," "),
                                                2,250) %>%
                                             tolower()),
           return_reason = ifelse(reason_dirty1 == "Ordered multiple styles or sizes","Ordered multiple",
                           ifelse(reason_dirty1 == "Looks different to image on site","Looks different",
                           ifelse(reason_dirty1 == "Poor quality or faulty", "Poor quality", reason_dirty1)))) %>%
    select(-contains("reason_dirty"))

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

write_feather(products_sold, "feathers/sales.feather")
write_feather(products, "feathers/products.feather")
write_feather(product_taxons, "feathers/product_taxons.feather")
write_feather(fb_images, "feathers/facebook_images.feather")
write_feather(line_item_customizations, "feathers/line_item_customizations.feather")
write_feather(customization_values, "feathers/customization_values.feather")
