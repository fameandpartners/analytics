library(readr)
library(dplyr)
library(stringr)
library(tidyr)

path_to_marketing_dropbox <- "/Users/Peter 1/Dropbox (Team Fame)/data/marketing/"

# ---- GOOGLE ANALYTICS ----
ga <- lapply(
    paste0(path_to_marketing_dropbox, "google_analytics/",
           list.files(path = paste0(path_to_marketing_dropbox, 
                                    "google_analytics"), 
                      pattern="*.csv")),
    read_csv,
    skip = 6,
    col_types = cols(
        .default = col_number(),
        Source = col_character(),
        Medium = col_character(),
        Campaign = col_character(),
        `Session Duration` = col_character(),
        `Avg. Session Duration` = col_character(),
        Date = col_date(format = "%Y%m%d"))) %>%
    bind_rows() %>%
    separate(`Session Duration`, into = c("SDH","SDM","SDS"), sep = ":") %>%
    mutate(utm_campaign = Campaign,
           New_Sessions = `% New Sessions` * Sessions,
           Page_Views = Clicks / (CTR / 100),
           Session_Duration_min = as.numeric(SDH) * 60 + as.numeric(SDM) + as.numeric(SDS) / 60,
           Platform = ifelse(str_detect(Source, "fbps|[f|F]acebook"), "Facebook",
               ifelse(str_detect(Source, "igps|[i|I]nstagram"), "Instagram", "Other")),
           ga_id = row_number()) %>%
    group_by(Date, Platform, utm_campaign) %>%
    summarise(Page_Views = sum(coalesce(Page_Views, 0)),
              Sessions = sum(coalesce(Sessions, 0)),
              New_Sessions = sum(coalesce(New_Sessions, 0)),
              Bounces = sum(coalesce(Bounces, 0)),
              Clicks = sum(coalesce(Clicks, 0)),
              Cost = sum(coalesce(Cost, 0)),
              Transactions = sum(coalesce(Transactions, 0)),
              Revenue = sum(coalesce(Revenue, 0)),
              Session_Duration_min = sum(coalesce(Session_Duration_min, 0))) %>%
    ungroup() %>%
    mutate(`% New Sessions` = coalesce(100 * (New_Sessions / Sessions), 0),
           Bounce_Rate = coalesce(100 * (Bounces / Sessions), 0),
           Avg_Session_Duration_min = coalesce(Session_Duration_min / Sessions, 0),
           CPC = coalesce(100 * (Cost / Clicks), 0),
           CTR = coalesce(Clicks / Page_Views, 0)) %>%
    replace(. == Inf, 0)

# ---- FACEBOOK ----
fb <- lapply(
    paste0(path_to_marketing_dropbox, "facebook/",
           list.files(path = paste0(path_to_marketing_dropbox, "facebook"), 
                      pattern="*.csv")),
    read_csv, 
    col_types = cols(
        .default = col_number(),
        `Reporting Starts` = col_date(),
        `Reporting Ends` = col_date(),
        `Ad Name` = col_character(),
        `Campaign Name` = col_character(),
        `Ad Set Name` = col_character(),
        Platform = col_character())) %>%
    bind_rows() %>%
    mutate(utm_campaign = `Ad Name`,
           Date = `Reporting Starts`,
           Unique_Clicks = coalesce(`Amount Spent (AUD)` / `Cost per Unique Click (All) (AUD)`, 0),
           fb_id = row_number()) %>%
    filter(Impressions > 0 & Reach > 0) %>%
    # Remove these aggregates after data cleaned
    group_by(Date, Platform, utm_campaign) %>%
    summarise(Reach = sum(Reach),
              Impressions = sum(Impressions),
              Amount_Spent_AUD = sum(coalesce(`Amount Spent (AUD)`, 0)),
              Unique_Clicks = sum(coalesce(Unique_Clicks, 0)),
              Unique_Link_Clicks = sum(coalesce(`Unique Link Clicks`, 0)),
              Adds_to_Cart = sum(coalesce(`Website Adds to Cart`, 0)),
              Conversions = sum(coalesce(`Website Conversions`, 0)),
              Leads = sum(coalesce(`Website Leads`, 0)),
              Purchases  = sum(coalesce(`Website Purchases`, 0)),
              Post_Shares = sum(coalesce(`Post Shares`, 0)),
              Post_Comments = sum(coalesce(`Post Comments`, 0)),
              Post_Reactions = sum(coalesce(`Post Reactions`, 0))) %>%
    ungroup() %>%
    mutate(Frequency = Impressions / Reach,
           Unique_CTR = coalesce(Unique_Clicks / Reach, 0),
           Unique_CPC = coalesce(Amount_Spent_AUD / Unique_Clicks, 0),
           fb_id = row_number()) %>%
    replace(. == Inf, 0)

# ---- MAILCHIMP ----
mc_csv <- read_csv(paste0(path_to_marketing_dropbox, 
                      "mailchimp/subscribed_members_export_ed02d85ed6.csv"),
               col_types = cols(.default = col_character())) %>%
    rename(utm_camp_low = utm_campaign) %>%
    mutate(Date = CONFIRM_TIME %>% 
               substr(1, 10) %>%
               as.Date(),
           Platform = ifelse(str_detect(utm_source, "fbps|[f|F]acebook"), "Facebook",
                      ifelse(str_detect(utm_source, "igps|[i|I]nstagram"), "Instagram", 
                             "Other")),
           utm_campaign = toupper(utm_camp_low))

mc <- mc_csv %>%
    filter(!is.na(utm_campaign) & !is.na(utm_source)) %>%
    group_by(Date, Platform, utm_campaign) %>%
    summarise(leads_na = n()) %>%
    ungroup()

# ---- MERGE GA & FB ----
ga_fb <- fb %>% 
    inner_join(ga, by = c("utm_campaign","Date","Platform")) %>%
    left_join(mc, by = c("utm_campaign","Date","Platform")) %>%
    separate(utm_campaign, into = c('cohort','country','region','age','target',
                                    'device_type','creative_type',
                                    'creative_strategy','theme','ad_format',
                                    'pic_source','copy_type','landing_page',
                                    'product_category','products'), 
             sep = "_", extra = "merge", remove = FALSE) %>%
    select(-Leads) %>%
    mutate(creative = paste(creative_type, creative_strategy, 
                            theme, ad_format, pic_source, copy_type,
                            landing_page, product_category, products),
           prospecting = !str_detect(cohort, "-RE"),
           Leads = coalesce(leads_na, as.integer(0))) %>%
    select(-leads_na)

# ---- PULL UTM CAMPAIGN TO PRODUCT NAME LOOKUP ----
upper_products_campaigns <- ga_fb %>% 
    select(utm_campaign, products) %>%
    separate(products, sep = "-", into = paste0("A", 1:7),
             extra = "drop", fill = "left") %>%
    gather(AX, product, -utm_campaign) %>%
    filter(!is.na(product) & product != "NA") %>%
    select(-AX) %>%
    unique() 

# ---- WRITE DATA TO static-data ----
clean <- function(df){
    df %>%
        format(scientific = FALSE) %>%
        as_data_frame()
}
path_to_static <- "~/code/analytics/ecommerce-performance/static-data/"
write_csv(clean(fb), paste0(path_to_static, "fb.csv"), na = "")
write_csv(clean(ga), paste0(path_to_static, "ga.csv"), na = "")
write_csv(clean(ga_fb), paste0(path_to_static, "ga_fb.csv"), na = "")
write_csv(clean(upper_products_campaigns), paste0(path_to_static, "upper_products_campaigns.csv"), na = "")