library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# ---- GOOGLE ANALYTICS ----
ga <- lapply(
    paste0("~/data/marketing/google_analytics/",
           list.files(path = "~/data/marketing/google_analytics", pattern="*.csv")),
    read_csv,
    skip = 6,
    col_types = cols(
        .default = col_number(),
        Source = col_character(),
        Medium = col_character(),
        Campaign = col_character(),
        Date = col_date(format = "%Y%m%d"))) %>%
    bind_rows() %>%
    mutate(utm_campaign = Campaign,
           New_Sessions = `% New Sessions` * Sessions,
           Page_Views = Clicks / (CTR / 100),
           ga_id = row_number()) %>%
    group_by(Date, utm_campaign) %>%
    summarise(Page_Views = sum(coalesce(Page_Views, 0)),
              Sessions = sum(coalesce(Sessions, 0)),
              New_Sessions = sum(coalesce(New_Sessions, 0)),
              Bounces = sum(coalesce(Bounces, 0)),
              Clicks = sum(coalesce(Clicks, 0)),
              Cost = sum(coalesce(Cost, 0)),
              Transactions = sum(coalesce(Transactions, 0)),
              Revenue = sum(coalesce(Revenue, 0))) %>%
    mutate(`% New Sessions` = coalesce(100 * (New_Sessions / Sessions), 0),
           Bounce_Rate = coalesce(100 * (Bounces / Sessions), 0),
           CPC = coalesce(100 * (Cost / Clicks), 0),
           CTR = coalesce(Clicks / Page_Views, 0)) %>%
    select(-Page_Views)

# ---- FACEBOOK ----
fb <- lapply(
    paste0("~/data/marketing/facebook/",
           list.files(path = "~/data/marketing/facebook", pattern="*.csv")),
    read_csv, 
    col_types = cols(
        .default = col_number(),
        `Reporting Starts` = col_date(),
        `Reporting Ends` = col_date(),
        `Ad Name` = col_character(),
        `Campaign Name` = col_character(),
        `Ad Set Name` = col_character())) %>%
    bind_rows() %>%
    mutate(utm_campaign = `Ad Name`,
           Date = `Reporting Starts`,
           Unique_Clicks = coalesce(`Amount Spent (AUD)` / `Cost per Unique Click (All) (AUD)`, 0),
           fb_id = row_number()) %>%
    filter(Impressions > 0 & Reach > 0) %>%
    # Remove these aggregates after data cleaned
    group_by(Date, utm_campaign) %>%
    summarise(Reach = sum(Reach),
              Impressions = sum(Impressions),
              Amount_Spent_AUD = sum(coalesce(`Amount Spent (AUD)`, 0)),
              Unique_Clicks = sum(coalesce(Unique_Clicks, 0)),
              Unique_Link_Clicks = sum(coalesce(`Unique Link Clicks`, 0)),
              Adds_to_Cart = sum(coalesce(`Website Adds to Cart`, 0)),
              Conversions = sum(coalesce(`Website Conversions`, 0)),
              Purchases  = sum(coalesce(`Website Purchases`, 0)),
              Post_Shares = sum(coalesce(`Post Shares`, 0)),
              Post_Comments = sum(coalesce(`Post Comments`, 0)),
              Post_Reactions = sum(coalesce(`Post Reactions`, 0))) %>%
    ungroup() %>%
    mutate(Frequency = Impressions / Reach,
           Unique_CTR = coalesce(Unique_Clicks / Reach, 0) * 100,
           Unique_CPC = coalesce(Amount_Spent_AUD / Unique_Clicks, 0) * 100,
           fb_id = row_number()) %>%
    replace(. == Inf, 0)

initcap <- function(string){
    require(dplyr)
    paste0(string %>% substr(1, 1) %>% toupper(),
           string %>% substr(2, 100000) %>% tolower())
}

# ---- MERGE GA & FB ----
ga_fb <- fb %>% 
    inner_join(ga, by = c("utm_campaign","Date")) %>%
    separate(utm_campaign, into = c('cohort','country','region','age','target',
                                    'device_type','creative_type','creative_strategy','theme','ad_format',
                                    'pic_source','copy_type','landing_page','product_category','products'), 
             sep = "_", extra = "merge", remove = FALSE) %>%
    mutate(creative = paste(initcap(creative_type), initcap(creative_strategy), initcap(theme),
                            initcap(ad_format), initcap(pic_source), initcap(copy_type)),
           prospecting = !str_detect(cohort, "-RE"))

# ---- PULL UTM CAMPAIGN TO PRODUCT NAME LOOKUP ----
upper_products_campaigns <- ga_fb %>% 
    select(utm_campaign, C15) %>%
    separate(C15, sep = "-", into = paste0("A", 1:7)) %>%
    gather(AX, product, -utm_campaign) %>%
    filter(!is.na(product) & product != "NA") %>%
    select(-AX) %>%
    unique() 

# ---- WRITE DATA TO static-data ----
path_to_static <- "~/code/analytics/ecommerce-performance/static-data/"
write_csv(fb, paste0(path_to_static, "fb.csv"), na = "")
write_csv(ga, paste0(path_to_static, "ga.csv"), na = "")
write_csv(ga_fb, paste0(path_to_static, "ga_fb.csv"), na = "")
write_csv(upper_products_campaigns, paste0(path_to_static, "upper_products_campaigns.csv"), na = "")