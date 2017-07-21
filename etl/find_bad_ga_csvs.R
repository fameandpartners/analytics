library(readr)
library(dplyr)
library(stringr)
library(tidyr)

path_to_marketing_dropbox <- "/Users/Peter 1/Dropbox (Team Fame)/data/marketing/"

read_ga_with_filename <- function(filesource){
    read_csv(
        filesource,
        skip = 6,
        col_types = cols(
            .default = col_number(),
            Source = col_character(),
            Medium = col_character(),
            Campaign = col_character(),
            `Session Duration` = col_character(),
            `Avg. Session Duration` = col_character(),
            Date = col_date(format = "%Y%m%d"))) %>% 
        mutate(filename = filesource)
}

ga <- lapply(
    paste0(path_to_marketing_dropbox, "google_analytics/",
           list.files(path = paste0(path_to_marketing_dropbox, 
                                    "google_analytics"), 
                      pattern="*.csv")),
    read_ga_with_filename) %>%
    bind_rows()

utm_corrections <- read_csv("~/data/UTM Corrections - Sheet1.csv")

ga %>% 
    rename(utm = Campaign) %>%
    inner_join(utm_corrections %>%
                   rename(utm = `UTM Was`),
               by = "utm") %>%
    count(utm, filename)