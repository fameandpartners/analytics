library(dplyr)
library(tidyr)
library(stringr)
library(readr)

setwd("~/code/analytics/ecommerce-performance/")
source("~/code/analytics/ecommerce-performance/global.R")
setwd("~/data")

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
               ifelse(fabric_title == "Main:", "main", "other"))))))))) %>%
    select(product_id, fabric_column, fabric_value) %>%
    replace(is.na(.), "other") %>%
    unique() %>%
    spread(fabric_column, fabric_value) %>%
    select(product_id, main, lining_1, lining_2, contrast_1, contrast_2,
           contrast_3, trim) %>%
    left_join(fabrics %>%
                  transmute(product_id, 
                            original_fabric_description = fabric_description),
              by = "product_id")

write_csv(cleaned_fabrics, "~/data/cleaned_fabrics.csv", na = "")
