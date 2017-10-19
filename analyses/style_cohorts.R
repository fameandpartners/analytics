library(tidyr)
library(dplyr)
library(readr)
library(dbscan)

dw <- src_postgres(dbname = "dw_dev",host = "localhost")

collections <- read_csv("~/data/collections.csv") %>%
    rename(Collection = collection_na) %>%
    inner_join(read_csv("~/data/Cohort Collections.csv"),
               by = "Collection")

sales <- tbl(dw, "sales") %>%
    filter(order_date >= "2016-01-01") %>%
    collect()

products <- tbl(dw, "products") %>%
    select(product_id, style_number) %>%
    collect() %>%
    left_join(collections %>%
                  transmute(product_id, `Collection Cohort` = Cohort),
               by = "product_id")

style_cohorts <- sales %>%
    inner_join(products %>% select(-style_number), 
               by = "product_id") %>%
    filter(!is.na(assigned_cohort)) %>%
    group_by(style_number, assigned_cohort) %>%
    summarise(n = sum(quantity)) %>%
    mutate(perc = n / sum(n)) %>% select(-n) %>%
    spread(assigned_cohort, perc, fill = 0) %>%
    ungroup() %>%
    left_join(products %>% select(style_number, `Collection Cohort`), 
              by = "style_number") %>%
    left_join(sales %>%
                  group_by(style_number) %>%
                  summarise(Units = sum(quantity)),
              by = "style_number") %>%
    rename(`Style Number` = style_number)

db <- dbscan(style_cohorts[,2:4], eps=0.14)

style_cohorts$Cluster = db$cluster

style_cohorts %>%
    group_by(Cluster) %>%
    summarise(styles = n(),
              Units = sum(Units),
              mean(Bridal),
              mean(Contemporary),
              mean(Prom))

adjusted_cohorts <- style_cohorts %>%
    mutate(`Adj. Cluster` = ifelse(Cluster %in% c(0,1,4), "Half Prom", 
                                ifelse(Cluster == 2, "Pure Contemporary",
                                       ifelse(Cluster == 3, "Pure Bridal", NA)))) %>%
    select(-Cluster)

adjusted_cohorts %>%
    group_by(`Adj. Cluster`) %>%
    summarise(styles = n(),
              Units = sum(Units),
              Bridal = mean(Bridal),
              Contemporary = mean(Contemporary),
              Prom = mean(Prom)) %>%
    write_csv("~/data/cohort_demand_clusters.csv")

table(adjusted_cohorts$`Collection Cohort`,
      adjusted_cohorts$`Adj. Cluster`)

adjusted_cohorts %>%
    inner_join(tbl(dw, "products") %>%
                   select(style_number, live) %>%
                   rename(`Style Number` = style_number) %>%
                   collect(),
               by = "Style Number") %>%
    left_join(sales %>% 
                  transmute(`Style Number` = style_number, dress_image_tag) %>%
                  unique(),
              by = "Style Number") %>% 
    select(-Bridal, -Contemporary, -Prom, -live) %>%
    write_csv("~/data/style_cohort_assignments.csv", na = "")
