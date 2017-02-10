library(readr)
library(RPostgreSQL)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(scales)
library(ggplot2)
library(shiny)

source("fp_init.R")

returns <- tbl(fp_con, sql(paste(
    "SELECT",
        "ir.reason_category,",
        "ir.reason_sub_category,",
        "SUM(li.quantity) items",
    "FROM item_returns ir",
    "INNER JOIN spree_line_items li",
        "ON li.id = ir.line_item_id",
    "INNER JOIN spree_orders o",
        "ON o.id = li.order_id",
    "WHERE o.completed_at IS NOT NULL ",
        "AND ir.requested_action = 'return'",
        "AND o.completed_at >= CURRENT_DATE - INTERVAL '12 months'",
    "GROUP BY",
        "ir.reason_category,",
        "ir.reason_sub_category"))) %>%
    collect() %>%
    mutate(sub_reason = 
               ifelse(is.na(reason_sub_category) | str_detect(tolower(reason_sub_category),"not state|reason|n/a|na|cancel|not specifelseied|charged twice|not spec"),"No Reason",
               ifelse(str_detect(tolower(reason_sub_category), "bust"),"Bust",
               ifelse(str_detect(tolower(reason_sub_category),"waist"),"Waist", 
               ifelse(str_detect(tolower(reason_sub_category), "long|short|length"), "Length", 
               ifelse(str_detect(tolower(reason_sub_category), "hips"), "Hips", 
               ifelse(str_detect(tolower(reason_sub_category), "fit|small|big|size|too low|tight|on the body"), "Fit",
               ifelse(str_detect(tolower(reason_sub_category),"late|delivery|ship|transit|received part|event"),"Shipment",
               ifelse(str_detect(tolower(reason_sub_category),"difficult to choose|not sure"), "Difficult to Choose", 
               ifelse(str_detect(tolower(reason_sub_category),"expectations|not clear|difelseferent|does not match|on the website|wrong"), "Missed Expectations", 
               ifelse(str_detect(tolower(reason_sub_category),"poor quality|stain|poorly made|damaged|difelseferent style|marks|customisation|cheap"), "Quality", 
               "No Reason")))))))))))  %>%
    rename(Reason = reason_category, `Sub Reason` = sub_reason)

returns$`Sub Reason` <- factor(
    returns$`Sub Reason`,
    levels = c("Fit","Length","Bust","Waist","Hips","Quality","Shipment","Missed Expectations","Difficult to Choose","No Reason")
)

returns %>%
    ggplot(aes(x = Reason, y = items, fill = `Sub Reason`)) +
    geom_bar(stat = "identity", position = "fill") +
    theme(axis.title.y = element_blank()) +
    scale_y_continuous(labels = percent)