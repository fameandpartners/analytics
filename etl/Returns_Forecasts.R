source("~/Documents/Code/R/fp_init.R")

item_returns <- tbl(fp_con, sql("SELECT * FROM item_returns")) %>% collect()
