setwd("~/code/analytics/ecommerce-performance/")
source("~/code/analytics/ecommerce-performance/global.R")
setwd("~/data")

returns <- tbl(fp_con, "item_returns") %>%
    select(id, requested_at, refunded_at, line_item_id, refund_amount, comments, uuid,
           reason_category, reason_sub_category, acceptance_status, order_paid_currency) %>%
    filter(line_item_id %in% ordered_units$line_item_id) %>%
    collect()

r1 <- read_csv("warehouse-returns-1.csv")
r2 <- read_csv("warehouse-returns-2.csv")
br <- read_csv("bad returns.csv")

all_warehouse <- bind_rows(list(
        r1 %>% 
            rename(order_number = `PO Number`) %>%
            select(order_number) %>%
            unique(),
        r2 %>% 
            rename(order_number = Reference1) %>%
            select(order_number) %>%
            unique(),
        br %>%
            select(order_number) %>%
            unique()
    )) %>% unique() %>%
    mutate(on2 = order_number %>%
               str_replace_all("RA#", "R") %>% 
               str_replace_all("RR","R") %>%
               str_replace_all("A#", "")) %>%
    separate(on2, c("on3","bs"), sep = "/", extra = "merge") %>%
    select(-order_number, -bs) %>%
    rename(order_number = on3)

order_summary <- products_sold %>%
    left_join(returns, by = "line_item_id") %>%
    group_by(order_number) %>%
    summarise(return_requested = max(return_requested) %>% as.logical(),
              item_returned = max(item_returned) %>% as.logical(),
              order_date = min(order_date),
              ship_date = max(ship_date),
              net_sales = sum(sales_usd),
              gross_rev = sum(gross_revenue_usd),
              return_comments = paste(coalesce(unique(comments), ""), collapse = "-|-"))

matched_returns <- order_summary %>%
    inner_join(all_warehouse, by = "order_number") 

matched_returns %>%
    filter(return_requested & year(ship_date) == 2017 & month(ship_date) < 4) %>%
    select(-return_requested) %>%
    group_by(`Ship Month` = month(ship_date, label = TRUE), 
             `Return Processed` = item_returned) %>%
    summarise(Orders = n_distinct(order_number),
              `Net Sales (USD)` = sum(net_sales) %>% dollar(),
              `Revenue (USD)` = sum(gross_rev) %>% dollar())# %>%
    #write_csv("missing returns summary.csv", na = '')

missing_return_details <- matched_returns %>%
    group_by(order_number) %>%
    summarise(return_comments = paste(coalesce(unique(return_comments), ""), collapse = "--|--")) %>%
    inner_join(products_sold, by = "order_number") %>%
    group_by(order_number) %>%
    summarise(return_requested = max(return_requested) %>% as.logical(),
              item_returned = max(item_returned) %>% as.logical(),
              order_date = max(order_date), 
              ship_date = max(ship_date), 
              net_sales = sum(sales_usd),
              gross_revenue = sum(gross_revenue_usd),
              return_comments = paste(coalesce(unique(return_comments), ""), collapse = "---|---")) %>%
    filter(return_requested & !item_returned & year(order_date) == 2017 & month(order_date) < 4) %>%
    mutate(has_comments = str_detect(return_comments, "[a-zA-Z]"),
           has_keywords = str_detect(tolower(return_comments), "\\$|refund|exchange")) %>%
    rename(return_comments_rm = return_comments) %>%
    mutate(return_comments = ifelse(!has_comments, NA, return_comments_rm)) %>%
    select(-return_comments_rm)

#write_csv(missing_return_details, "missing returns details.csv", na = '')

# Pull refund amount from comments
comments_refund_amount <- returns %>%
    filter(!is.na(comments) & requested_at >= as.Date("2017-01-01")) %>%
    mutate(refund_amount_from_comments_str = comments %>% 
               str_extract("\\$(.*) ") %>%
               str_replace_all("\\$","") %>%
               str_extract("[0-9]{1,6}.[0-9]{0,2}") %>%
               str_trim()) %>%
    filter(!is.na(refund_amount_from_comments_str)) %>%
    mutate(refund_amount_from_comments = as.numeric(refund_amount_from_comments_str),
           refund_amount_for_update = refund_amount_from_comments * 100)

# Pull refund amounts from return events
return_events <- tbl(fp_con, "item_return_events") %>%
    filter(event_type == "refund") %>%
    collect() %>%
    mutate(refund_amount_from_event_data = data %>% 
               str_extract("[0-9]{1,6}.[0-9]{0,2}") %>%
               str_replace_all("\\'","") %>%
               as.numeric()) %>%
    rename(uuid = item_return_uuid) %>%
    select(-id)

events_refund_amount <- returns %>%
    filter(is.na(refund_amount)) %>%
    inner_join(return_events, by = "uuid") %>%
    mutate(refund_amount_for_update = refund_amount_from_event_data * 100)

refund_amount_updates <- bind_rows(list(
    comments_refund_amount %>%
        filter(id != 10895) %>% # Bad record
        select(id, refund_amount_for_update),
    events_refund_amount %>%
        select(id, refund_amount_for_update)))

write_csv(refund_amount_updates, "item_returns updates 2017-04-28.csv")

refund_amount_updates %>%
    inner_join(returns, by = "id") %>%
    inner_join(products_sold, by = "line_item_id") %>%
    filter(!duplicated(line_item_id)) %>%
    group_by(ship_year_month) %>%
    summarise(sum((refund_amount_for_update * conversion_rate) / 100))

# ---- Returns Missing from Warehouses ----
order_summary %>%
    filter(return_requested & item_returned 
           & year(order_date) == 2017 & month(order_date) < 4) %>%
    anti_join(all_warehouse, by = "order_number") #%>%
    #$write_csv("Returns Missing From Warehouse.csv")