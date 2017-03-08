withProgress(message = 'Downloading Returns', value = 0, {
    incProgress(1 / 10, detail = "Querying Line Item Customiztations")
    
    line_item_personalizations <- 
        tbl(fp_con, "line_item_personalizations") %>%
        collect(n = Inf)
    
    incProgress(2 / 10, detail = "Parsing Customiztation Data")
    
    line_item_personalization_customization_values <-
        line_item_personalizations %>% 
        filter(str_detect(customization_value_ids, "[0-9]")) %>% 
        mutate(parsed_ids = 
                   str_replace_all(customization_value_ids, "\n|'| |---", "") %>% 
                   substr(2, 20)) %>% 
        select(id, parsed_ids) %>%
        separate(parsed_ids, sep = "-", c("id1","id2","id3","id4","id5"), 
                 extra = "warn", fill = "right") %>% 
        gather(which_cust_id, customization_value_id_char, -id, na.rm = TRUE) %>%
        rename(line_item_personalization_id = id) %>%
        transmute(id = row_number(),
                  line_item_personalization_id,
                  customization_value_id = as.integer(customization_value_id_char))
    
    incProgress(3 / 10, detail = "Querying Customization Lookups")
    
    customization_values <- collect(tbl(fp_con, "customisation_values"))
    
    incProgress(4 / 10, detail = "Querying Returns")
    
    returns <- tbl(fp_con, sql(paste(
        "SELECT",
        "ir.line_item_id,",
        "li.order_id,",
        "ir.order_number,",
        "ir.refund_amount / 100 refund_amount,",
        "li.price * li.quantity original_amount,",
        "ir.customer_name,",
        "ir.contact_email,",
        "ir.product_name,",
        "ir.reason_category,",
        "ir.reason_sub_category,",
        "ir.request_notes,",
        "ir.refund_status,",
        "lip.color,",
        "lip.size,",
        "lip.height,",
        "lip.line_item_personalization_id,",
        "o.completed_at::date order_date,",
        "s.shipment_date,",
        "ir.refunded_at::date refund_date",
        "FROM item_returns ir",
        "INNER JOIN spree_line_items li",
        "ON li.id = ir.line_item_id",
        "INNER JOIN spree_orders o",
        "ON o.id = li.order_id INNER JOIN (",
        "SELECT",
        "order_id,",
        "MAX(shipped_at::date) shipment_date",
        "FROM spree_shipments",
        "GROUP BY order_id) s",
        "ON s.order_id = o.id LEFT JOIN (",
        "SELECT",
        "line_item_id,",
        "STRING_AGG(DISTINCT color, ',') color,",
        "STRING_AGG(DISTINCT size, ',') size,",
        "STRING_AGG(DISTINCT height, ',') height,",
        "MIN(id) line_item_personalization_id",
        "FROM line_item_personalizations",
        "GROUP BY line_item_id) lip",
        "ON lip.line_item_id = li.id",
        "WHERE o.completed_at IS NOT NULL",
        "AND ir.requested_action = 'return'",
        "AND o.completed_at >= '2016-06-01'"))) %>%
        collect()
    
    incProgress(5 / 10, detail = "Wangling Data")
    
    customizations <- 
        line_item_personalizations %>%
        inner_join(line_item_personalization_customization_values, 
                   by = c("id" = "line_item_personalization_id")) %>%
        inner_join(customization_values, by = c("customization_value_id" = "id")) %>%
        group_by(line_item_id) %>%
        summarise(customizations = paste(unique(presentation), collapse = ", "))
    
    incProgress(6 / 10, detail = "Preparing Download")
    
    bulk_export <- returns %>%
        left_join(customizations, by = "line_item_id")
})