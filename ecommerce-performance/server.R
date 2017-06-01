shinyServer(function(input, output) {
 # ---- Styles Tab ----
    # ---- Styles Data Set Filters ----
    
    collection_filter <- reactive({
        if(length(input$collections) > 0) {
            input$collections
        } else { unique(products_sold$collection) }
    })
    
    order_status_filter <- reactive({
        if(length(input$order_status) > 0) {
            input$order_status
        } else { unique(products_sold$order_status) }
    })
    
    taxon_filter <- reactive({
        if(length(input$taxons) > 0){
            taxon_product_ids <- product_taxons %>%
                filter(taxon_name %in% input$taxons) %>%
                select(product_id)
            return(unique(taxon_product_ids$product_id))
        } else { unique(products_sold$product_id) }
    })
    
    assigned_cohort_filter <- reactive({
        if(length(input$assigned_cohort)){
            input$assigned_cohort
        } else { unique(products_sold$assigned_cohort) }
    })
    
    filtered_sales <- reactive({
        sales <- products_sold %>%
            filter(between(order_date, input$order_dates[1], input$order_dates[2])) %>%
            filter(collection %in% collection_filter()) %>%
            filter(between(price_usd, input$price_range[1], input$price_range[2])) %>%
            filter(between(us_size, input$us_size[1], input$us_size[2])) %>%
            filter(order_status %in% order_status_filter()) %>%
            filter(product_id %in% taxon_filter()) %>%
            filter(assigned_cohort %in% assigned_cohort_filter())
        return(sales)
    })
    
    # ---- Top Styles ----
    style_ranking_data <- reactive({
        filtered_sales() %>%
            filter(sales_usd > 0) %>%
            mutate(net_sales = gross_revenue_usd + adjustments_usd,
                   cogs = manufacturing_cost + li_shipping_cost + payment_processing_cost) %>%
            group_by(style_number) %>%
            summarise(`Style Name` = paste(unique(style_name), collapse = ","),
                      `Dress Image` = dress_image_tag[1],
                      Units = sum(quantity),
                      Sales = sum(sales_usd),
                      ASP = mean(sales_usd),
                      `Refund Request Rate` = sum(sales_usd * return_requested) / sum(sales_usd),
                      `Customization Rate` = sum(quantity * physically_customized) / sum(quantity),
                      Margin = (sum(net_sales) - sum(cogs)) / sum(net_sales)) %>%
            arrange(desc(Sales))
    })
    
    output$style_ranking <- renderDataTable({
        # Rename style number here so that selected_product_ids can work
        style_ranking_data() %>%
            rename(`Style Number` = style_number) %>%
            datatable(class = "hover row-border", style = "bootstrap", escape = FALSE,
                      options = list(lengthMenu = c(5, 10, 50), pageLength = 5)) %>%
            formatCurrency(c("Units"), digits = 0, currency = "") %>%
            formatCurrency(c("Sales","ASP")) %>%
            formatPercentage(c("Refund Request Rate",
                               #"Return Rate", 
                               "Customization Rate",
                               "Margin"))
    })
    
    output$style_ranking_down <- downloadHandler(
        filename = function() { paste("Top Styles ", today(), ".csv", sep='') },
        content = function(file) {
            write_csv(style_ranking_data() %>% 
                          rename(`Style Number` = style_number) %>%
                          select(-`Dress Image`), 
                      file, na = "")
        })
    
    # Filter for products selected in the datatable
    selected_product_ids <- reactive({
        style_numbers <- style_ranking_data()[input$style_ranking_rows_selected,]$style_number
        selected_df <- 
            filtered_sales() %>%
            filter(style_number %in% style_numbers)
        unique(selected_df$product_id)
    })
    
    selected_sales <- reactive({
        if(length(selected_product_ids()) > 0){
            filtered_sales() %>%
                filter(product_id %in% selected_product_ids())
        } else {
            filtered_sales()
        }
    })
    
    # ---- KPIs ----
    output$kpis <- renderTable({
        selected_sales() %>%
            filter(sales_usd > 0) %>%
            group_by(order_id) %>%
            mutate(return_request_amount = sales_usd * return_requested,
                   net_revenue = gross_revenue_usd + adjustments_usd,
                   cogs = coalesce(manufacturing_cost, 70) + li_shipping_cost + payment_processing_cost) %>%
            summarise(quantity = sum(quantity), 
                      sales_usd = sum(sales_usd), 
                      refunds_requested_usd = sum(return_request_amount),
                      refund_amount_usd = sum(coalesce(refund_amount_usd, 0)),
                      physically_customized = max(physically_customized),
                      net_revenue = sum(net_revenue),
                      cogs = sum(cogs)) %>%
            summarise(`Total Units` = short_number(sum(quantity)),
                      `Total Sales` = short_dollar(sum(sales_usd)),
                      ASP = dollar(sum(sales_usd) / sum(quantity)),
                      `AOV` = short_dollar(mean(sales_usd)),
                      `Refund Request Rate` = round(sum(refunds_requested_usd) / sum(sales_usd), 2) %>% percent(),
                      `Customization Rate` = round(sum(quantity * physically_customized) / sum(quantity), 2) %>% percent(),
                      `Margin` = ((sum(net_revenue) - sum(cogs)) / sum(net_revenue)) %>% percent())
    })
    
    # ---- Daily Sales ----
    
    daily_sales_data <- reactive({
        selected_sales() %>%
            group_by(order_date, order_status) %>%
            summarise(order_week_start = min(order_date),
                      `Sales (USD)` = sum(sales_usd))
    })
    
    output$daily_sales <- renderPlot({
        daily_sales_data() %>%
            ggplot(aes(x = order_date, y = `Sales (USD)`, fill = order_status)) +
            geom_bar(stat = "identity", color = "black", size = 0.2) +
            scale_y_continuous(labels = short_dollar) +
            theme_bw(base_size = 14) +
            theme(axis.title.x = element_blank(),
                  legend.title = element_blank()) +
            scale_fill_brewer(palette = "Set3")
    })
    
    # ---- Top Colors ---- 
    
    top_colors_data <- reactive({
        top_colors <- 
            selected_sales() %>%
            filter(!is.na(color)) %>%
            count(color) %>%
            top_n(10, n) %>%
            arrange(n)
        
        top_colors$color <- factor(
            top_colors$color,
            levels = top_colors$color
        )
        
        return(top_colors)
    })
    
    output$top_colors <- renderPlot({
        top_colors_data() %>%
            rename(Color = color, Units = n) %>%
            ggplot(aes(x = Color, y = Units)) +
            geom_bar(stat = "identity") +
            theme_bw(base_size = 14) +
            coord_flip() +
            theme(axis.title.y = element_blank())
    })

    # ---- Download All Data ----
    output$download_all <- downloadHandler(
        filename = function() { paste("eCommerce Performance Details ", today(), ".csv", sep='') },
        content = function(file) {
            write_csv(selected_sales() %>%
                          select(
                              line_item_id,
                              order_id,
                              order_number,
                              order_state,
                              quantity,
                              price,
                              sales_usd,
                              currency,
                              ship_city,
                              ship_state,
                              ship_country,
                              order_date,
                              ship_date,
                              email,
                              user_id,
                              customer_name,
                              product_id,
                              style_name,
                              style_number,
                              order_status,
                              return_requested,
                              return_reason,
                              reason_sub_category,
                              color,
                              size,
                              collection
                          ), 
                      file, na = "")
        }
    )
    
    # ---- Weekly Customization Rates ----
    output$cust_rates <- renderPlot({
        selected_sales() %>%
            mutate(order_year_week = paste(year(order_date), 
                                           formatC(week(order_date), width = 2, flag = "0"), 
                                           sep = " W")) %>%
            group_by(order_year_week) %>% 
            summarise(`Week Ending` = as.character(max(order_date)),
                      Units = sum(physically_customized * quantity) / sum(quantity)) %>% 
            ggplot(aes(x = `Week Ending`, y = Units)) + 
            geom_bar(stat = "identity") +
            scale_y_continuous(labels = percent) +
            theme_bw(base_size = 14) +
            theme(legend.title = element_blank(),
                  axis.text.x = element_text(hjust = 1, angle = 35))
    })
    
    # ---- Size Distribution ----
    output$size_dist <- renderPlot({
        old_heights <- selected_sales() %>%
            filter(height %in% c("Petite", "Standard", "Tall"))
        if(nrow(old_heights) > 1){
            selected_sales() %>%
                filter(height %in% c("Petite", "Standard", "Tall")) %>%
                group_by(height, us_size) %>%
                summarise(Units = sum(quantity)) %>%
                arrange(desc(us_size)) %>%
                ggplot(aes(x = us_size, y = Units)) +
                geom_bar(stat = "identity") +
                scale_x_continuous(breaks = seq(min(selected_sales()$us_size), 
                                                max(selected_sales()$us_size), 2)) +
                theme_bw(base_size = 14) +
                #coord_flip() +
                xlab("Size (US)") +
                facet_grid(height ~ .)
        } else {
            selected_sales() %>%
                group_by(us_size) %>%
                summarise(Units = sum(quantity)) %>%
                arrange(desc(us_size)) %>%
                ggplot(aes(x = us_size, y = Units)) +
                geom_bar(stat = "identity") +
                scale_x_continuous(breaks = seq(min(selected_sales()$us_size), 
                                                max(selected_sales()$us_size), 2)) +
                theme_bw(base_size = 14) +
                #coord_flip() +
                xlab("Size (US)")
        }
    })
    
 # ---- Returns Tab ----
    return_collections_filter <- reactive({
        if(length(input$collections_r) > 0) {
            input$collections_r
        } else { unique(products_sold$collection) }
    })
    
    return_styles_filter <- reactive({
        if(length(input$style_r) > 0) {
            input$style_r
        } else { unique(products_sold$style_name) }
    })
    
    return_heights_filter <- reactive({
        if(length(input$height_r) > 0) {
            input$height_r
        } else { unique(products_sold$height) }
    })
    
    filtered_for_returns <- reactive({
        products_sold %>%
            filter(between(ship_date, input$ship_dates_r[1], input$ship_dates_r[2])) %>%
            filter(collection %in% return_collections_filter()) %>%
            filter(style_name %in% return_styles_filter()) %>%
            filter(height %in% return_heights_filter()) %>%
            filter(between(us_size, input$us_size_r[1], input$us_size_r[2]))
    })
    
    filtered_returns <- reactive({
        filtered_for_returns() %>% filter(return_requested)
    })
    
    # ---- Return Reasons ----
    return_reasons_data <- reactive({
        filtered_returns() %>%
            filter(!is.na(return_reason)) %>%
            group_by(return_reason) %>%
            summarise(Units = n())
    })
    
    output$return_reasons <- renderPlot({
        returns_df <- return_reasons_data()
        
        if(nrow(returns_df) > 0){
            returns_df %>%
                ggplot(aes(x = "", y = Units, fill = return_reason)) +
                geom_bar(stat = "identity", position = "fill", color = "black", size = 0.2) +
                coord_polar("y", start = 0) +
                theme_minimal(base_size = 14) +
                theme(axis.text = element_blank(),
                      axis.title = element_blank(),
                      legend.title = element_blank(),
                      legend.position = "left") +
                scale_fill_brewer(palette = "Set3")
        } else {
            ggplot(data_frame(x = c(NA)), aes(x = x)) + 
                ggtitle("No Returns") + 
                theme_minimal() + 
                theme(axis.title = element_blank(), 
                      axis.text = element_blank())
        }
    })
    
    output$return_reasons_down <- downloadHandler(
        filename = function() { paste0("Return Reasons ", today(), ".csv") },
        content = function(file) { write_csv(return_reasons_data(), file, na = "") }
    )
    
    # --- Secondary Return Reasons ----
    sec_return_reasons_data <- reactive({
        filtered_returns() %>%
            group_by(reason_sub_category) %>%
            summarise(Units = n()) %>%
            mutate(ranking = rank(-Units),
                   sec_reason = ifelse(ranking < 10, reason_sub_category, "Other Reason")) %>%
            group_by(sec_reason) %>%
            summarise(Units = sum(Units)) %>%
            arrange(desc(Units)) %>%
            ungroup()
    })
    
    output$sec_return_reasons <- renderPlot({
        returns_df <- sec_return_reasons_data()
    
        returns_df$sec_reason <- factor(
            returns_df$sec_reason,
            levels = c(returns_df$sec_reason[returns_df$sec_reason != "Other Reason"], "Other Reason")
        )
        
        if(nrow(returns_df) > 0){
            returns_df %>%
                ggplot(aes(x = "", y = Units, fill = sec_reason)) +
                geom_bar(stat = "identity", position = "fill", color = "black", size = 0.2) +
                coord_polar("y", start = 0) +
                theme_minimal(base_size = 14) +
                theme(axis.text = element_blank(),
                      axis.title = element_blank(),
                      legend.title = element_blank(),
                      legend.position = "left") +
                scale_fill_brewer(palette = "Set3")
        } else {
            ggplot(data_frame(x = c(NA)), aes(x = x)) + 
                ggtitle("No Returns") + 
                theme_minimal() + 
                theme(axis.title = element_blank(), 
                      axis.text = element_blank())
        }
    })
    
    output$sec_return_reasons_down <- downloadHandler(
        filename = function() { paste0("Secondary Return Reasons ", today(), ".csv") },
        content = function(file) { write_csv(
            filtered_returns() %>%
                group_by(reason_sub_category) %>%
                summarise(Units = n()) %>%
                arrange(desc(Units)) %>%
                mutate(`Percent of Total` = Units / sum(Units)) %>%
                rename(`Secondary Return Reason` = reason_sub_category), 
            file, na = "") }
    )
    
    # ---- Primary and Secondary Return Reason Details ----
    reason_details_data <- reactive({
        filtered_returns() %>%
            group_by(`Primary Return Reason` = return_reason, 
                     `Secondary Return Reason` = reason_sub_category) %>% 
            summarise(Units = n(),
                      `Revenue (USD)` = sum(sales_usd)) %>%
            arrange(desc(Units))
    })
    
    output$reason_details <- renderDataTable({
        reason_details_data() %>%
            datatable(class = "hover row-border", style = "bootstrap", 
                      rownames = FALSE, selection = "none") %>%
            formatCurrency(c("Units"), digits = 0, currency = "") %>%
            formatCurrency(c("Revenue (USD)"))
    })
    
    output$reason_details_down <- downloadHandler(
        filename = function() { paste0("Secondary Return Reasons ", today(), ".csv") },
        content = function(file){ write_csv(reason_details_data(), file, na = "")}
    )
    
    # ---- Monthly Return Rates ----
    return_rates_data <- reactive({
        filtered_for_returns() %>% 
            filter(order_status %in% c("Shipped","Refund Requested","Returned")) %>%
            group_by(ship_year_month) %>% 
            summarise(`Gross Revenue` = sum(gross_revenue_usd),
                      `Processed Returns` = sum(coalesce(refund_amount_usd, 0)),
                      `Requested Returns` = sum(return_requested * gross_revenue_usd),
                      `Estimated Returns` = sum(ifelse(# See NOTES in global.R
                          ship_date >= today() - 90,
                          coalesce(refund_amount_usd, return_requested * sales_usd * 0.65),
                          coalesce(refund_amount_usd, 0))),
                      `Refund Request Rate` = `Requested Returns` / `Gross Revenue`,
                      `Return Rate` = `Processed Returns` / `Gross Revenue`)
    })
    output$monthly_return_rates <- renderPlot({
         return_rates_data() %>% 
            ggplot(aes(x = ship_year_month)) +
            geom_line(aes(y = `Return Rate`, color = "Return Rate"), group = 1) +
            geom_line(aes(y = `Refund Request Rate`, color = "Refund Request Rate"), group = 1) +
            scale_y_continuous(labels = percent, limits = c(0, max(return_rates_data()$`Refund Request Rate`)*1.1)) +
            theme_bw(base_size = 14) +
            theme(legend.title = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.x = element_text(hjust = 1, angle = 35)) +
            ylab("Revenue (USD)") +
            scale_color_brewer(palette = "Set1")
    })
    output$return_rates_down <- downloadHandler(
        filename = function() { paste0("Monthly Return Rates ", today(), ".csv") },
        content = function(file){ write_csv(return_rates_data(), file, na = "") }
    )
    # ---- Return Rates by Height and Length ----
    height_length_return_rate_data <- reactive({
        filtered_for_returns() %>%
            filter(!is.na(height) & !is.na(length)) %>%
            group_by(Height = height, Length = length) %>%
            summarise(Revenue = sum(gross_revenue_usd),
                      `Returns Requested` = sum(gross_revenue_usd * return_requested),
                      Returns = sum(coalesce(refund_amount_usd, 0))) %>%
            ungroup() %>%
            mutate(`Return Request Rate` = `Returns Requested` / Revenue,
                   `Return Rate` = Returns / Revenue) 
    })
    
    output$height_length_return_rate <- renderDataTable({
        height_length_return_rate_data() %>%
            datatable(class = "hover row-border", style = "bootstrap", 
                      rownames = FALSE, selection = "none") %>%
            formatPercentage(c("Return Request Rate","Return Rate")) %>%
            formatCurrency(c("Revenue","Returns Requested","Returns"))
    })
    
    # ---- Returns Export ----
    
    output$download_returns <- downloadHandler(
        filename = function() { paste("Returns Details Since Jun 2016 ", today(), ".csv", sep='') },
        content = function(file) {
            source("returns-bulk-export.R")
            write_csv(bulk_export, file, na = "")
        }
    )
    
# ---- Conversions Tab ----
    
    cohort_filter <- reactive({
        if(length(input$cohort_select) > 0) {
            input$cohort_select
        } else { unique(all_touches$cohort) }
    })
    
    utm_source_filter <- reactive({
        if(length(input$utm_source) > 0) {
            input$utm_source
        } else { unique(all_touches$utm_source) }
    })
    
    utm_medium_filter <- reactive({
        if(length(input$utm_medium) > 0) {
            input$utm_medium
        } else { unique(all_touches$utm_medium) }
    })
    
    utm_campaign_filter <- reactive({
        if(length(input$utm_campaign) > 0) {
            input$utm_campaign
        } else { unique(all_touches$utm_campaign) }
    })
    
    filtered_touches <- reactive({
        temp_df <- all_touches %>%
            filter(between(as.Date(added_to_cart_at), input$conversion_dates[1], input$conversion_dates[2])) %>%
            filter(cohort %in% cohort_filter()) %>%
            filter(utm_source %in% utm_source_filter()) %>%
            filter(utm_medium %in% utm_medium_filter()) %>%
            filter(utm_campaign %in% utm_campaign_filter())
        if(nchar(input$utm_campaign_search) > 0){
            temp_df %>%
                filter(str_detect(utm_campaign, input$utm_campaign_search))
        } else { temp_df }
    })
    
    # ---- Monthly Carts, Orders and Conversion Rates ----
    carts_orders_data <- reactive({
        filtered_touches() %>% 
            filter(!is.na(step)) %>% 
            group_by(cart_year_month, step) %>% 
            summarise(o = n_distinct(order_id)) %>% 
            arrange(cart_year_month, desc(step)) %>% 
            mutate(Orders = cumsum(o))
    })
    
    output$cart_to_purchase <- renderPlot({
        df <- carts_orders_data() %>%
            group_by(cart_year_month) %>%
            summarise(`Cart to Purchase` = min(Orders) / max(Orders))
        
        df$vjust <- rep(c(2, -2), length.out = nrow(df))
        
        if(nrow(df) > 0){
            ggplot(df, aes(x = cart_year_month, y = `Cart to Purchase`)) +
                geom_line(group = 1) +
                geom_point() +
                geom_text(aes(label = paste0(round(`Cart to Purchase` * 100), "%"), vjust = vjust), size = 5) +
                scale_y_continuous(labels = percent, 
                                   limits = c(min(df$`Cart to Purchase`)*(1 - 0.5),
                                              max(df$`Cart to Purchase`)*(1 + 0.5))) +
                theme_minimal(base_size = 14) +
                theme(axis.text.x = element_blank(),
                      axis.title.x = element_blank())
        }
    })
    
    output$carts_orders <- renderPlot({
        df <- carts_orders_data()
        if(nrow(df) > 0){
            ggplot(df, aes(x = cart_year_month, y = Orders, fill = step)) + 
                geom_bar(stat = "identity", position = "dodge") + 
                theme_minimal(base_size = 14) +
                scale_y_continuous(labels = short_number) +
                theme(axis.title.x = element_blank(),
                      legend.position = "bottom",
                      legend.title = element_blank()) +
                scale_fill_brewer(palette = "Set1")
        }
    })
    
    # ---- Revenue by UTM Source ----
    output$utm_rev <- renderPlot({
        top10 <- filtered_touches() %>%
            filter(!is.na(touch_time) & !is.na(ordered_at) & !is.na(utm_source) & step == "Purchase") %>% 
            group_by(order_id) %>% 
            filter(touch_time == max(touch_time)) %>% 
            arrange(order_id, touch_type) %>% 
            summarise(source = utm_source[1], revenue = sum(sales_usd)) %>% 
            group_by(Source = source) %>% 
            summarise(`Revenue (USD)` = sum(revenue)) %>%
            top_n(10, `Revenue (USD)`)
        
        top10$Source <- factor(
            top10$Source,
            levels = (top10 %>% arrange(`Revenue (USD)`))$Source
        )
        
        if(nrow(top10) > 0){
            ggplot(top10, aes(x = Source, y = `Revenue (USD)`)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = short_dollar(`Revenue (USD)`)), hjust = -0.05, size = 4) +
                scale_y_continuous(labels = short_dollar, limits = c(0, max(top10$`Revenue (USD)`)*1.25)) +
                coord_flip() +
                theme_bw(base_size = 14)
        }
    })
    
    # ---- Cohort Conversion Funnels ----
    output$cohort_conversions <- renderPlot({
        cohort_conversions <- filtered_touches() %>%
            group_by(cohort, step) %>%
            summarise(ord = n_distinct(order_id)) %>%
            arrange(cohort, desc(step)) %>%
            mutate(orders = cumsum(ord)) %>%
            arrange(cohort, step) %>%
            mutate(order_conversion = orders / max(orders))
        
        if(nrow(cohort_conversions) > 0){
            ggplot(cohort_conversions, 
                   aes(x = step, y = order_conversion)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = percent(order_conversion)), vjust = -0.75) +
                geom_text(aes(label = prettyNum(orders, big.mark = ",")), vjust = 1.6, color = "white") +
                scale_y_continuous(labels = percent) +
                facet_grid(.~ cohort) +
                theme_bw(base_size = 14) +
                theme(axis.title.x = element_blank()) +
                ylab("Orders")
        }
    })
    
    # ---- UTM Source Conversions ----
    output$source_conv <- renderPlot({
        utm_source_orders <- filtered_touches() %>%
            filter(!is.na(utm_source)) %>%
            group_by(utm_source, step) %>%
            summarise(ord = n_distinct(order_id))
        
        utm_source_conversions <- utm_source_orders %>%
            arrange(utm_source, desc(step)) %>%
            mutate(orders = cumsum(ord)) %>%
            arrange(utm_source, step) %>%
            mutate(order_conversion = orders / max(orders)) %>%
            replace(is.na(.), 1)
        
        top5_m <- utm_source_orders %>% 
            summarise(orders = sum(ord)) %>%
            arrange(desc(orders)) %>% 
            top_n(5, orders)
        
        utm_source_conversions$utm_source <- factor(
            utm_source_conversions$utm_source,
            levels = top5_m$utm_source
        )
        
        df <- utm_source_conversions %>%
            filter(utm_source %in% top5_m$utm_source)
        
        if(nrow(df) > 0){
            ggplot(df, 
                   aes(x = step, y = order_conversion)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = percent(order_conversion)), vjust = -1) +
                geom_text(aes(label = prettyNum(orders, big.mark = ",")), 
                          vjust = 1.6, color = "white") +
                scale_y_continuous(labels = percent) +
                facet_grid(utm_source ~.) +
                theme_bw(base_size = 16) +
                theme(axis.title = element_blank())
        }
    })
    
    # ---- UTM Medium Conversions ----
    output$med_conv <- renderPlot({
        utm_med_orders <- filtered_touches() %>%
            filter(!is.na(utm_medium)) %>%
            group_by(utm_medium, step) %>%
            summarise(ord = n_distinct(order_id))
        
        utm_med_conversions <- utm_med_orders %>%
            arrange(utm_medium, desc(step)) %>%
            mutate(orders = cumsum(ord)) %>%
            arrange(utm_medium, step) %>%
            mutate(order_conversion = orders / max(orders)) %>%
            replace(is.na(.), 1)
        
        top5_m <- utm_med_orders %>% 
            summarise(orders = sum(ord)) %>%
            arrange(desc(orders)) %>% 
            top_n(5, orders)
        
        utm_med_conversions$utm_medium <- factor(
            utm_med_conversions$utm_medium,
            levels = top5_m$utm_medium
        )
        
        df <- utm_med_conversions %>%
            filter(utm_medium %in% top5_m$utm_medium)
        if(nrow(df) > 0){
            ggplot(df, aes(x = step, y = order_conversion)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = percent(order_conversion)), vjust = -1) +
                geom_text(aes(label = prettyNum(orders, big.mark = ",")), 
                          vjust = 1.6, color = "white") +
                scale_y_continuous(labels = percent) +
                facet_grid(utm_medium ~.) +
                theme_bw(base_size = 16) +
                theme(axis.title = element_blank())
        }
        
    })
    
    # ---- UTM Campaign Conversions ----
    utm_camp_conversions <- reactive({
        utm_camp_orders <- filtered_touches() %>%
            filter(!is.na(utm_campaign)) %>%
            group_by(utm_campaign, step) %>%
            summarise(ord = n_distinct(order_id))
        
        utm_camp_conversions <- utm_camp_orders %>%
            arrange(utm_campaign, desc(step)) %>%
            mutate(orders = cumsum(ord)) %>%
            arrange(utm_campaign, step) %>%
            mutate(order_conversion = orders / max(orders)) %>%
            replace(is.na(.), 1)
        
        top5_m <- utm_camp_orders %>% 
            summarise(orders = sum(ord)) %>%
            arrange(desc(orders)) %>% 
            top_n(5, orders)
        
        utm_camp_conversions$utm_campaign <- factor(
            utm_camp_conversions$utm_campaign,
            levels = top5_m$utm_campaign
        )
        
        utm_camp_conversions %>%
            filter(utm_campaign %in% top5_m$utm_campaign)
    })
    
    output$camp_conv <- renderPlot({
        df <- utm_camp_conversions()
        if(nrow(df) > 0){
            ggplot(df, aes(x = step, y = order_conversion)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = percent(order_conversion)), vjust = -1) +
                geom_text(aes(label = prettyNum(orders, big.mark = ",")), 
                          vjust = 1.6, color = "white") +
                scale_y_continuous(labels = percent) +
                facet_grid(utm_campaign ~.) +
                theme_bw(base_size = 16) +
                theme(axis.title = element_blank())
        }
    })
    
    output$camp_conv_down <- downloadHandler(
        filename = function() {paste("UTM Campaign Conversions ", today(), ".csv", sep='')},
        content = function(file) { write_csv(utm_camp_conversions() %>%
                                                 select(-ord), 
                                             file, na = "" )}
    )
    
    # ---- Monthly Cohort Distrobution ----
    cohort_dist_data <- reactive({
        filtered_touches() %>%
            filter(cohort %in% c("Prom","Bridal","Contemporary")
                   & state == "complete") %>% 
            group_by(order_year_month, cohort) %>% 
            summarise(Orders = n_distinct(order_id)) 
    })
    
    output$monthly_cohorts <- renderPlot({
        df <- cohort_dist_data()
        if(nrow(df) > 0){
            ggplot(df, aes(x = order_year_month, y = Orders, fill = cohort)) + 
                geom_bar(stat = "identity", position = "fill", color = "black", size = 0.2) + 
                theme_bw(base_size = 14) +
                theme(axis.title.x = element_blank(),
                      legend.title = element_blank(),
                      legend.position = "bottom") +
                scale_fill_brewer(palette = "Set3") + 
                scale_y_continuous(labels = percent)
        }
    })
    
    output$monthly_cohorts_down <- downloadHandler(
        filename = function() {paste("Monthly Cohort Distribution ", today(), ".csv")},
        content = function(file) {
            write_csv(cohort_dist_data() %>%
                          group_by(order_year_month) %>%
                          mutate(`% of Orders` = Orders / sum(Orders)) %>%
                          rename(Cohort = cohort,
                                 `Order Month` = order_year_month),
                      file, na = "")
        }
    )
    
    # ---- Days from First Touch to Purchase ----
    output$sales_cycle <- renderPlot({
        orders_by_sales_cycle <- filtered_touches() %>% 
            filter(!is.na(ordered_at) & state == "complete") %>% 
            group_by(order_id) %>% 
            summarise(sales_cycle_days = as.numeric(difftime(max(ordered_at), min(touch_time), units = "secs")) / 86400) %>% 
            mutate(sales_cycle = ifelse(sales_cycle_days < 1, "Less than a Day", 
                                        ifelse(sales_cycle_days <= 7, "1 - 7 Days", 
                                               ifelse(sales_cycle_days <= 14, "7 - 14 Days", 
                                                      ifelse(sales_cycle_days <= 30, "15 - 30 Days", 
                                                             ifelse(sales_cycle_days <= 60, "30 - 60 Days", "Over 60 Days")))))) %>% 
            group_by(sales_cycle) %>% summarise(Orders = n())
        
        orders_by_sales_cycle$sales_cycle <- factor(
            orders_by_sales_cycle$sales_cycle,
            levels = c("Less than a Day","1 - 7 Days","7 - 14 Days","15 - 30 Days","30 - 60 Days","Over 60 Days") %>% rev()
        )
        
        ggplot(orders_by_sales_cycle, aes(y = Orders, x = sales_cycle)) +
            geom_bar(stat = "identity") +
            theme_bw(base_size = 14) +
            theme(axis.title.y = element_blank()) +
            coord_flip()
        
    })
    
# ---- FB & GA Tab ----
    # ---- Filter Functions ----
    conv_platform_filter <- reactive({
        if(length(input$conv_platform) > 0){
            input$conv_platform
        } else { unique(ga_fb$Platform) }
    })
    
    conv_cohort_filter <- reactive({
        if(length(input$conv_cohort) > 0){
            input$conv_cohort
        } else { unique(ga_fb$cohort) }
    })
    
    target_filter <- reactive({
        if(length(input$conv_target) > 0){
            input$conv_target
        } else { unique(ga_fb$target) }
    })
    
    country_filter <- reactive({
        if(length(input$conv_country) > 0){
            input$conv_country
        } else { unique(ga_fb$country) }
    })
    
    region_filter <- reactive({
        if(length(input$conv_region) > 0){
            input$conv_region
        } else { unique(ga_fb$region) }
    })
    
    age_filter <- reactive({
        if(length(input$conv_age) > 0){
            input$conv_age
        } else { unique(ga_fb$age)}
    })
    
    device_type_filter <- reactive({
        if(length(input$conv_device_type) > 0){
            input$conv_device_type
        } else { unique(ga_fb$device_type)}
    })
    
    creative_type_filter <- reactive({
        if(length(input$conv_creative_type) > 0){
            input$conv_creative_type
        } else { unique(ga_fb$creative_type)}
    })
    
    creative_strategy_filter <- reactive({
        if(length(input$conv_creative_strategy) > 0){
            input$conv_creative_strategy
        } else {unique(ga_fb$creative_strategy)}
    })
    
    theme_filter <- reactive({
        if(length(input$conv_theme) > 0){
            input$conv_theme
        } else { unique(ga_fb$theme)}
    })
    
    ad_format_filter <- reactive({
        if(length(input$conv_ad_format)){
            input$conv_ad_format
        } else {unique(ga_fb$ad_format)}
    })
    
    pic_source_filter <- reactive({
        if(length(input$conv_pic_source)){
            input$conv_pic_source
        } else {unique(ga_fb$pic_source)}
    })
    
    copy_type_filter <- reactive({
        if(length(input$conv_copy_type)) {
            input$conv_copy_type
        } else {unique(ga_fb$copy_type)}
    })
    
    landing_page_filter <- reactive({
        if(length(input$conv_landing_page)){
            input$conv_landing_page
        } else {unique(ga_fb$landing_page)}
    })
    
    product_category_filter <- reactive({
        if(length(input$conv_product_category)){
            input$conv_product_category
        } else {unique(ga_fb$product_category)}
    })
    
    filtered_ga_fb <- reactive({
        ga_fb %>%
            filter(ifelse(input$conv_prospecting == "Prospecting", T, F) == prospecting) %>%
            filter(between(Date, input$conv_dates[1], input$conv_dates[2])) %>%
            filter(cohort %in% conv_cohort_filter()) %>%
            filter(target %in% target_filter()) %>%
            filter(country %in% country_filter()) %>%
            filter(region %in% region_filter()) %>%
            filter(age %in% age_filter()) %>%
            filter(device_type %in% device_type_filter()) %>%
            filter(creative_type %in% creative_type_filter()) %>%
            filter(creative_strategy %in% creative_strategy_filter()) %>%
            filter(theme %in% theme_filter()) %>%
            filter(ad_format %in% ad_format_filter()) %>%
            filter(pic_source %in% pic_source_filter()) %>%
            filter(copy_type %in% copy_type_filter()) %>%
            filter(landing_page %in% landing_page_filter()) %>%
            filter(product_category %in% product_category_filter()) %>%
            filter(Platform %in% conv_platform_filter())
    })
    
    # ---- Summary Tables ----
    conv_kpi_summarise <- function(conv_df){
        conv_df %>%
            summarise(Reach = sum(Reach),
                      Impressions = sum(Impressions),
                      `Spend (USD)` = sum(coalesce(Amount_Spent_USD, 0)),
                      Unique_Clicks = sum(coalesce(Unique_Clicks, 0)),
                      Unique_Link_Clicks = sum(coalesce(Unique_Link_Clicks, 0)),
                      Adds_to_Cart = sum(coalesce(Adds_to_Cart, 0)),
                      Conversions = sum(coalesce(Conversions, 0)),
                      Purchases  = sum(coalesce(Purchases, Transactions, 0)),
                      Post_Shares = sum(coalesce(Post_Shares, 0)),
                      Post_Comments = sum(coalesce(Post_Comments, 0)),
                      Post_Reactions = sum(coalesce(Post_Reactions, 0)),
                      Bounces = sum(coalesce(Bounces, 0)),
                      Sessions = sum(coalesce(Sessions, 0)),
                      Session_Duration = sum(coalesce(Session_Duration_min, 0))) %>%
            mutate(CTR = coalesce(Unique_Clicks / Reach, 0),
                   CPC = coalesce(`Spend (USD)` / Unique_Clicks, 0),
                   CAC = `Spend (USD)` / Purchases,
                   CPAC = coalesce(`Spend (USD)` / Adds_to_Cart),
                   `Avg. Session Duration` = coalesce(Session_Duration / Sessions, 0),
                   `Bounce Rate` = coalesce((Bounces / Sessions), 0)) %>%
            replace(. == Inf, 0)
    }
    
    output$conv_kpis <- renderTable(
        filtered_ga_fb() %>% 
            conv_kpi_summarise() %>% 
            transmute(`Spend (USD)` = dollar(`Spend (USD)`), 
                      Purchases = Purchases %>% round() %>% as.integer(), 
                      CAC = dollar(CAC), 
                      CTR = percent(CTR), 
                      CPC = dollar(CPC), 
                      CPAC = dollar(CPAC),
                      `T.O.S.` = paste(round(`Avg. Session Duration`, 2), "min"),
                      Sessions = short_number(Sessions),
                      `Total Carts` = short_number(Adds_to_Cart),
                      `Bounce Rate` = percent(`Bounce Rate`))
    )
    
    creative_summary_data <- reactive({
        filtered_ga_fb() %>% 
            group_by(Creative = creative) %>% 
            conv_kpi_summarise() %>%
            arrange(desc(`Spend (USD)`)) %>%
            transmute(Creative, `Spend (USD)`, Purchases, 
                      CAC = ifelse(CAC == 0, NA, CAC), 
                      CTR, 
                      CPC = ifelse(CPC == 0, NA, CPC), 
                      CPAC = ifelse(CPAC == 0, NA, CPAC), 
                      `T.O.S.` = `Avg. Session Duration`, 
                      Sessions,
                      `Total Carts` = Adds_to_Cart,
                      `Bounce Rate`)
    })
    
    output$conv_creative_summary <- renderDataTable({
        perf_thresh <- if(input$conv_prospecting == "Prospecting"){
            c(100, 300)
        } else { c(25, 50) }
        
        creative_summary_data() %>% 
            datatable(class = "hover row-border", style = "bootstrap", escape = FALSE,
                      options = list(lengthMenu = c(10, 25, 50), pageLength = 10)) %>%
            formatPercentage(c("CTR","Bounce Rate"), digits = 1) %>%
            formatCurrency(c("Spend (USD)", "CAC", "CPC", "CPAC")) %>%
            formatCurrency(c("T.O.S."), before = FALSE, currency = " min") %>%
            formatCurrency(c("Sessions","Total Carts"), currency = "", digits = 0) %>%
            formatStyle(
                "CAC", background = styleInterval(
                    perf_thresh, c("#f97068","#f9fc5f","#77ff68") %>% rev()
                )
            ) %>%
            formatStyle(
                "CPAC", background = styleInterval(
                    perf_thresh / 3, c("#f97068","#f9fc5f","#77ff68") %>% rev()
                )
            )
    })
    
    output$conv_creative_summary_down <- downloadHandler(
            filename = function() {paste0("FB & IG Creative ", today(), ".csv")},
            content = function(file) {write_csv(creative_summary_data(), file, na = "")}
        )
    
    # ---- Comparisons ----
    output$conv_cohort_comp <- renderPlot({
        cohort_summary <- filtered_ga_fb() %>%
            group_by(cohort) %>%
            conv_kpi_summarise()
        if(input$conv_cohort_metric == "Spend (USD)"){
            df <- cohort_summary %>%
                filter(`Spend (USD)` > 0) %>%
                arrange(desc(`Spend (USD)`))
            df$cohort <- factor(df$cohort, levels = df$cohort)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = cohort, fill = cohort, y = `Spend (USD)`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_cohort_metric == "Purchases") {
            df <- cohort_summary %>%
                filter(Purchases > 0) %>%
                arrange(desc(Purchases))
            df$cohort <- factor(df$cohort, levels = df$cohort)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = cohort, fill = cohort, y = Purchases)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_cohort_metric == "CAC"){
            df <- cohort_summary %>%
                filter(CAC > 0) %>%
                arrange(desc(CAC))
            df$cohort <- factor(df$cohort, levels = df$cohort)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = cohort, fill = cohort, y = CAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_cohort_metric == "CTR"){
            df <- cohort_summary %>%
                filter(CTR > 0) %>%
                arrange(desc(CTR))
            df$cohort <- factor(df$cohort, levels = df$cohort)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = cohort, fill = cohort, y = CTR)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        } else if(input$conv_cohort_metric == "CPAC"){
            df <- cohort_summary %>%
                filter(CPAC > 0) %>%
                arrange(desc(CPAC))
            df$cohort <- factor(df$cohort, levels = df$cohort)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = cohort, fill = cohort, y = CPAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = dollar)
            }
        } else if(input$conv_cohort_metric == "T.O.S."){
            df <- cohort_summary %>%
                rename(`T.O.S.` = `Avg. Session Duration`) %>%
                filter(`T.O.S.` > 0) %>%
                arrange(desc(`T.O.S.`))
            df$cohort <- factor(df$cohort, levels = df$cohort)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = cohort, fill = cohort, y = `T.O.S.`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = function(x){paste(x, "min")})
            }
        } else if(input$conv_cohort_metric == "Sessions"){
            df <- cohort_summary %>%
                filter(Sessions > 0) %>%
                arrange(desc(Sessions))
            df$cohort <- factor(df$cohort, levels = df$cohort)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = cohort, fill = cohort, y = Sessions)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_cohort_metric == "Total Carts"){
            df <- cohort_summary %>%
                rename(`Total Carts` = Adds_to_Cart) %>%
                filter(`Total Carts` > 0) %>%
                arrange(desc(`Total Carts`))
            df$cohort <- factor(df$cohort, levels = df$cohort)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = cohort, fill = cohort, y = `Total Carts`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_cohort_metric == "Bounce Rate"){
            df <- cohort_summary %>%
                filter(`Bounce Rate` > 0) %>%
                arrange(desc(`Bounce Rate`))
            df$cohort <- factor(df$cohort, levels = df$cohort)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = cohort, fill = cohort, y = `Bounce Rate`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        }
    })
    
    output$conv_target_comp <- renderPlot({
        target_summary <- filtered_ga_fb() %>%
            group_by(target) %>%
            conv_kpi_summarise()
        if(input$conv_target_metric == "Spend (USD)"){
            df <- target_summary %>%
                filter(`Spend (USD)` > 0) %>%
                arrange(desc(`Spend (USD)`))
            df$target <- factor(df$target, levels = df$target)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = target, fill = target, y = `Spend (USD)`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_target_metric == "Purchases") {
            df <- target_summary %>%
                filter(Purchases > 0) %>%
                arrange(desc(Purchases))
            df$target <- factor(df$target, levels = df$target)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = target, fill = target, y = Purchases)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_target_metric == "CAC"){
            df <- target_summary %>%
                filter(CAC > 0) %>%
                arrange(desc(CAC))
            df$target <- factor(df$target, levels = df$target)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = target, fill = target, y = CAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_target_metric == "CTR"){
            df <- target_summary %>%
                filter(CTR > 0) %>%
                arrange(desc(CTR))
            df$target <- factor(df$target, levels = df$target)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = target, fill = target, y = CTR)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        } else if(input$conv_target_metric == "CPAC"){
            df <- target_summary %>%
                filter(CPAC > 0) %>%
                arrange(desc(CPAC))
            df$target <- factor(df$target, levels = df$target)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = target, fill = target, y = CPAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = dollar)
            }
        } else if(input$conv_target_metric == "T.O.S."){
            df <- target_summary %>%
                rename(`T.O.S.` = `Avg. Session Duration`) %>%
                filter(`T.O.S.` > 0) %>%
                arrange(desc(`T.O.S.`))
            df$target <- factor(df$target, levels = df$target)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = target, fill = target, y = `T.O.S.`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = function(x){paste(x, "min")})
            }
        } else if(input$conv_target_metric == "Sessions"){
            df <- target_summary %>%
                filter(Sessions > 0) %>%
                arrange(desc(Sessions))
            df$target <- factor(df$target, levels = df$target)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = target, fill = target, y = Sessions)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_target_metric == "Total Carts"){
            df <- target_summary %>%
                rename(`Total Carts` = Adds_to_Cart) %>%
                filter(`Total Carts` > 0) %>%
                arrange(desc(`Total Carts`))
            df$target <- factor(df$target, levels = df$target)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = target, fill = target, y = `Total Carts`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_target_metric == "Bounce Rate"){
            df <- target_summary %>%
                filter(`Bounce Rate` > 0) %>%
                arrange(desc(`Bounce Rate`))
            df$target <- factor(df$target, levels = df$target)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = target, fill = target, y = `Bounce Rate`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        }
    })
    
    output$conv_country_comp <- renderPlot({
        country_summary <- filtered_ga_fb() %>%
            group_by(country) %>%
            conv_kpi_summarise()
        if(input$conv_country_metric == "Spend (USD)"){
            df <- country_summary %>%
                filter(`Spend (USD)` > 0) %>%
                arrange(desc(`Spend (USD)`))
            df$country <- factor(df$country, levels = df$country)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = country, fill = country, y = `Spend (USD)`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_country_metric == "Purchases") {
            df <- country_summary %>%
                filter(Purchases > 0) %>%
                arrange(desc(Purchases))
            df$country <- factor(df$country, levels = df$country)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = country, fill = country, y = Purchases)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_country_metric == "CAC"){
            df <- country_summary %>%
                filter(CAC > 0) %>%
                arrange(desc(CAC))
            df$country <- factor(df$country, levels = df$country)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = country, fill = country, y = CAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_country_metric == "CTR"){
            df <- country_summary %>%
                filter(CTR > 0) %>%
                arrange(desc(CTR))
            df$country <- factor(df$country, levels = df$country)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = country, fill = country, y = CTR)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        } else if(input$conv_country_metric == "CPAC"){
            df <- country_summary %>%
                filter(CPAC > 0) %>%
                arrange(desc(CPAC))
            df$country <- factor(df$country, levels = df$country)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = country, fill = country, y = CPAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = dollar)
            }
        } else if(input$conv_country_metric == "T.O.S."){
            df <- country_summary %>%
                rename(`T.O.S.` = `Avg. Session Duration`) %>%
                filter(`T.O.S.` > 0) %>%
                arrange(desc(`T.O.S.`))
            df$country <- factor(df$country, levels = df$country)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = country, fill = country, y = `T.O.S.`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = function(x){paste(x, "min")})
            }
        } else if(input$conv_country_metric == "Sessions"){
            df <- country_summary %>%
                filter(Sessions > 0) %>%
                arrange(desc(Sessions))
            df$country <- factor(df$country, levels = df$country)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = country, fill = country, y = Sessions)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_country_metric == "Total Carts"){
            df <- country_summary %>%
                rename(`Total Carts` = Adds_to_Cart) %>%
                filter(`Total Carts` > 0) %>%
                arrange(desc(`Total Carts`))
            df$country <- factor(df$country, levels = df$country)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = country, fill = country, y = `Total Carts`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_country_metric == "Bounce Rate"){
            df <- country_summary %>%
                filter(`Bounce Rate` > 0) %>%
                arrange(desc(`Bounce Rate`))
            df$country <- factor(df$country, levels = df$country)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = country, fill = country, y = `Bounce Rate`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        }
    })
    
    output$conv_age_comp <- renderPlot({
        age_summary <- filtered_ga_fb() %>%
            group_by(age) %>%
            conv_kpi_summarise()
        if(input$conv_age_metric == "Spend (USD)"){
            df <- age_summary %>%
                filter(`Spend (USD)` > 0) %>%
                arrange(desc(`Spend (USD)`))
            df$age <- factor(df$age, levels = df$age)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = age, fill = age, y = `Spend (USD)`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_age_metric == "Purchases") {
            df <- age_summary %>%
                filter(Purchases > 0) %>%
                arrange(desc(Purchases))
            df$age <- factor(df$age, levels = df$age)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = age, fill = age, y = Purchases)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_age_metric == "CAC"){
            df <- age_summary %>%
                filter(CAC > 0) %>%
                arrange(desc(CAC))
            df$age <- factor(df$age, levels = df$age)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = age, fill = age, y = CAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_age_metric == "CTR"){
            df <- age_summary %>%
                filter(CTR > 0) %>%
                arrange(desc(CTR))
            df$age <- factor(df$age, levels = df$age)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = age, fill = age, y = CTR)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        } else if(input$conv_age_metric == "CPAC"){
            df <- age_summary %>%
                filter(CPAC > 0) %>%
                arrange(desc(CPAC))
            df$age <- factor(df$age, levels = df$age)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = age, fill = age, y = CPAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = dollar)
            }
        } else if(input$conv_age_metric == "T.O.S."){
            df <- age_summary %>%
                rename(`T.O.S.` = `Avg. Session Duration`) %>%
                filter(`T.O.S.` > 0) %>%
                arrange(desc(`T.O.S.`))
            df$age <- factor(df$age, levels = df$age)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = age, fill = age, y = `T.O.S.`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = function(x){paste(x, "min")})
            }
        } else if(input$conv_age_metric == "Sessions"){
            df <- age_summary %>%
                filter(Sessions > 0) %>%
                arrange(desc(Sessions))
            df$age <- factor(df$age, levels = df$age)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = age, fill = age, y = Sessions)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_age_metric == "Total Carts"){
            df <- age_summary %>%
                rename(`Total Carts` = Adds_to_Cart) %>%
                filter(`Total Carts` > 0) %>%
                arrange(desc(`Total Carts`))
            df$age <- factor(df$age, levels = df$age)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = age, fill = age, y = `Total Carts`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_age_metric == "Bounce Rate"){
            df <- age_summary %>%
                filter(`Bounce Rate` > 0) %>%
                arrange(desc(`Bounce Rate`))
            df$age <- factor(df$age, levels = df$age)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = age, fill = age, y = `Bounce Rate`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        }
    })
    
    output$conv_device_type_comp <- renderPlot({
        device_type_summary <- filtered_ga_fb() %>%
            group_by(device_type) %>%
            conv_kpi_summarise()
        if(input$conv_device_type_metric == "Spend (USD)"){
            df <- device_type_summary %>%
                filter(`Spend (USD)` > 0) %>%
                arrange(desc(`Spend (USD)`))
            df$device_type <- factor(df$device_type, levels = df$device_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = device_type, fill = device_type, y = `Spend (USD)`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_device_type_metric == "Purchases") {
            df <- device_type_summary %>%
                filter(Purchases > 0) %>%
                arrange(desc(Purchases))
            df$device_type <- factor(df$device_type, levels = df$device_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = device_type, fill = device_type, y = Purchases)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_device_type_metric == "CAC"){
            df <- device_type_summary %>%
                filter(CAC > 0) %>%
                arrange(desc(CAC))
            df$device_type <- factor(df$device_type, levels = df$device_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = device_type, fill = device_type, y = CAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_device_type_metric == "CTR"){
            df <- device_type_summary %>%
                filter(CTR > 0) %>%
                arrange(desc(CTR))
            df$device_type <- factor(df$device_type, levels = df$device_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = device_type, fill = device_type, y = CTR)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        } else if(input$conv_device_type_metric == "CPAC"){
            df <- device_type_summary %>%
                filter(CPAC > 0) %>%
                arrange(desc(CPAC))
            df$device_type <- factor(df$device_type, levels = df$device_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = device_type, fill = device_type, y = CPAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = dollar)
            }
        } else if(input$conv_device_type_metric == "T.O.S."){
            df <- device_type_summary %>%
                rename(`T.O.S.` = `Avg. Session Duration`) %>%
                filter(`T.O.S.` > 0) %>%
                arrange(desc(`T.O.S.`))
            df$device_type <- factor(df$device_type, levels = df$device_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = device_type, fill = device_type, y = `T.O.S.`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = function(x){paste(x, "min")})
            }
        } else if(input$conv_device_type_metric == "Sessions"){
            df <- device_type_summary %>%
                filter(Sessions > 0) %>%
                arrange(desc(Sessions))
            df$device_type <- factor(df$device_type, levels = df$device_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = device_type, fill = device_type, y = Sessions)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_device_type_metric == "Total Carts"){
            df <- device_type_summary %>%
                rename(`Total Carts` = Adds_to_Cart) %>%
                filter(`Total Carts` > 0) %>%
                arrange(desc(`Total Carts`))
            df$device_type <- factor(df$device_type, levels = df$device_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = device_type, fill = device_type, y = `Total Carts`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_device_type_metric == "Bounce Rate"){
            df <- device_type_summary %>%
                filter(`Bounce Rate` > 0) %>%
                arrange(desc(`Bounce Rate`))
            df$device_type <- factor(df$device_type, levels = df$device_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = device_type, fill = device_type, y = `Bounce Rate`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        }
    })
    
    output$conv_creative_type_comp <- renderPlot({
        creative_type_summary <- filtered_ga_fb() %>%
            group_by(creative_type) %>%
            conv_kpi_summarise()
        if(input$conv_creative_type_metric == "Spend (USD)"){
            df <- creative_type_summary %>%
                filter(`Spend (USD)` > 0) %>%
                arrange(desc(`Spend (USD)`))
        df$creative_type <- factor(df$creative_type, levels = df$creative_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_type, fill = creative_type, y = `Spend (USD)`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_creative_type_metric == "Purchases") {
            df <- creative_type_summary %>%
                filter(Purchases > 0) %>%
                arrange(desc(Purchases))
            df$creative_type <- factor(df$creative_type, levels = df$creative_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_type, fill = creative_type, y = Purchases)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_creative_type_metric == "CAC"){
            df <- creative_type_summary %>%
                filter(CAC > 0) %>%
                arrange(desc(CAC))
            df$creative_type <- factor(df$creative_type, levels = df$creative_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_type, fill = creative_type, y = CAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_creative_type_metric == "CTR"){
            df <- creative_type_summary %>%
                filter(CTR > 0) %>%
                arrange(desc(CTR))
            df$creative_type <- factor(df$creative_type, levels = df$creative_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_type, fill = creative_type, y = CTR)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        } else if(input$conv_creative_type_metric == "CPAC"){
            df <- creative_type_summary %>%
                filter(CPAC > 0) %>%
                arrange(desc(CPAC))
            df$creative_type <- factor(df$creative_type, levels = df$creative_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_type, fill = creative_type, y = CPAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = dollar)
            }
        } else if(input$conv_creative_type_metric == "T.O.S."){
            df <- creative_type_summary %>%
                rename(`T.O.S.` = `Avg. Session Duration`) %>%
                filter(`T.O.S.` > 0) %>%
                arrange(desc(`T.O.S.`))
            df$creative_type <- factor(df$creative_type, levels = df$creative_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_type, fill = creative_type, y = `T.O.S.`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = function(x){paste(x, "min")})
            }
        } else if(input$conv_creative_type_metric == "Sessions"){
            df <- creative_type_summary %>%
                filter(Sessions > 0) %>%
                arrange(desc(Sessions))
            df$creative_type <- factor(df$creative_type, levels = df$creative_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_type, fill = creative_type, y = Sessions)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_creative_type_metric == "Total Carts"){
            df <- creative_type_summary %>%
                rename(`Total Carts` = Adds_to_Cart) %>%
                filter(`Total Carts` > 0) %>%
                arrange(desc(`Total Carts`))
            df$creative_type <- factor(df$creative_type, levels = df$creative_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_type, fill = creative_type, y = `Total Carts`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_creative_type_metric == "Bounce Rate"){
            df <- creative_type_summary %>%
                filter(`Bounce Rate` > 0) %>%
                arrange(desc(`Bounce Rate`))
            df$creative_type <- factor(df$creative_type, levels = df$creative_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_type, fill = creative_type, y = `Bounce Rate`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        }
    })
    
    output$conv_creative_strategy_comp <- renderPlot({
        creative_strategy_summary <- filtered_ga_fb() %>%
            group_by(creative_strategy) %>%
            conv_kpi_summarise()
        if(input$conv_creative_strategy_metric == "Spend (USD)"){
            df <- creative_strategy_summary %>%
                filter(`Spend (USD)` > 0) %>%
                arrange(desc(`Spend (USD)`))
            df$creative_strategy <- factor(df$creative_strategy, levels = df$creative_strategy)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_strategy, fill = creative_strategy, y = `Spend (USD)`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_creative_strategy_metric == "Purchases") {
            df <- creative_strategy_summary %>%
                filter(Purchases > 0) %>%
                arrange(desc(Purchases))
            df$creative_strategy <- factor(df$creative_strategy, levels = df$creative_strategy)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_strategy, fill = creative_strategy, y = Purchases)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_creative_strategy_metric == "CAC"){
            df <- creative_strategy_summary %>%
                filter(CAC > 0) %>%
                arrange(desc(CAC))
            df$creative_strategy <- factor(df$creative_strategy, levels = df$creative_strategy)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_strategy, fill = creative_strategy, y = CAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_creative_strategy_metric == "CTR"){
            df <- creative_strategy_summary %>%
                filter(CTR > 0) %>%
                arrange(desc(CTR))
            df$creative_strategy <- factor(df$creative_strategy, levels = df$creative_strategy)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_strategy, fill = creative_strategy, y = CTR)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        } else if(input$conv_creative_strategy_metric == "CPAC"){
            df <- creative_strategy_summary %>%
                filter(CPAC > 0) %>%
                arrange(desc(CPAC))
            df$creative_strategy <- factor(df$creative_strategy, levels = df$creative_strategy)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_strategy, fill = creative_strategy, y = CPAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = dollar)
            }
        } else if(input$conv_creative_strategy_metric == "T.O.S."){
            df <- creative_strategy_summary %>%
                rename(`T.O.S.` = `Avg. Session Duration`) %>%
                filter(`T.O.S.` > 0) %>%
                arrange(desc(`T.O.S.`))
            df$creative_strategy <- factor(df$creative_strategy, levels = df$creative_strategy)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_strategy, fill = creative_strategy, y = `T.O.S.`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = function(x){paste(x, "min")})
            }
        } else if(input$conv_creative_strategy_metric == "Sessions"){
            df <- creative_strategy_summary %>%
                filter(Sessions > 0) %>%
                arrange(desc(Sessions))
            df$creative_strategy <- factor(df$creative_strategy, levels = df$creative_strategy)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_strategy, fill = creative_strategy, y = Sessions)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_creative_strategy_metric == "Total Carts"){
            df <- creative_strategy_summary %>%
                rename(`Total Carts` = Adds_to_Cart) %>%
                filter(`Total Carts` > 0) %>%
                arrange(desc(`Total Carts`))
            df$creative_strategy <- factor(df$creative_strategy, levels = df$creative_strategy)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_strategy, fill = creative_strategy, y = `Total Carts`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_creative_strategy_metric == "Bounce Rate"){
            df <- creative_strategy_summary %>%
                filter(`Bounce Rate` > 0) %>%
                arrange(desc(`Bounce Rate`))
            df$creative_strategy <- factor(df$creative_strategy, levels = df$creative_strategy)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = creative_strategy, fill = creative_strategy, y = `Bounce Rate`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        }
    })
    
    output$conv_theme_comp <- renderPlot({
        theme_summary <- filtered_ga_fb() %>%
            group_by(theme) %>%
            conv_kpi_summarise()
        if(input$conv_theme_metric == "Spend (USD)"){
            df <- theme_summary %>%
                filter(`Spend (USD)` > 0) %>%
                arrange(desc(`Spend (USD)`))
            df$theme <- factor(df$theme, levels = df$theme)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = theme, fill = theme, y = `Spend (USD)`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_theme_metric == "Purchases") {
            df <- theme_summary %>%
                filter(Purchases > 0) %>%
                arrange(desc(Purchases))
            df$theme <- factor(df$theme, levels = df$theme)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = theme, fill = theme, y = Purchases)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_theme_metric == "CAC"){
            df <- theme_summary %>%
                filter(CAC > 0) %>%
                arrange(desc(CAC))
            df$theme <- factor(df$theme, levels = df$theme)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = theme, fill = theme, y = CAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_theme_metric == "CTR"){
            df <- theme_summary %>%
                filter(CTR > 0) %>%
                arrange(desc(CTR))
            df$theme <- factor(df$theme, levels = df$theme)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = theme, fill = theme, y = CTR)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        } else if(input$conv_theme_metric == "CPAC"){
            df <- theme_summary %>%
                filter(CPAC > 0) %>%
                arrange(desc(CPAC))
            df$theme <- factor(df$theme, levels = df$theme)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = theme, fill = theme, y = CPAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = dollar)
            }
        } else if(input$conv_theme_metric == "T.O.S."){
            df <- theme_summary %>%
                rename(`T.O.S.` = `Avg. Session Duration`) %>%
                filter(`T.O.S.` > 0) %>%
                arrange(desc(`T.O.S.`))
            df$theme <- factor(df$theme, levels = df$theme)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = theme, fill = theme, y = `T.O.S.`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = function(x){paste(x, "min")})
            }
        } else if(input$conv_theme_metric == "Sessions"){
            df <- theme_summary %>%
                filter(Sessions > 0) %>%
                arrange(desc(Sessions))
            df$theme <- factor(df$theme, levels = df$theme)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = theme, fill = theme, y = Sessions)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_theme_metric == "Total Carts"){
            df <- theme_summary %>%
                rename(`Total Carts` = Adds_to_Cart) %>%
                filter(`Total Carts` > 0) %>%
                arrange(desc(`Total Carts`))
            df$theme <- factor(df$theme, levels = df$theme)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = theme, fill = theme, y = `Total Carts`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_theme_metric == "Bounce Rate"){
            df <- theme_summary %>%
                filter(`Bounce Rate` > 0) %>%
                arrange(desc(`Bounce Rate`))
            df$theme <- factor(df$theme, levels = df$theme)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = theme, fill = theme, y = `Bounce Rate`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        }
    })
    
    output$conv_ad_format_comp <- renderPlot({
        ad_format_summary <- filtered_ga_fb() %>%
            group_by(ad_format) %>%
            conv_kpi_summarise()
        if(input$conv_ad_format_metric == "Spend (USD)"){
            df <- ad_format_summary %>%
                filter(`Spend (USD)` > 0) %>%
                arrange(desc(`Spend (USD)`))
            df$ad_format <- factor(df$ad_format, levels = df$ad_format)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = ad_format, fill = ad_format, y = `Spend (USD)`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_ad_format_metric == "Purchases") {
            df <- ad_format_summary %>%
                filter(Purchases > 0) %>%
                arrange(desc(Purchases))
            df$ad_format <- factor(df$ad_format, levels = df$ad_format)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = ad_format, fill = ad_format, y = Purchases)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_ad_format_metric == "CAC"){
            df <- ad_format_summary %>%
                filter(CAC > 0) %>%
                arrange(desc(CAC))
            df$ad_format <- factor(df$ad_format, levels = df$ad_format)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = ad_format, fill = ad_format, y = CAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_ad_format_metric == "CTR"){
            df <- ad_format_summary %>%
                filter(CTR > 0) %>%
                arrange(desc(CTR))
            df$ad_format <- factor(df$ad_format, levels = df$ad_format)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = ad_format, fill = ad_format, y = CTR)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        } else if(input$conv_ad_format_metric == "CPAC"){
            df <- ad_format_summary %>%
                filter(CPAC > 0) %>%
                arrange(desc(CPAC))
            df$ad_format <- factor(df$ad_format, levels = df$ad_format)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = ad_format, fill = ad_format, y = CPAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = dollar)
            }
        } else if(input$conv_ad_format_metric == "T.O.S."){
            df <- ad_format_summary %>%
                rename(`T.O.S.` = `Avg. Session Duration`) %>%
                filter(`T.O.S.` > 0) %>%
                arrange(desc(`T.O.S.`))
            df$ad_format <- factor(df$ad_format, levels = df$ad_format)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = ad_format, fill = ad_format, y = `T.O.S.`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = function(x){paste(x, "min")})
            }
        } else if(input$conv_ad_format_metric == "Sessions"){
            df <- ad_format_summary %>%
                filter(Sessions > 0) %>%
                arrange(desc(Sessions))
            df$ad_format <- factor(df$ad_format, levels = df$ad_format)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = ad_format, fill = ad_format, y = Sessions)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_ad_format_metric == "Total Carts"){
            df <- ad_format_summary %>%
                rename(`Total Carts` = Adds_to_Cart) %>%
                filter(`Total Carts` > 0) %>%
                arrange(desc(`Total Carts`))
            df$ad_format <- factor(df$ad_format, levels = df$ad_format)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = ad_format, fill = ad_format, y = `Total Carts`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_ad_format_metric == "Bounce Rate"){
            df <- ad_format_summary %>%
                filter(`Bounce Rate` > 0) %>%
                arrange(desc(`Bounce Rate`))
            df$ad_format <- factor(df$ad_format, levels = df$ad_format)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = ad_format, fill = ad_format, y = `Bounce Rate`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        }
    })
    
    output$conv_copy_type_comp <- renderPlot({
        copy_type_summary <- filtered_ga_fb() %>%
            group_by(copy_type) %>%
            conv_kpi_summarise()
        if(input$conv_copy_type_metric == "Spend (USD)"){
            df <- copy_type_summary %>%
                filter(`Spend (USD)` > 0) %>%
                arrange(desc(`Spend (USD)`))
            df$copy_type <- factor(df$copy_type, levels = df$copy_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = copy_type, fill = copy_type, y = `Spend (USD)`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_copy_type_metric == "Purchases") {
            df <- copy_type_summary %>%
                filter(Purchases > 0) %>%
                arrange(desc(Purchases))
            df$copy_type <- factor(df$copy_type, levels = df$copy_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = copy_type, fill = copy_type, y = Purchases)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_copy_type_metric == "CAC"){
            df <- copy_type_summary %>%
                filter(CAC > 0) %>%
                arrange(desc(CAC))
            df$copy_type <- factor(df$copy_type, levels = df$copy_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = copy_type, fill = copy_type, y = CAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_copy_type_metric == "CTR"){
            df <- copy_type_summary %>%
                filter(CTR > 0) %>%
                arrange(desc(CTR))
            df$copy_type <- factor(df$copy_type, levels = df$copy_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = copy_type, fill = copy_type, y = CTR)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        } else if(input$conv_copy_type_metric == "CPAC"){
            df <- copy_type_summary %>%
                filter(CPAC > 0) %>%
                arrange(desc(CPAC))
            df$copy_type <- factor(df$copy_type, levels = df$copy_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = copy_type, fill = copy_type, y = CPAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = dollar)
            }
        } else if(input$conv_copy_type_metric == "T.O.S."){
            df <- copy_type_summary %>%
                rename(`T.O.S.` = `Avg. Session Duration`) %>%
                filter(`T.O.S.` > 0) %>%
                arrange(desc(`T.O.S.`))
            df$copy_type <- factor(df$copy_type, levels = df$copy_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = copy_type, fill = copy_type, y = `T.O.S.`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = function(x){paste(x, "min")})
            }
        } else if(input$conv_copy_type_metric == "Sessions"){
            df <- copy_type_summary %>%
                filter(Sessions > 0) %>%
                arrange(desc(Sessions))
            df$copy_type <- factor(df$copy_type, levels = df$copy_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = copy_type, fill = copy_type, y = Sessions)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_copy_type_metric == "Total Carts"){
            df <- copy_type_summary %>%
                rename(`Total Carts` = Adds_to_Cart) %>%
                filter(`Total Carts` > 0) %>%
                arrange(desc(`Total Carts`))
            df$copy_type <- factor(df$copy_type, levels = df$copy_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = copy_type, fill = copy_type, y = `Total Carts`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_copy_type_metric == "Bounce Rate"){
            df <- copy_type_summary %>%
                filter(`Bounce Rate` > 0) %>%
                arrange(desc(`Bounce Rate`))
            df$copy_type <- factor(df$copy_type, levels = df$copy_type)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = copy_type, fill = copy_type, y = `Bounce Rate`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        }
    })
    
    output$conv_landing_page_comp <- renderPlot({
        landing_page_summary <- filtered_ga_fb() %>%
            group_by(landing_page) %>%
            conv_kpi_summarise()
        if(input$conv_landing_page_metric == "Spend (USD)"){
            df <- landing_page_summary %>%
                filter(`Spend (USD)` > 0) %>%
                arrange(desc(`Spend (USD)`))
            df$landing_page <- factor(df$landing_page, levels = df$landing_page)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = landing_page, fill = landing_page, y = `Spend (USD)`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_landing_page_metric == "Purchases") {
            df <- landing_page_summary %>%
                filter(Purchases > 0) %>%
                arrange(desc(Purchases))
            df$landing_page <- factor(df$landing_page, levels = df$landing_page)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = landing_page, fill = landing_page, y = Purchases)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_landing_page_metric == "CAC"){
            df <- landing_page_summary %>%
                filter(CAC > 0) %>%
                arrange(desc(CAC))
            df$landing_page <- factor(df$landing_page, levels = df$landing_page)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = landing_page, fill = landing_page, y = CAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_landing_page_metric == "CTR"){
            df <- landing_page_summary %>%
                filter(CTR > 0) %>%
                arrange(desc(CTR))
            df$landing_page <- factor(df$landing_page, levels = df$landing_page)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = landing_page, fill = landing_page, y = CTR)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        } else if(input$conv_landing_page_metric == "CPAC"){
            df <- landing_page_summary %>%
                filter(CPAC > 0) %>%
                arrange(desc(CPAC))
            df$landing_page <- factor(df$landing_page, levels = df$landing_page)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = landing_page, fill = landing_page, y = CPAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = dollar)
            }
        } else if(input$conv_landing_page_metric == "T.O.S."){
            df <- landing_page_summary %>%
                rename(`T.O.S.` = `Avg. Session Duration`) %>%
                filter(`T.O.S.` > 0) %>%
                arrange(desc(`T.O.S.`))
            df$landing_page <- factor(df$landing_page, levels = df$landing_page)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = landing_page, fill = landing_page, y = `T.O.S.`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = function(x){paste(x, "min")})
            }
        } else if(input$conv_landing_page_metric == "Sessions"){
            df <- landing_page_summary %>%
                filter(Sessions > 0) %>%
                arrange(desc(Sessions))
            df$landing_page <- factor(df$landing_page, levels = df$landing_page)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = landing_page, fill = landing_page, y = Sessions)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_landing_page_metric == "Total Carts"){
            df <- landing_page_summary %>%
                rename(`Total Carts` = Adds_to_Cart) %>%
                filter(`Total Carts` > 0) %>%
                arrange(desc(`Total Carts`))
            df$landing_page <- factor(df$landing_page, levels = df$landing_page)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = landing_page, fill = landing_page, y = `Total Carts`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_landing_page_metric == "Bounce Rate"){
            df <- landing_page_summary %>%
                filter(`Bounce Rate` > 0) %>%
                arrange(desc(`Bounce Rate`))
            df$landing_page <- factor(df$landing_page, levels = df$landing_page)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = landing_page, fill = landing_page, y = `Bounce Rate`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        }
    })
    
    output$conv_product_category_comp <- renderPlot({
        product_category_summary <- filtered_ga_fb() %>%
            group_by(product_category) %>%
            conv_kpi_summarise()
        if(input$conv_product_category_metric == "Spend (USD)"){
            df <- product_category_summary %>%
                filter(`Spend (USD)` > 0) %>%
                arrange(desc(`Spend (USD)`))
            df$product_category <- factor(df$product_category, levels = df$product_category)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = product_category, fill = product_category, y = `Spend (USD)`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_product_category_metric == "Purchases") {
            df <- product_category_summary %>%
                filter(Purchases > 0) %>%
                arrange(desc(Purchases))
            df$product_category <- factor(df$product_category, levels = df$product_category)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = product_category, fill = product_category, y = Purchases)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_product_category_metric == "CAC"){
            df <- product_category_summary %>%
                filter(CAC > 0) %>%
                arrange(desc(CAC))
            df$product_category <- factor(df$product_category, levels = df$product_category)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = product_category, fill = product_category, y = CAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_dollar)
            }
        } else if(input$conv_product_category_metric == "CTR"){
            df <- product_category_summary %>%
                filter(CTR > 0) %>%
                arrange(desc(CTR))
            df$product_category <- factor(df$product_category, levels = df$product_category)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = product_category, fill = product_category, y = CTR)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        } else if(input$conv_product_category_metric == "CPAC"){
            df <- product_category_summary %>%
                filter(CPAC > 0) %>%
                arrange(desc(CPAC))
            df$product_category <- factor(df$product_category, levels = df$product_category)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = product_category, fill = product_category, y = CPAC)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = dollar)
            }
        } else if(input$conv_product_category_metric == "T.O.S."){
            df <- product_category_summary %>%
                rename(`T.O.S.` = `Avg. Session Duration`) %>%
                filter(`T.O.S.` > 0) %>%
                arrange(desc(`T.O.S.`))
            df$product_category <- factor(df$product_category, levels = df$product_category)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = product_category, fill = product_category, y = `T.O.S.`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = function(x){paste(x, "min")})
            }
        } else if(input$conv_product_category_metric == "Sessions"){
            df <- product_category_summary %>%
                filter(Sessions > 0) %>%
                arrange(desc(Sessions))
            df$product_category <- factor(df$product_category, levels = df$product_category)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = product_category, fill = product_category, y = Sessions)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_product_category_metric == "Total Carts"){
            df <- product_category_summary %>%
                rename(`Total Carts` = Adds_to_Cart) %>%
                filter(`Total Carts` > 0) %>%
                arrange(desc(`Total Carts`))
            df$product_category <- factor(df$product_category, levels = df$product_category)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = product_category, fill = product_category, y = `Total Carts`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = short_number)
            }
        } else if(input$conv_product_category_metric == "Bounce Rate"){
            df <- product_category_summary %>%
                filter(`Bounce Rate` > 0) %>%
                arrange(desc(`Bounce Rate`))
            df$product_category <- factor(df$product_category, levels = df$product_category)
            if(nrow(df) > 0){
                df %>%
                    ggplot(aes(x = product_category, fill = product_category, y = `Bounce Rate`)) +
                    geom_bar(stat = "identity") +
                    theme_bw(base_size = 14) +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none") +
                    scale_y_continuous(labels = percent)
            }
        }
    })
    # ---- Trends ----
    conv_trends_data <- reactive({
        filtered_ga_fb() %>%
            group_by(Date) %>%
            conv_kpi_summarise()
    })
    
    output$conv_acquisition <- renderPlot({
        conv_trends_data() %>%
            ggplot(aes(x = Date)) +
            geom_line(aes(y = CAC, color = "CAC"), group = 1) +
            geom_line(aes(y = CPAC, color = "CPAC"), group = 1) + 
            theme_minimal(base_size = 16) +
            scale_y_continuous(labels = short_dollar) +
            theme(axis.title.y = element_blank(),
                  legend.title = element_blank())
    })
    
    output$conv_cpc <- renderPlot({
        conv_trends_data() %>%
            ggplot(aes(x = Date)) +
            geom_line(aes(y = CPC, color = "CPC"), group = 1) +
            theme_minimal(base_size = 16) +
            scale_y_continuous(labels = dollar) +
            theme(axis.title.y = element_blank(),
                  legend.title = element_blank())
    })
    
    output$conv_ctr <- renderPlot({
        conv_trends_data() %>%
            ggplot(aes(x = Date)) +
            geom_line(aes(y = CTR, color = "CTR"), group = 1) +
            theme_minimal(base_size = 16) +
            scale_y_continuous(labels = percent) +
            theme(axis.title.y = element_blank(),
                  legend.title = element_blank())
    })
    
    output$conv_reach <- renderPlot({
        conv_trends_data() %>%
            ggplot(aes(x = Date)) +
            geom_line(aes(y = Reach, color = "Reach"), group = 1) +
            theme_minimal(base_size = 16) +
            scale_y_continuous(labels = short_number) +
            theme(axis.title.y = element_blank(),
                  legend.title = element_blank())
    })
    
    output$conv_sessions <- renderPlot({
        conv_trends_data() %>%
            ggplot(aes(x = Date)) +
            geom_line(aes(y = Sessions, color = "Sessions"), group = 1) +
            theme_minimal(base_size = 16) +
            scale_y_continuous(labels = short_number) +
            theme(axis.title.y = element_blank(),
                  legend.title = element_blank())
    })
    
    output$conv_clicks <- renderPlot({
        conv_trends_data() %>%
            rename(Clicks = Unique_Clicks) %>%
            ggplot(aes(x = Date)) +
            geom_line(aes(y = Clicks, color = "Clicks"), group = 1) +
            theme_minimal(base_size = 16) +
            scale_y_continuous(labels = short_number) +
            theme(axis.title.y = element_blank(),
                  legend.title = element_blank())
    })
    
    output$conv_purchases <- renderPlot({
        conv_trends_data() %>%
            ggplot(aes(x = Date)) +
            geom_line(aes(y = Purchases, color = "Purchases"), group = 1) +
            theme_minimal(base_size = 16) +
            scale_y_continuous(labels = short_number) +
            theme(axis.title.y = element_blank(),
                  legend.title = element_blank())
    })
# ---- Finances Tab ----
    quarter_filter <- reactive({
        if(input$quarter == "All Year") {
            seq(1, 4)
        } else {
            input$quarter %>% str_replace("Q", "") %>% as.numeric()
        }
    })
    
    monthly_budget_actuals <- reactive({
        monthly_budget_actuals_2017 %>%
            filter(ship_quarter %in% quarter_filter())
    })
    
    quarterly_and_annual_budget_actuals <- reactive({
        if(input$quarter == "All Year"){
            annual_budget_actuals_2017
        } else {
            quarterly_budget_actuals_2017 %>%
                filter(ship_quarter %in% quarter_filter())
        }
    })
    
    budget_v_actual_plot <- function(metric_value, metric_units = "dollar", ylabel){
        if(nrow(monthly_budget_actuals()) > 0){
            plot_data <- monthly_budget_actuals() %>%
                filter(metric == metric_value)
            
            max_lim <- c(plot_data$actuals_2017,
                         plot_data$budget_2017) %>% max()
            
            label_func <- 
                if(metric_units == "number") {
                    short_number
                } else if(metric_units == "rate"){
                    percent 
                } else { short_dollar }
            
            plot_data %>%
                ggplot(aes(x = ship_month)) +
                geom_bar(aes(y = actuals_2017), stat = "identity") + 
                geom_bar(aes(y = budget_2017), stat = "identity", alpha = 0, color = "black", size = 0.35) +
                geom_label(aes(label = percent(percent_of_budget), y = actuals_2017), vjust = -0.15, size = 6) + 
                scale_y_continuous(labels = label_func, limits = c(0, max_lim * 1.1)) +
                theme_minimal(base_size = 16) +
                xlab("Month") +
                ylab(ylabel)
        } else {
            ggplot() + theme_minimal(base_size = 24) + ggtitle("No data available for this quarter yet.")
        }
    }
    
    finance_downloader <- function(reactive_df, export_name){
        downloadHandler(
            filename = function() { paste0("Finances - ", export_name, " ", today(), ".csv") },
            content = function(file) { write_csv(reactive_df %>% select(-metric), file, na = "") }
        )
    }
    
    # ---- Gross Revenue Budget vs Actual ----
    line_item_details <- reactive({
        products_sold %>%
            filter(year(ship_date) %in% as.numeric(input$year)
                   & quarter(ship_date) %in% quarter_filter()) %>%
            mutate(ship_quarter = quarter(ship_date),
                   ship_month = month(ship_date)) %>%
            arrange(ship_date) %>%
            select(line_item_id, order_number, currency, order_state, payment_state, order_status,
                   gross_revenue_usd, shipping_usd, taxes_usd, promotions_usd,
                   other_adjustments_usd, adjustments_usd, sales_usd, order_date, ship_date)
    })
    
    output$download_finances <- downloadHandler(
        filename = function() { paste("Finances ", today(), ".csv", sep='') },
        content = function(file) {
            write_csv(line_item_details(), file, na = "")
        })
    
    gross_revenue <- reactive(quarterly_and_annual_budget_actuals() %>% filter(metric == "gross_revenue"))
    output$gross_revenue_actual <- renderText(dollar(gross_revenue()$actuals_2017[1]))
    output$gross_revenue_pob <- renderText(percent(gross_revenue()$percent_of_budget[1]))
    output$gross_revenue_yoy <- renderText(percent(gross_revenue()$percent_change_yoy[1]))
    
    units_shipped <- reactive(quarterly_and_annual_budget_actuals() %>% filter(metric == "units_shipped"))
    output$units_shipped_actual <- renderText(format(units_shipped()$actuals_2017[1], big.mark = ","))
    output$units_shipped_pob <- renderText(percent(units_shipped()$percent_of_budget[1]))
    output$units_shipped_yoy <- renderText(percent(units_shipped()$percent_change_yoy[1]))
    
    asp <- reactive(quarterly_and_annual_budget_actuals() %>% filter(metric == "average_selling_price"))
    output$asp_actual <- renderText(dollar(asp()$actuals_2017[1]))
    output$asp_pob <- renderText(percent(asp()$percent_of_budget[1]))
    output$asp_yoy <- renderText(percent(asp()$percent_change_yoy[1]))
    
    cogs <- reactive(quarterly_and_annual_budget_actuals() %>% filter(metric == "cogs"))
    output$cogs_actual <- renderText(dollar(cogs()$actuals_2017[1]))
    output$cogs_pob <- renderText(percent(cogs()$percent_of_budget[1]))
    output$cogs_yoy <- renderText(percent(cogs()$percent_change_yoy[1]))
    
    average_unit_cogs <- reactive(quarterly_and_annual_budget_actuals() %>% filter(metric == "average_unit_cogs"))
    output$average_unit_cogs_actual <- renderText(dollar(average_unit_cogs()$actuals_2017[1]))
    output$average_unit_cogs_pob <- renderText(percent(average_unit_cogs()$percent_of_budget[1]))
    output$average_unit_cogs_yoy <- renderText(percent(average_unit_cogs()$percent_change_yoy[1]))
    
    returns <- reactive(quarterly_and_annual_budget_actuals() %>% filter(metric == "returns"))
    output$returns_actual <- renderText(dollar(returns()$actuals_2017[1]))
    output$returns_pob <- renderText(percent(returns()$percent_of_budget[1]))
    output$returns_yoy <- renderText(percent(returns()$percent_change_yoy[1]))
    
    return_rate <- reactive(quarterly_and_annual_budget_actuals() %>% filter(metric == "return_rate"))
    output$return_rate_actual <- renderText(percent(return_rate()$actuals_2017[1]))
    output$return_rate_pob <- renderText(percent(return_rate()$percent_of_budget[1]))
    output$return_rate_yoy <- renderText(percent(return_rate()$percent_change_yoy[1]))
    
    returns_per_unit <- reactive(quarterly_and_annual_budget_actuals() %>% filter(metric == "returns_per_unit"))
    output$returns_per_unit_actual <- renderText(dollar(returns_per_unit()$actuals_2017[1]))
    output$returns_per_unit_pob <- renderText(percent(returns_per_unit()$percent_of_budget[1]))
    output$returns_per_unit_yoy <- renderText(percent(returns_per_unit()$percent_change_yoy[1]))
    
    gross_margin <- reactive(quarterly_and_annual_budget_actuals() %>% filter(metric == "gross_margin"))
    output$gross_margin_actual <- renderText(percent(gross_margin()$actuals_2017[1]))
    output$gross_margin_pob <- renderText(percent(gross_margin()$percent_of_budget[1]))
    output$gross_margin_yoy <- renderText(percent(gross_margin()$percent_change_yoy[1]))
    
    output$gross_revenue <- renderPlot(budget_v_actual_plot("gross_revenue", ylabel = "Revenue (USD)"))
    output$gross_revenue_down <- finance_downloader(monthly_budget_actuals() %>% filter(metric == "gross_revenue"), "Gross Revenue")
    
    output$gross_margin <- renderPlot(budget_v_actual_plot("gross_margin", "rate", ylabel = "Revenue (USD)"))
    output$gross_margin_down <- finance_downloader(monthly_budget_actuals() %>% filter(metric == "gross_margin"), "Gross Margin")
    
    output$cogs <- renderPlot(budget_v_actual_plot("cogs", ylabel = "Cost (USD)"))
    output$cogs_down <- finance_downloader(monthly_budget_actuals() %>% filter(metric == "cogs_down"), "COGS")
    
    output$units_shipped <- renderPlot(budget_v_actual_plot("units_shipped", "number", ylabel = "Units"))
    output$units_shipped_down <- finance_downloader(monthly_budget_actuals() %>% filter(metric == "units_shipped"), "Units Shipped")
    
    output$average_selling_price <- renderPlot(budget_v_actual_plot("average_selling_price", ylabel = "Price (USD)"))
    output$average_selling_price_down <- finance_downloader(monthly_budget_actuals() %>% filter(metric == "average_selling_price_down"), "ASP")
    
    output$average_unit_cogs <- renderPlot(budget_v_actual_plot("average_unit_cogs", ylabel = "Cost (USD)"))
    output$average_unit_cogs_down <- finance_downloader(monthly_budget_actuals() %>% filter(metric == "average_unit_cogs"), "Avg. Unit COGS")
    
    output$returns <- renderPlot(budget_v_actual_plot("returns", ylabel = "Lost Revenue (USD)"))
    output$returns_down <- finance_downloader(monthly_budget_actuals() %>% filter(metric == "returns"), "Returns")
    
    output$return_rate <- renderPlot(budget_v_actual_plot("return_rate", "rate", ylabel = "Revenue (USD)"))
    output$return_rate_down <- finance_downloader(monthly_budget_actuals() %>% filter(metric == "return_rate"), "Return Rate")
    
    output$returns_per_unit <- renderPlot(budget_v_actual_plot("returns_per_unit", ylabel = "Revenue Lost (USD)"))
    output$returns_per_unit_down <- finance_downloader(monthly_budget_actuals() %>% filter(metric == "returns_per_unit"), "Returns per Unit")
})
