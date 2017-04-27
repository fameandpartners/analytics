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
    
    # ---- Montly Return Rates ----
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
        all_touches %>%
            filter(between(as.Date(added_to_cart_at), input$conversion_dates[1], input$conversion_dates[2])) %>%
            filter(cohort %in% cohort_filter()) %>%
            filter(utm_source %in% utm_source_filter()) %>%
            filter(utm_medium %in% utm_medium_filter()) %>%
            filter(utm_campaign %in% utm_campaign_filter())
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
        
        df %>%
            ggplot(aes(x = cart_year_month, y = `Cart to Purchase`)) +
            geom_line(group = 1) +
            geom_point() +
            geom_text(aes(label = paste0(round(`Cart to Purchase` * 100), "%"), vjust = vjust), size = 5) +
            scale_y_continuous(labels = percent, 
                               limits = c(min(df$`Cart to Purchase`)*(1 - 0.5),
                                          max(df$`Cart to Purchase`)*(1 + 0.5))) +
            theme_minimal(base_size = 14) +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank())
    })
    
    output$carts_orders <- renderPlot({
        carts_orders_data() %>% 
            ggplot(aes(x = cart_year_month, y = Orders, fill = step)) + 
            geom_bar(stat = "identity", position = "dodge") + 
            theme_minimal(base_size = 14) +
            scale_y_continuous(labels = short_number) +
            theme(axis.title.x = element_blank(),
                  legend.position = "bottom",
                  legend.title = element_blank()) +
            scale_fill_brewer(palette = "Set1")
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
        
        top10 %>%
            ggplot(aes(x = Source, y = `Revenue (USD)`)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = short_dollar(`Revenue (USD)`)), hjust = -0.05, size = 4) +
            scale_y_continuous(labels = short_dollar, limits = c(0, max(top10$`Revenue (USD)`)*1.25)) +
            coord_flip() +
            theme_bw(base_size = 14)
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
        
        ggplot(utm_source_conversions %>%
                   filter(utm_source %in% top5_m$utm_source), 
               aes(x = step, y = order_conversion)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = percent(order_conversion)), vjust = -1) +
            geom_text(aes(label = prettyNum(orders, big.mark = ",")), 
                      vjust = 1.6, color = "white") +
            scale_y_continuous(labels = percent) +
            facet_grid(utm_source ~.) +
            theme_bw(base_size = 16) +
            theme(axis.title = element_blank())
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
        
        ggplot(utm_med_conversions %>%
                   filter(utm_medium %in% top5_m$utm_medium), 
               aes(x = step, y = order_conversion)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = percent(order_conversion)), vjust = -1) +
            geom_text(aes(label = prettyNum(orders, big.mark = ",")), 
                      vjust = 1.6, color = "white") +
            scale_y_continuous(labels = percent) +
            facet_grid(utm_medium ~.) +
            theme_bw(base_size = 16) +
            theme(axis.title = element_blank())
    })
    
    # ---- UTM Campaign Conversions ----
    output$camp_conv <- renderPlot({
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
        
        ggplot(utm_camp_conversions %>%
                   filter(utm_campaign %in% top5_m$utm_campaign), 
               aes(x = step, y = order_conversion)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = percent(order_conversion)), vjust = -1) +
            geom_text(aes(label = prettyNum(orders, big.mark = ",")), 
                      vjust = 1.6, color = "white") +
            scale_y_continuous(labels = percent) +
            facet_grid(utm_campaign ~.) +
            theme_bw(base_size = 16) +
            theme(axis.title = element_blank())
    })
    
    # ---- Monthly Cohort Distrobution ----
    output$monthly_cohorts <- renderPlot({
        filtered_touches() %>%
            filter(cohort %in% c("Prom","Bridal","Contemporary")
                   & state == "complete") %>% 
            group_by(order_year_month, cohort) %>% 
            summarise(Orders = n_distinct(order_id)) %>% 
            ggplot(aes(x = order_year_month, y = Orders, fill = cohort)) + 
            geom_bar(stat = "identity", position = "fill", color = "black", size = 0.2) + 
            theme_bw(base_size = 14) +
            theme(axis.title.x = element_blank(),
                  legend.title = element_blank(),
                  legend.position = "bottom") +
            scale_fill_brewer(palette = "Set3") + 
            scale_y_continuous(labels = percent)
    })
    
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
