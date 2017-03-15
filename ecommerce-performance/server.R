shinyServer(function(input, output) {
    library(scales)
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

    product_live_filter <- reactive({
        if(length(input$live) > 0) { input$live } else { c("Yes", "No") }
    })
    
    filtered_sales <- reactive({
        sales <- products_sold %>%
            filter(between(order_date, input$order_dates[1], input$order_dates[2])) %>%
            filter(collection %in% collection_filter()) %>%
            filter(between(price_usd, input$price_range[1], input$price_range[2])) %>%
            filter(between(us_size, input$us_size[1], input$us_size[2])) %>%
            filter(product_live %in% product_live_filter()) %>%
            filter(order_status %in% order_status_filter())
        return(sales)
    })
    
    # ---- Top Styles ----
    style_ranking_data <- reactive({
        filtered_sales() %>%
            filter(revenue_usd > 0) %>%
            group_by(style_number) %>%
            summarise(`Style Name` = paste(unique(style_name), collapse = ","),
                      Units = sum(quantity),
                      Revenue = sum(revenue_usd),
                      `Return Rate` = sum(coalesce(refund_amount_usd, 0)) / sum(revenue_usd),
                      `Customization Rate` = sum(quantity * physically_customized) / sum(quantity)) %>%
            arrange(desc(Revenue))
    })
    
    output$style_ranking <- renderDataTable({
        # Rename style number here so that selected_product_ids can work
        style_ranking_data() %>%
            rename(`Style Number` = style_number) %>%
            datatable(class = "hover row-border", style = "bootstrap") %>%
            formatCurrency(c("Units"), digits = 0, currency = "") %>%
            formatCurrency(c("Revenue")) %>%
            formatPercentage(c("Return Rate", "Customization Rate"))
    })
    
    output$style_ranking_down <- downloadHandler(
        filename = function() { paste("Top Styles ", today(), ".csv", sep='') },
        content = function(file) { write_csv(style_ranking_data() %>% 
                                                 rename(`Style Number` = style_number), 
                                             file, na = "") }
        )
    
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
        sum_stats <- 
            selected_sales() %>%
            filter(revenue_usd > 0) %>%
            summarise(`Total Units` = short_number(sum(quantity)),
                      `Total Revenue` = short_dollar(sum(revenue_usd)),
                      `Return Rate` = round(sum(coalesce(refund_amount_usd, 0)) / sum(revenue_usd), 2) %>% percent(),
                      `Customization Rate` = round(sum(quantity * physically_customized) / sum(quantity), 2) %>% percent())
        return(sum_stats)
    })
    
    # ---- Daily Sales ----
    
    daily_sales_data <- reactive({
        selected_sales() %>%
            group_by(order_date, order_status) %>%
            summarise(order_week_start = min(order_date),
                      `Revenue USD` = sum(revenue_usd))
    })
    
    output$daily_sales <- renderPlot({
        daily_sales_data() %>%
            ggplot(aes(x = order_date, y = `Revenue USD`, fill = order_status)) +
            geom_bar(stat = "identity", color = "black", size = 0.2) +
            scale_y_continuous(labels = dollar) +
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
                              shipment_state,
                              quantity,
                              price,
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
                              return_reason,
                              reason_sub_category,
                              color,
                              size,
                              product_live,
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
                                            max(selected_sales()$us_size), 1)) +
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
        filtered_for_returns() %>% filter(item_returned)
    })
    
    # ---- Return Reasons ----
    
    output$return_reasons <- renderPlot({
        returns_df <- filtered_returns() %>%
            filter(!is.na(return_reason)) %>%
            group_by(return_reason) %>%
            summarise(Units = n())
        
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
    
    output$sec_return_reasons <- renderDataTable({
        filtered_returns() %>%
            group_by(`Primary Return Reason` = return_reason, 
                     `Secondary Return Reason` = reason_sub_category) %>% 
            summarise(Units = n(),
                      `Revenue (USD)` = sum(revenue_usd)) %>%
            arrange(desc(Units)) %>%
            datatable(class = "hover row-border", style = "bootstrap", 
                      rownames = FALSE, selection = "none") %>%
            formatCurrency(c("Units"), digits = 0, currency = "") %>%
            formatCurrency(c("Revenue (USD)"))
    })
    
    # ---- Montly Return Rates ----
    output$monthly_return_rates <- renderPlot({
        filtered_for_returns() %>% 
            filter(order_status %in% c("Shipped","Returned")) %>%
            group_by(ship_year_month) %>% 
            summarise(`Revenue (USD)` = sum(coalesce(refund_amount_usd, 0)) / sum(revenue_usd)) %>% 
            ggplot(aes(x = ship_year_month, y = `Revenue (USD)`)) + 
            geom_bar(stat = "identity") +
            scale_y_continuous(labels = percent) +
            theme_bw(base_size = 14) +
            theme(legend.title = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.x = element_text(hjust = 1, angle = 35))
    })
    
    # ---- Factory Returns ----
    
    output$factory_returns <- renderPlot({
        filtered_for_returns() %>% 
            filter(order_status %in% c("Shipped","Returned") & !is.na(factory_name)) %>% 
            group_by(ship_year_month, factory_name) %>% 
            summarise(`Return Rate` = sum(coalesce(refund_amount_usd, 0)) / sum(revenue_usd)) %>% 
            ggplot(aes(x = ship_year_month, y = `Return Rate`)) + 
            geom_bar(stat = "identity") + 
            facet_grid(factory_name~.) +
            theme_bw(base_size = 14) +
            scale_y_continuous(labels = percent) +
            theme(axis.title.x = element_blank())
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
    
    conversion_sales <- reactive({
        products_sold %>%
            inner_join(all_touches %>% 
                           select(email, cohort) %>% 
                           unique(), 
                       by = "email") %>%
            filter(between(as.Date(order_date), input$conversion_dates[1], input$conversion_dates[2])) %>%
            filter(cohort %in% cohort_filter())
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
            scale_y_continuous(labels = short_number_space) +
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
            summarise(source = utm_source[1], revenue = sum(revenue_usd)) %>% 
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
    
    # ---- UTM Medium Conversions ----
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
        conversion_sales() %>%
            filter(cohort %in% c("Prom","Bridal","Contemporary")) %>% 
            group_by(order_year_month, cohort) %>% 
            summarise(Orders = n()) %>% 
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
})
