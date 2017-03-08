shinyServer(function(input, output) {
    # ---- Conversions Tab ----
    
    
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
    
    # ---- Return Reasons ----
    
    return_reason_data <- reactive({
        selected_sales() %>%
            filter(item_returned) %>%
            group_by(return_reason) %>%
            summarise(Units = n())
    })
    
    output$return_reasons <- renderPlot({
        returns_df <- return_reason_data()
        
        if(nrow(return_reason_data()) > 0){
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
    
    # ---- Monthly Customization Rates ----
    output$cust_rates <- renderPlot({
        selected_sales() %>%
            group_by(order_year_month) %>% 
            summarise(Units = sum(physically_customized * quantity) / sum(quantity)) %>% 
            ggplot(aes(x = order_year_month, y = Units)) + 
            geom_bar(stat = "identity") +
            scale_y_continuous(labels = percent) +
            theme_bw(base_size = 14) +
            theme(axis.title.x = element_blank(),
                  legend.title = element_blank(),
                  axis.text.x = element_text(hjust = 1, angle = 45))
    })
    
    # ---- Size Distrobution ----
    output$size_dist <- renderPlot({
        selected_sales() %>%
            filter(height %in% c("Petite", "Standard", "Tall")) %>%
            group_by(height, size) %>%
            summarise(Units = sum(quantity)) %>%
            arrange(desc(size)) %>%
            ggplot(aes(x = size, y = Units)) +
            geom_bar(stat = "identity") +
            theme_bw(base_size = 14) +
            #coord_flip() +
            theme(axis.title.x = element_blank()) +
            facet_grid(height ~ .)
    })
    
    
    # ---- Returns Tab ----
    
    # ---- Factory Returns ----
    
    output$factory_returns <- renderPlot({
        products_sold %>% 
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
})
