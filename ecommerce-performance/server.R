shinyServer(function(input, output) {
    
    # ---- Global Data Set Filters ----
    
    filtered_sales <- reactive({
        sales <- products_sold %>%
            filter(between(order_date, input$order_dates[1], input$order_dates[2])) %>%
            filter(collection %in% input$collections)
        return(sales)
    })
    
    # ---- Top Styles ----
    style_ranking_data <- reactive({
        filtered_sales() %>%
            filter(revenue_usd > 0 & order_state == 'complete') %>%
            group_by(style_number) %>%
            summarise(`Style Name` = paste(unique(style_name), collapse = ","),
                      Units = n(),
                      Revenue = sum(revenue_usd),
                      `Return Rate` = sum(revenue_usd * item_returned) / sum(revenue_usd),
                      `Customization Rate` = sum(revenue_usd * physically_customized) / sum(revenue_usd)) %>%
            arrange(desc(Revenue))
    })
    
    output$style_ranking <- renderDataTable({
        style_ranking_data() %>%
            rename(`Style Number` = style_number) %>%
            datatable(rownames = FALSE, class = "hover row-border", style = "bootstrap") %>%
            formatCurrency(c("Units"), digits = 0, currency = "") %>%
            formatCurrency(c("Revenue")) %>%
            formatPercentage(c("Return Rate", "Customization Rate"))
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
    
    # ---- Weekly Sales ----
    
    weekly_sales_data <- reactive({
        selected_sales() %>%
            group_by(order_date, order_status) %>%
            summarise(order_week_start = min(order_date),
                      `Revenue USD` = sum(revenue_usd))
    })
    
    # output$rows_test <- renderText({
    #     dftest <- weekly_sales_data()
    #     nrow(dftest)
    # })
    
    output$weekly_sales <- renderPlot({
        weekly_sales_data() %>%
            ggplot(aes(x = order_date, y = `Revenue USD`, fill = order_status)) +
            geom_bar(stat = "identity", color = "black", size = 0.2) +
            scale_y_continuous(labels = dollar) +
            theme_bw(base_size = 12) +
            theme(axis.title.x = element_blank(),
                  legend.title = element_blank())
    })
    
    # ---- Return Reasons ----
    # output$distPlot2 <- renderPlot({
    #     
    #     returns %>%
    #         group_by(Reason, `Sub Reason`) %>%
    #         summarise(items = sum(items)) %>%
    #         ggplot(aes(x = Reason, y = items, fill = `Sub Reason`)) +
    #         geom_bar(stat = "identity", position = "fill", color = "black", size = 0.25) +
    #         scale_y_continuous(labels = percent) + 
    #         theme_bw(base_size = 12) +
    #         coord_flip() + 
    #         theme(axis.title = element_blank(),
    #               legend.title = element_blank())
    #     
    # })
})
