shinyServer(function(input, output) {
    
    # ---- Data Set Filters ----
    
    filtered_sales <- reactive({
        sales <- products_sold %>%
            filter(between(order_date, input$order_dates[1], input$order_dates[2]))
        return(sales)
    })
    
    
    output$style_ranking <- renderDataTable({
        filtered_sales() %>%
            filter(revenue_usd > 0) %>%
            group_by(`Style Name` = style_name) %>%
            summarise(Units = n(),
                      Revenue = sum(revenue_usd),
                      `Return Rate` = sum(revenue_usd * item_returned) / sum(revenue_usd),
                      `Customization Rate` = sum(revenue_usd * physically_customized) / sum(revenue_usd)) %>%
            arrange(desc(Revenue)) %>%
            datatable(rownames = FALSE) %>%
            formatCurrency(c("Units"), digits = 0, currency = "") %>%
            formatCurrency(c("Revenue")) %>%
            formatPercentage(c("Return Rate", "Customization Rate"))
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
