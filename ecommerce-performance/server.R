library(shiny)

shinyServer(function(input, output) {
    
    # ---- Data Set Filters ----
    
    filtered_sales <- reactive({
        return(products_sold)
    })
    
    
    output$distPlot <- renderDataTable({
        filtered_sales() %>%
            filter(revenue_usd > 0) %>%
            group_by(style_name) %>%
            summarise(units = n(),
                      revenue = sum(revenue_usd),
                      return_rate = sum(revenue_usd * item_returned) / sum(revenue_usd),
                      customization_rate = sum(revenue_usd * physically_customized) / sum(revenue_usd))
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
    
    output$distPlot3 <- renderPlot({
        
        
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
    })
    output$distPlot4 <- renderPlot({
        
        
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
    })
    
    output$distPlot5 <- renderPlot({
        
        
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
    })
    
})
