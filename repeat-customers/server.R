library(shiny)

shinyServer(function(input, output) {
    
    # ---- Master Dataset ---- 
    
    skirt_select <- reactive({ if(input$skirt == T) {c(T)} else {c(T,F)}})
    evening_select <- reactive({ if(input$evening == T){c(T)} else {c(T,F)}})
    cocktail_select <- reactive({ if(input$cocktail == T) {c(T)} else {c(T,F)}})
    bridal_select <- reactive({ if(input$bridal == T) {c(T)} else {c(T,F)}})
    daytime_select <- reactive({ if(input$daytime == T) {c(T)} else {c(T,F)}})
    jumpsuit_select <- reactive({ if(input$jumpsuit == T) {c(T)} else {c(T,F)}})
    
    filtered_sales <- reactive({
        
        sales %>%
            filter(
                between(ship_date, 
                        input$ship_dates[1], 
                        input$ship_dates[2])) %>%
            filter(customer_engagement_level %in% input$cust_eng) %>%
            filter(short_long %in% input$short_long) %>%
            filter(skirt %in% skirt_select()) %>%
            filter(evening %in% evening_select()) %>%
            filter(cocktail %in% cocktail_select()) %>%
            filter(bridal %in% bridal_select()) %>%
            filter(daytime %in% daytime_select()) %>%
            filter(jumpsuit %in% jumpsuit_select())
        
    })
    
    # ---- Monthly Revenue ----
    monthly_new_repeat_data <- reactive({
        filtered_sales() %>%
            group_by(ship_year_month, customer_engagement_level) %>%
            summarise(`Revenue (USD)` = sum(revenue_usd))
    })
    
    output$monthly_new_repeat <- renderPlot({
        monthly_new_repeat_data() %>%
            ggplot(aes(x = ship_year_month, y = `Revenue (USD)`,fill = customer_engagement_level)) +
            geom_bar(stat = "identity", color = "black", size = 0.1) +
            scale_y_continuous(labels = dollar)+
            theme_bw(base_size = 14) +
            theme(legend.title = element_blank(),
                  axis.text.x = element_text(hjust = 1, angle = 45),
                  legend.position = "none",
                  axis.title.x = element_blank())
    })
    
    output$monthly_new_repeat_down <- downloadHandler(
        filename = function() { paste0("Monthly Revenue ", today(), ".csv") },
        content = function(file) { write_csv(monthly_new_repeat_data(), file) }
    )
    
    # ---- Monthly Repeat Rate ----
    repeat_rate_data <- reactive({
        filtered_sales() %>%
            anti_join(same_dress, by = "id") %>%
            group_by(ship_year_month) %>%
            summarise(customers = n_distinct(email),
                      repeat_customers = n_distinct(ifelse(email %in% repeat_customers, email, NA), na.rm = TRUE),
                      `Repeat Rate` = repeat_customers / customers)
    })
    
    output$repeat_rate <- renderPlot({
        repeat_rate_data() %>%
             ggplot(aes(x = ship_year_month, y = `Repeat Rate`)) + 
             geom_bar(stat = "identity") + 
             scale_y_continuous(labels = percent) +
             theme_bw(base_size = 14) +
             theme(axis.text.x = element_text(hjust = 1, angle = 45),
                   axis.title.x = element_blank())
         
     })
     
    output$repeat_rate_down <- downloadHandler(
        filename = function() { paste("Monthly Repeat Rate ", today(), ".csv", sep='') },
        content = function(file) {
            write_csv(repeat_rate_data(), file)
        }
    )
    
    # ---- Top Return Reasons ----
    return_reasons_data <- reactive({
        rr_sales <- 
            filtered_sales() %>%
            filter(!is.na(return_order_id)) %>%
            rename(return_reason_long = return_reason) %>%
            mutate(return_reason = ifelse(
                return_reason_long == "Looks Different To Image On Site",
                "Looks Different\nTo Image On\nSite",
                ifelse(
                    return_reason_long == "Ordered Multiple Styles Or Sizes",
                    "Ordered Multiple\nStyles Or Sizes",
                    ifelse(
                        return_reason_long == "Poor Quality Or Faulty",
                        "Poor Quality\nOr Faulty",
                        ifelse(is.na(return_reason_long) 
                               | return_reason_long %in% c("N/A","Na","Not Specified","Not Stated","Not Satisfied"),
                               "No Reason",
                               return_reason_long)))))
        
        orders_by_return_reason <- 
            rr_sales %>% 
            filter(return_reason != "No Reason") %>% 
            count(return_reason) %>% 
            arrange(n)
        
        rr_sales$return_reason <- factor(
            rr_sales$return_reason,
            levels = c(orders_by_return_reason$return_reason, "No Reason"))
        
        rr_sales %>% 
            group_by(return_reason) %>%
            summarise(`Units Returned` = n())
    })
    
     output$return_reasons <- renderPlot({
         rr_sales <- 
             filtered_sales() %>%
             filter(!is.na(return_order_id)) %>%
             rename(return_reason_long = return_reason) %>%
             mutate(return_reason = ifelse(
                 return_reason_long == "Looks Different To Image On Site",
                 "Looks Different\nTo Image On\nSite",
                 ifelse(
                     return_reason_long == "Ordered Multiple Styles Or Sizes",
                     "Ordered Multiple\nStyles Or Sizes",
                     ifelse(
                         return_reason_long == "Poor Quality Or Faulty",
                         "Poor Quality\nOr Faulty",
                         ifelse(is.na(return_reason_long) 
                                | return_reason_long %in% c("N/A","Na","Not Specified","Not Stated","Not Satisfied"),
                                "No Reason",
                                return_reason_long)))))
         
         orders_by_return_reason <- 
             rr_sales %>% 
             filter(return_reason != "No Reason") %>% 
             count(return_reason) %>% 
             arrange(n)
         
         rr_sales$return_reason <- factor(
             rr_sales$return_reason,
             levels = c(orders_by_return_reason$return_reason, "No Reason"))
         
         rr_sales %>% 
             group_by(`Return Reason` = return_reason) %>%
             summarise(`Units Returned` = n()) %>%
             ggplot(aes(x = `Return Reason`, y = `Units Returned`)) +
             geom_bar(stat = "identity") +
             coord_flip() +
             theme_bw(base_size = 12) +
             theme(axis.title.y = element_blank(),
                   legend.title = element_blank(),
                   axis.ticks.y = element_blank())
     })
     
     output$return_reasons_down <- downloadHandler(
         filename = function() { paste("Top Return Reasons ", today(), ".csv", sep='') },
         content = function(file) {
             write_csv(return_reasons_data(), file)
         }
     )
     
    # ---- Top Styles by Revenue ----
    top_styles_data <- reactive({
        top_10_styles <-
            (filtered_sales() %>%
                 group_by(style_name) %>%
                 summarise(revenue = sum(revenue_usd)) %>%
                 arrange(desc(revenue)))$style_name[1:10]
        
        style_sales <- 
            filtered_sales() %>%
            filter(style_name %in% top_10_styles) %>%
            group_by(Style = style_name, customer_engagement_level) %>%
            summarise(`Revenue (USD)` = sum(revenue_usd))
        
        style_sales$Style = factor(style_sales$Style, 
                                        levels = rev(top_10_styles))
        return(style_sales)
    })
    
     output$top_styles <- renderPlot({
         top_styles_data() %>%
             ggplot(aes(x = Style, y = `Revenue (USD)`, fill = customer_engagement_level)) +
             geom_bar(stat = "identity") +
             scale_y_continuous(labels = dollar) +
             coord_flip() +
             theme_bw(base_size = 12) +
             theme(axis.title.y = element_blank(),
                   legend.title = element_blank())
     })
     
     output$top_styles_down <- downloadHandler(
         filename = function() { paste("Top Sytles by Revenue ", today(), ".csv", sep='') },
         content = function(file) {
             write_csv(top_styles_data(), file)
         }
     )
     
    # ---- Monthly Return Rates ----
     return_rate_data <- reactive({
         filtered_sales() %>%
             group_by(ship_year_month) %>%
             summarise(`Return Rate` = sum(item_returned * revenue_usd) / sum(revenue_usd))
     })
     
     output$return_rate <- renderPlot({
         return_rate_data() %>%
             ggplot(aes(x = ship_year_month, y = `Return Rate`)) +
             geom_bar(stat = "identity") +
             theme_bw(base_size = 14) +
             scale_y_continuous(labels = percent) +
             theme(axis.text.x = element_text(hjust = 1, angle = 45),
                   axis.title.x = element_blank())
     })
     
     output$return_rate_down <- downloadHandler(
         filename = function() { paste("Monthly Return Rates ", today(), ".csv", sep='') },
         content = function(file) {
             write_csv(return_rate_data(), file)
         }
     )
     
    # ---- Monthly Return Reasons ----
     return_reasons_data <- reactive({
         filtered_sales() %>%
             filter(
                 !is.na(return_order_id)
                 & !is.na(return_reason) 
                 & !(return_reason %in% c(
                     "N/A",
                     "Na",
                     "Not Specified",
                     "Not Stated",
                     "Not Satisfied",
                     "No Reason"))) %>%
             group_by(ship_year_month, return_reason) %>%
             summarise(`Revenue (USD)` = sum(revenue_usd))
     })
     
     output$monthly_return_reasons <- renderPlot({
         return_reasons_data() %>%
             ggplot(aes(x = ship_year_month, y = `Revenue (USD)`, fill = return_reason)) +
             geom_bar(stat = "identity", position = 'fill', colour="black", size = 0.1) +
             scale_y_continuous(labels = percent) +
             theme_bw(base_size = 14) +
             theme(legend.title = element_blank(),
                   legend.position = "none",
                   axis.text.x = element_text(hjust = 1, angle = 45),
                   axis.title.x = element_blank())
     })
     
     output$monthly_return_reasons_down <- downloadHandler(
         filename = function() { paste("Monthly Return Reasons ", today(), ".csv", sep='') },
         content = function(file) {
             write_csv(return_reasons_data(), file)
         }
     )
     
    # ---- Purchases by Return Reason ---- 
     which_order_return_reasons_data <- reactive({
         which_order_return_sales <-
             filtered_sales() %>%
             filter(!is.na(return_reason) & return_reason != "No Reason") %>%
             group_by(return_reason, which_order) %>%
             summarise(`Revenue (USD)` = sum(revenue_usd))
         
         which_order_return_sales$which_order <- factor(
             which_order_return_sales$which_order,
             sort(unique(which_order_return_sales$which_order), TRUE)
         )
         
         return(which_order_return_sales)
     })
     
     output$which_order_return_reasons <- renderPlot({
         which_order_return_reasons_data() %>%
             ggplot(aes(x = which_order, y = `Revenue (USD)`, fill = return_reason)) +
             geom_bar(stat = "sum", position = "fill", color = "black", size = 0.1) +
             scale_y_continuous(labels = percent) +
             theme_bw(base_size = 14) +
             theme(legend.title = element_blank(),
                   axis.title.y = element_blank()) +
             coord_flip()
     })
     
     output$which_order_return_reasons_down <- downloadHandler(
         filename = function() { paste("Purchases by Return Reason ", today(), ".csv", sep='') },
         content = function(file) {
             write_csv(which_order_return_reasons_data(), file)
         }
     )
     
    # ---- Top Cities by Revenue ----
     top_cities_data <- reactive({
         top_10_cities_by_rev <- 
             (filtered_sales() %>%
                  group_by(ship_city) %>%
                  summarise(revenue = sum(revenue_usd)) %>%
                  arrange(desc(revenue)))$ship_city[1:10]
         
         top_cities_with_elevel <- 
             filtered_sales() %>%
             filter(ship_city %in% top_10_cities_by_rev) %>%
             group_by(ship_city, customer_engagement_level) %>%
             summarise(revenue = sum(revenue_usd))
         
         top_cities_with_elevel$ship_city <- factor(
             top_cities_with_elevel$ship_city,
             levels = rev(top_10_cities_by_rev)
         )
         
         return(top_cities_with_elevel)
     })
     
     output$top_cities <- renderPlot({
         top_cities_data() %>%
             rename(`Revenue (USD)` = revenue) %>%
             ggplot(aes(x = ship_city, y = `Revenue (USD)`, fill = customer_engagement_level)) +
             geom_bar(stat = "identity") +
             scale_y_continuous(labels = dollar) +
             coord_flip() +
             theme_bw(base_size = 14) +
             theme(axis.title.y = element_blank(),
                   legend.title = element_blank())
     })
     
     output$top_cities_down <- downloadHandler(
         filename = function() { paste0("Top Cities ", today(), ".csv") },
         content = function(file) { write_csv(top_cities_data(), file) }
     )
     
    # ---- Monthly Sales by Color ----
     color_year_data <- reactive({
         top_colors <- 
             (filtered_sales() %>%
                  count(color) %>%
                  arrange(desc(n)))$color[1:10]
         
         filtered_sales() %>%
             filter(color %in% top_colors & !is.na(color)) %>%
             group_by(ship_year_month, color) %>%
             summarise(`Units Sold` = n())
     })
     
     output$color_year <- renderPlot({
         color_year_data() %>%
             ggplot(aes(x = ship_year_month, y = `Units Sold`, fill = color, group = 1)) +
             geom_bar(stat = "identity", color = "black", size = 0.3) +
             theme_bw(base_size = 14) +
             theme(axis.title.x = element_blank(),
                   axis.text.x = element_text(hjust = 1, angle = 45),
                   legend.title = element_blank())
     })
     
     output$color_year_down <- downloadHandler(
         filename = function() { paste("Monthly Sales by Color ", today(), ".csv", sep='') },
         content = function(file) {
             write_csv(color_year_data(), file)
         }
     )
    
    # ---- Avg Days between Sales ----
     avg_days_to_purchase_data <- reactive({
         filtered_sales() %>%
             filter(customer_engagement_level != "New Customer") %>% 
             group_by(email) %>% 
             arrange(order_num) %>%
             mutate(days_since_last_purchase = as.numeric(order_date - lag(order_date)), 
                    purchase_from = ifelse(
                        which_order == "2nd Purchase", "1st to 2nd Purchase", ifelse(
                            which_order == "3rd+ Purchase", "2nd to 3rd Purchase", NA))) %>% 
             ungroup() %>% 
             filter(!is.na(purchase_from) 
                    & !is.na(days_since_last_purchase)) %>%
             group_by(purchase_from) %>%
             summarise(avg_days = mean(days_since_last_purchase))
     })
     
     output$avg_days_to_purchase <- renderPlot({
         avg_days_to_purchase_data() %>%
             ggplot(aes(x = purchase_from, y = avg_days)) +
             geom_bar(stat = "identity") +
             theme_bw(base_size = 14) +
             theme(axis.title.x = element_blank()) +
             ylab("Avg Days")
     })
     
     output$avg_days_to_purchase_down <- downloadHandler(
         filename = function() { paste("Avg Days between Sales ", today(), ".csv", sep='') },
         content = function(file) {
             write_csv(avg_days_to_purchase_data(), file)
         }
     )
     
    # ---- Avg LTV by Top Cities ----
     avg_ltv_data <- reactive({
         top_cities <- 
             (filtered_sales() %>%
                  filter(customer_engagement_level != "New Customer") %>%
                  group_by(ship_city) %>%
                  summarise(rev = sum(revenue_usd)) %>%
                  top_n(5, rev) %>%
                  arrange(desc(rev)))$ship_city
         
         monthly_ltv <- 
             filtered_sales() %>%
             filter(ship_city %in% top_cities & customer_engagement_level != "New Customer") %>%
             group_by(ship_city, email) %>%
             summarise(ca_date = min(order_date),
                       ltv = sum(revenue_usd)) %>%
             mutate(ca_year_qtr = paste(
                 year(ca_date), 
                 paste0("Q", quarter(ca_date)), 
                 sep = " - ")) %>%
             ungroup() %>%
             group_by(ship_city, ca_year_qtr) %>%
             summarise(avg_ltv = mean(ltv))
         
         monthly_ltv$ship_city <- factor(
             monthly_ltv$ship_city,
             levels = top_cities
         )
         
         monthly_ltv
     })
     
     output$avg_ltv <- renderPlot({
         avg_ltv_data() %>%
             ggplot(aes(x = ca_year_qtr, y = avg_ltv)) +
             geom_bar(stat = "identity") +
             facet_grid(. ~ ship_city) +
             scale_y_continuous(labels = dollar) +
             theme_bw(base_size = 14) +
             theme(axis.title.x = element_blank(),
                   legend.title = element_blank(),
                   axis.text.x = element_text(hjust = 1, angle = 45)) +
             ylab("Avg LTV")
     })
     
     output$avg_ltv_down <- downloadHandler(
         filename = function() { paste("Avg LTV by Top 5 Cities ", today(), ".csv", sep='') },
         content = function(file) {
             write_csv(avg_ltv_data(), file)
         }
     )
     
    # ---- Customer Purchase History Export ----
     datasetInput <- reactive({
         down <- 
             filtered_sales() %>%
             group_by(email) %>%
             mutate(`Purchase` = paste0(
                 order_num, 
                 ifelse(order_num == 1, "st",
                        ifelse(order_num == 2, "nd",
                               ifelse(order_num == 3, "rd",
                                      "th"))),
                 " Purchase"),
                 `LTV` = sum(revenue_usd)) %>%
             ungroup()
         
         down %>%
             transmute(
                 `Customer` = customer_name,
                 `Customer Email` = email,
                 `LTV`,
                 `Order Date` = order_date,
                 `Order Number` = order_number,
                 `Purchase`,
                 `Amount (USD)` = revenue_usd,
                 `Style` = style_name,
                 `Color` = color,
                 `Size` = size) %>%
             ungroup() %>%
             arrange(`Customer Email`, `Order Date`, `Purchase`)
     })
     
     output$download_table <- renderDataTable(
         datasetInput()
     )
     
     output$downloadData <- downloadHandler(
         filename = function() { paste("Repeat Customer Sales ", today(), ".csv", sep='') },
         content = function(file) {
             write_csv(datasetInput(), file)
         }
     )
     
})
