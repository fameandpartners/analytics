shinyUI(fluidPage(
    theme="styles.css",
    navbarPage(
        title = "eCommerce Performance",
        tabPanel(
            "Styles",
            
            fluidRow(
                column(
                    4,
                    dateRangeInput(
                        "order_dates",
                        label = "Order Date",
                        start = today() - 90,
                        end = today()
                    ),
                    selectInput(
                        "collections",
                        label = "Collection",
                        choices = sort(unique(products_sold$collection)),
                        multiple = TRUE
                    ),
                    # selectInput(
                    #     "live",
                    #     "Style Live on Website:",
                    #     choices = c("Yes","No"),
                    #     multiple = TRUE
                    # ),
                    sliderInput(
                        "us_size",
                        "Size (US):",
                        min = 0,
                        max = 20,
                        value = c(0,20)
                    ),
                    sliderInput(
                        "price_range",
                        "Price (USD):",
                        min = min(products_sold$price_usd),
                        max = max(products_sold$price_usd),
                        value = c(min(products_sold$price_usd), 
                                  max(products_sold$price_usd))
                    )
                ),
                column(
                    8,
                    div(tableOutput("kpis"), id= "kpis", align = "right"),
                    dataTableOutput("style_ranking")
                )
            ),
            
            hr(),
            
            fluidRow(
                h3("Daily Sales", align = "center"),
                plotOutput("weekly_sales")
            ),
            
            fluidRow(
                column(
                    6,
                    h3("Return Reasons", align = "center"),
                    plotOutput("return_reasons")
                ),
                column(
                    6,
                    h3("Top Colors", align = "center"),
                    plotOutput("top_colors")
                )
            ),
            
            fluidRow(
                downloadButton("download_all")
            )
        )
        # ,
        # tabPanel(
        #     "Conversions",
        #     fluidRow(
        #         h3("Cumulative Revenue"),
        #         plotOutput("cumulative_rev")
        #     )
        # )
    ),
    
    title = titlePanel(NULL, "Ecommerce Performance")
))