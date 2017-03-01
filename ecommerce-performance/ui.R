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
                    selectInput(
                        "order_status",
                        label = "Order Status",
                        choices = sort(unique(products_sold$order_status)),
                        multiple = TRUE
                    ),
                    selectInput(
                        "live",
                        "Style Live on Website:",
                        choices = c("Yes","No"),
                        multiple = TRUE
                    ),
                    sliderInput(
                        "us_size",
                        "Size (US):",
                        min = 0,
                        max = 22,
                        value = c(0,22)
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
                    div(tableOutput("kpis"), id = "kpi-wrapper"),
                    br(),
                    dataTableOutput("style_ranking"),
                    downloadButton("style_ranking_down")
                )
            ),
            
            hr(),
            
            fluidRow(
                h3c("Daily Sales"),
                plotOutput("daily_sales")
            ),
            
            fluidRow(
                column(
                    6,
                    h3c("Return Reasons"),
                    plotOutput("return_reasons")
                ),
                column(
                    6,
                    h3c("Top Colors"),
                    plotOutput("top_colors")
                )
            ),
            
            fluidRow(
                h3c("Monthly Customization Rates"),
                plotOutput("cust_rates")
            ),
            
            fluidRow(
                h3c("Size Distribution"),
                plotOutput("size_dist")
            ),
            
            fluidRow(
                downloadButton("download_all")
            )
        ),
        
        tabPanel(
            "Returns",
            
            h1("Returns Bulk Export"),
            p("The download will take up to 1 minute"),
            downloadButton("download_returns"),
            
            hr(),
            
            fluidRow(plotOutput("factory_returns"))
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