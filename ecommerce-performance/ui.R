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
                    h3c("Size Distribution"),
                    plotOutput("size_dist")
                ),
                column(
                    6,
                    h3c("Top Colors"),
                    plotOutput("top_colors")
                )
            ),
            
            fluidRow(
                h3c("Weekly Customization Rates"),
                plotOutput("cust_rates")
            ),
            
            fluidRow(
                downloadButton("download_all")
            )
        ),
        tabPanel(
            "Returns",
            fluidRow(
                column(
                    4,
                    dateRangeInput(
                        "ship_dates_r",
                        label = "Ship Date",
                        start = today() - 395,
                        end = today() - 30
                    ),
                    selectInput(
                        "collections_r",
                        label = "Collection",
                        choices = sort(unique(products_sold$collection)),
                        multiple = TRUE
                    ),
                    selectInput(
                        "style_r",
                        label = "Style",
                        choices = sort(unique(products_sold$style_name)),
                        multiple = TRUE
                    ),
                    selectInput(
                        "height_r",
                        label = "Height",
                        choices = c("Petite","Standard","Tall"),
                        multiple = TRUE
                    ),
                    sliderInput(
                        "us_size_r",
                        label = "Size (US)",
                        min = 0,
                        max = 22,
                        value = c(0,22)
                    )
                ),
                column(
                    8,
                    h3c("Monthly Return Rates"),
                    plotOutput("monthly_return_rates")
                )
            ),
            
            fluidRow(
                column(
                    5,
                    h3c("Return Reasons"),
                    plotOutput("return_reasons")
                ),
                column(
                    7,
                    h3c("Primary and Secondary Return Reasons"),
                    dataTableOutput("sec_return_reasons")
                )
            ),
            
            h3c("Monthly Return Rates by Factory"),
            fluidRow(plotOutput("factory_returns", height = "600px")),
            
            hr(),
            
            h1("Returns Bulk Export"),
            p("The download will take up to 1 minute"),
            downloadButton("download_returns")
        ),
        tabPanel(
            "Conversions",
            fluidRow(
                column(
                    4,
                    dateRangeInput("conversion_dates", 
                                   start = today() - 365,
                                   end = today(),
                                   "Date"),
                    selectInput("cohort_select",
                                "Cohort",
                                choices = c("Prom","Bridal","Contemporary"),
                                multiple = TRUE),
                    h4("UTM Parameters:"),
                    selectInput("utm_source",
                                "Source",
                                choices = sort(unique(all_touches$utm_source)),
                                multiple = TRUE),
                    selectInput("utm_medium",
                                "Medium",
                                choices = sort(unique(all_touches$utm_medium)),
                                multiple = TRUE),
                    selectInput("utm_campaign",
                                "Campaign",
                                choices = sort(unique(all_touches$utm_campaign)),
                                multiple = TRUE)
                ),
                column(
                    8,
                    h3c("Monthly Carts, Orders and Conversion Rates"),
                    plotOutput("cart_to_purchase", height = "200px"),
                    plotOutput("carts_orders", height = "300px")
                )
            ),
            fluidRow(
                column(
                    4,
                    h3c("Revenue by UTM Source"),
                    plotOutput("utm_rev")
                ),
                column(
                    8,
                    h3c("Cohort Conversion Funnels"),
                    plotOutput("cohort_conversions")
                )
            ),
            fluidRow(
                column(
                    4,
                    h3c("UTM Source Conversions"),
                    plotOutput("source_conv", height = "800px")
                ),
                column(
                    4,
                    h3c("UTM Medium Conversions"),
                    plotOutput("med_conv", height = "800px")
                ),
                column(
                    4,
                    h3c("UTM Campaign Conversions"),
                    plotOutput("camp_conv", height = "800px")
                )
            )
        )
    ),
    title = titlePanel(NULL, "Ecommerce Performance")
))