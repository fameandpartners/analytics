library(shiny)

shinyUI(fluidPage(
    titlePanel(NULL, "Repeat Customers"),
    
    fluidRow(
        column(
            4,
            dateRangeInput(
                "ship_dates", 
                label = "Ship Date",
                start = as.Date("2016-01-01"),
                end = as.Date("2017-01-01")
            )
        ),
        column(
            4,
            selectInput(
                "short_long",
                label = "Dress Length",
                choices = c("Long", "Short", "Not Specified"),
                selected = c("Long", "Short", "Not Specified"),
                multiple = TRUE
            )
        ),
        column(
            4,
            selectInput(
                "cust_eng",
                label = "Purchase Frequency",
                choices = unique(sales$customer_engagement_level),
                selected = unique(sales$customer_engagement_level),
                multiple = TRUE
            )
        )
    ),
    
    fluidRow(
        column(2, checkboxInput("skirt", label = "Skirts")),
        column(2, checkboxInput("evening", label = "Evening")),
        column(2, checkboxInput("cocktail", label = "Cocktail")),
        column(2, checkboxInput("bridal", label = "Bridal")),
        column(2, checkboxInput("daytime", label = "Daytime")),
        column(2, checkboxInput("jumpsuit", label = "Jumpsuit"))
    ),
    
    fluidRow(
        column(
            5,
            h4("Top Cities by Revenue", align = "center"),
            plotOutput("top_cities"),
            downloadButton("top_cities_down")
        ),
        column(
            7,
            h4("Monthly Revenue", align = "center"),
            plotOutput("monthly_new_repeat"),
            downloadButton("monthly_new_repeat_down")
        )
    ),
    
    fluidRow(
        column(
            5,
            h4("Top Styles by Revenue", align = "center"),
            plotOutput("top_styles"),
            downloadButton("top_styles_down")
        ),
        column(
            7,
            h4("Monthly Repeat Rates", align = "center"),
            plotOutput("repeat_rate"),
            downloadButton("repeat_rate_down")
        )
    ),
    
    br(),
    
    fluidRow(
        column(
            5,
            h4("Purchases by Return Reason", align = "center"),
            plotOutput("which_order_return_reasons"),
            downloadButton("which_order_return_reasons_down")
        ),
        column(
            7, 
            h4("Monthly Return Reasons", align = "center"),
            plotOutput("monthly_return_reasons"),
            downloadButton("monthly_return_reasons_down")
        )
    ),
    
    fluidRow(
        column(
            5, 
            h4("Top Return Reasons", align = "center"),
            plotOutput("return_reasons"),
            downloadButton("return_reasons_down")
        ),
        column(
            7,
            h4("Monthly Return Rates", align = "center"),
            plotOutput("return_rate"),
            downloadButton("return_rate_down")
        )
    ),
    
    fluidRow(
        column(
            5,
            h4("Avg Days between Sales", align = "center"),
            plotOutput("avg_days_to_purchase"),
            downloadButton("avg_days_to_purchase_down")
        ),
        column(
            7,
            h4("Monthly Sales by Color", align = "center"), 
            plotOutput("color_year"),
            downloadButton("color_year_down")
        )),
    
    fluidRow(
        h4("Avg Repeat Customer LTV by Top Cities", align = "center"), 
        plotOutput("avg_ltv"), 
        downloadButton("avg_ltv_down")
    ),
    
    br(),
    br(),
    br(),
    
    fluidRow(
        h2("Customer Purchase History", align = "center"),
        dataTableOutput("download_table"),
        downloadButton('downloadData', 'Download')
    )
    )
)
