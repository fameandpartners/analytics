shinyUI(fluidPage(
    titlePanel(NULL, "Ecommerce Performance"),
    
    fluidRow(
        column(
            4,
            dateRangeInput(
                "order_dates",
                label = "Order Date",
                start = as.Date("2016-01-01"),
                end = as.Date("2017-01-01")
            )
        )
    ),

    fluidRow(
        column(1),
        column(
            10, 
            h1("Top Styles", align = "center"),
            br(),
            dataTableOutput("style_ranking")
        ),
        column(1)
    )
)
)