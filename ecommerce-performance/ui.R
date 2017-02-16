shinyUI(fluidPage(
    titlePanel(NULL, "Ecommerce Performance"),
    
    h1("Ecommerce Performance", align = "center"),
    
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
                selected = unique(products_sold$collection),
                multiple = TRUE
            )
        ),
        column(
            8,
            div(dataTableOutput("style_ranking"))
        )
    ),
    
    fluidRow(
        plotOutput("weekly_sales")
    )
)
)