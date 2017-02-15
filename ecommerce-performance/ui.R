library(shiny)

shinyUI(fluidPage(
    
    
    # Application title
    titlePanel(NULL, "Ecommerce Performance"),
    
    
    sidebarLayout(
        sidebarPanel(
            width = 2,
            sliderInput(
                "bins",
                "Number of bins:",
                min = 1,
                max = 50,
                value = 30
            )
            
        ),
        
        mainPanel(
            width = 10,
            fluidRow(
                column(
                    6,
                    dataTableOutput("distPlot")
                ),
                column(
                    6,
                    plotOutput("distPlot2"),
                    plotOutput("distPlot3")
                )
            ),
            fluidRow(plotOutput("distPlot4")),
            fluidRow(plotOutput("distPlot5"))
        )
    )
    
))
