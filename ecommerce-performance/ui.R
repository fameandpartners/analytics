
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
    
    
    # Application title
    titlePanel(NULL, "Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins
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
        
        # Show a plot of the generated distribution
        mainPanel(
            width = 10,
            fluidRow(
                column(
                    6,
                    plotOutput("distPlot", height = 800)
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
