#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(
    fluidPage(
    
    # Application title
    titlePanel("BeerData"),
    sidebarLayout(
      # Sidebar with a slider input for number of bins 
   
        sidebarPanel(
          sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30)
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input,output) {
  beer=read_sav("/Users/owner/Desktop/homework/unit8/beer4.sav")
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- beer$IBU
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',xlab="IBU",main="Histogram of IBU")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)