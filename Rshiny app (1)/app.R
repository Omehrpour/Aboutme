library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(stringr)
library(ggplot2)
ui <- dashboardPage(
  dashboardHeader(title = "Page"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About Me", tabName = "about", icon = icon("address-card")),
      menuItem("Beer Study", tabName = "beer", icon = icon("university"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "about",
              fluidRow(fileInput("myFile", "Choose a file", accept = c('image/png', 'image/jpeg'))),
              
              textInput(inputId= "name",
                        label="Name:",
                        value="",
                        placeholder="Omid Mehrpour"),
              (textInput(inputId= "Place of birth",
                         label="Where I am from?:",
                         value="",
                         placeholder="")),
              (textInput(inputId= "interests",
                         label="What are my intersts?:",
                         value="",
                         placeholder="")),
              (textInput(inputId= "work place",
                         label="Where do I work?:",
                         value="",
                         placeholder="")),
              (textInput(inputId= "study place",
                         label="Where did I go to school?:",
                         value="",
                         placeholder="")),
              selectInput(inputId="sex:",
                          label="Sex:",
                          choices=list(Female = "F",
                                       Male = "M")),
              
              sliderInput(inputId = "year",
                          label="Year Range:",
                          min = 1,
                          max = 50,
                          value = 30,
                          sep=""),
              submitButton(text="Creat my plot! "),
              plotOutput(outputId = "nameplot")
      ),
      
      # Second tab content
      tabItem(tabName = "beer",
              fluidPage(
                tabsetPanel(
                  tabPanel("Activity Four", fluid = TRUE,
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
                  ),
                  tabPanel("ACtivity Five", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(
                               fileInput("file1", "Choose CSV File",
                                         accept = c(
                                           "text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")
                               ),
                               div( switchInput(inputId = "switch2",
                                                onLabel = "scatter",
                                                offLabel = "Histogram"),style=" text-align: center"),
                               selectInput("state", "Select state:",
                                           c("all")),
                               checkboxInput("regression", "Hide/Show Regression line", FALSE),
                              
                               ),
                             mainPanel(
                                 fluidRow(
                                 
                                 column(6, plotOutput("histscat1")),
                                 column(6, plotOutput("histscat2"))  
                                 
                               ),
                               fluidRow(
                                 
                                 column(6, plotOutput("scatterplot")),
                                 column(6, plotOutput("barplot"))  
                                 
                               ),
                               
                             )
                           )
                  )
                )
              )
            
      )
    )
  )
)

server <- function(input, output,session) {
  
  
  #for activity 4
  
  ########################################
beer=read.csv("Beers.csv")

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- beer$IBU
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',xlab="IBU",main="Histogram of IBU")
  })

  ############################
  
  
  
  
  #for activity5
  
  ###################################
  infile  <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data = read.csv(inFile$datapath)
    
    updateSelectInput(session, "state",
                      label = "Select state:",
                      choices = c("all",unique( data$Style)) ,
                      selected = head( c("all",unique( data$Style)), 1)
    )
    return (data)
  })

   
  reactive({
    infile()
    
    print("asdasda")
  })
  
   output$histscat1 <- renderPlot({
     
     req(infile())
     ggplot(infile(), aes(x=IBU)) + 
       geom_histogram(color="black", fill="white")
     
    })
   output$histscat2 <- renderPlot({
     req(infile())
     ggplot(infile(), aes(x=ABV)) + 
       geom_histogram(color="black", fill="white")
   })
   
   
   generatePlots <- function(data){
     
     if (input$switch2 == FALSE){
       
       output$histscat1 <- renderPlot({
         
     
         ggplot(data, aes(x=IBU)) + 
           geom_histogram(color="black", fill="white")
         
       })
       output$histscat2 <- renderPlot({
         
         ggplot(data, aes(x=ABV)) + 
           geom_histogram(color="black", fill="white")
       })
     }else{
       
       output$histscat1 <- renderPlot({
         
         req(infile())
         ggplot(data, aes(x=IBU)) + 
           geom_boxplot(fill="slateblue", alpha=0.2)
         
       })
       output$histscat2 <- renderPlot({
        
         ggplot(data, aes(x=ABV)) + 
           geom_boxplot(fill="slateblue", alpha=0.2)
       })
     }
   }
   observeEvent(input$switch2,{
     #histogram
     req(infile())
     data =infile()
     if(input$state == "all"){
       generatePlots(data)
     }else{
       data = data[data$Style == input$state,]
       generatePlots(data)
     }
     
   })
   
   observeEvent(input$state,{
     req(infile())
     
     data =infile()
     if(input$state == "all"){
       generatePlots(data)
       generate_scatter(data,input$regression)
     }else{
       data = data[data$Style == input$state,]
       generatePlots(data)
       generate_scatter(data,input$regression)
     }
   
     
     
     
   })
   
   output$barplot <- renderPlot({
     
     req(infile())
     data1 <- infile()
     data1 = data1[order(data1$ABV,decreasing = TRUE),]
     
     # data1 = na.omit(data1, c("Name"))
     data1$Name <- lapply(data1$Name, function(x){
       ifelse(str_count(x, " ") == 0,x,toString( strsplit(x, " ")[[1]][1:2]))
       
     })
     data1$Name = unlist(data1$Name)
     data1 = data1[!duplicated(data1$Name),]
     
     df <- data.frame(Name=(data1$Name[1:5]),
                      ABV=data1$ABV[1:5])
    
     
     ggplot(data=df, aes(x=Name, y=ABV)) +
       geom_bar(stat="identity", fill="steelblue")+
       geom_text(aes(label=ABV), vjust=1.6, color="white", size=3.5)+
       theme_minimal()+  theme(axis.text.x = element_text(angle = 45))+
       labs(title="Top 5 Names by ABV")
     
   })
   
   
   output$scatterplot <- renderPlot({
     req(infile())
     data <- infile()
     
     generate_scatter(data,FALSE)
   })
  
  generate_scatter <- function(data1,isRegression){
    print("scatter")
    print(isRegression)
    output$scatterplot = renderPlot({
      if(isRegression == FALSE){
        
        ggplot(data1, aes(x=ABV, y=IBU)) + 
          geom_point(shape=18, color="blue")
      }else{
        ggplot(data1, aes(x=ABV, y=IBU)) + 
          geom_point(shape=18, color="blue")+
          geom_smooth(method=lm, se=FALSE, linetype="dashed",
                      color="darkred")
      }
    })
   
  }
   observeEvent(input$regression,{
     
     req(infile())
     data <- infile()
     generate_scatter(data,input$regression)
     
   })
 ######################################
}

shinyApp(ui, server)


