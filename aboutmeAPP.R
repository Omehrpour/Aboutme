library(shiny)
library(tidyverse)
ui=fluidPage(fluidRow(fileInput("myFile", "Choose a file", accept = c('image/png', 'image/jpeg'))),
             
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
)
server=function(input,output){
}

shinyApp(ui=ui,server=server)
