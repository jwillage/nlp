library(shiny)
library(hashr)
source("predict.R")

shinyServer(
  function(input, output){
    fragment <- renderPrint({input$fragment})
    frag <- renderText({input$fragment})
    output$pred <- renderPrint(paste({input$fragment}, 
                                     stupidBackoff(cleanInput(input$fragment))[1]))
  }
)