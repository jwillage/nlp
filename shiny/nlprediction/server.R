library(shiny)
source("predict.R")
shinyServer(
  function(input, output){
    fragment <- renderPrint({input$fragment})
    frag <- renderText({input$fragment})
    #strip punctuation from input and covert to lower
    output$pred <- renderPrint(paste({input$fragment}, 
                                     stupidBackoff(cleanInput(input$fragment))))
  }
)