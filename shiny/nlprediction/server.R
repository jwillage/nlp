library(shiny)
shinyServer(
  function(input, output){
    model.tri <- readRDS("model_tri_sample_EN.RDS")
    fragment <- renderPrint({input$fragment})
    frag <- renderText({input$fragment})
    output$pred <- renderPrint(paste({input$fragment}, 
                                     model.tri[[1]][model.tri[[1]]$idx == input$fragment, "x"]))
  }
)