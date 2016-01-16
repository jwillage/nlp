library(shiny)
library(shinyjs)
source("predict.R")

shinyServer(
  function(input, output){
    toggle("inputBox")
    toggle("main")
    pred <- reactive({stupidBackoff(cleanInput(input$fragment), hash = T, top = input$k, 
                                           bi.model = bi.model, tri.model = tri.model, 
                                           quad.model = quad.model, quint.model = quint.model)  })
     
  output$topPred <- renderPrint(cat(pred()[1, "gram"]))

  output$otherPred <- renderUI({
    ret <- list()
    if(input$k >1 )
      for (i in 2:input$k)
        ifelse(!is.na(pred()[i, "gram"]),
          ret[[i-1]]<-h4(pred()[i, "gram"], align="center", class = "text-muted"),
          ret[[i-1]]<-h4(""))
    ret
  })
    #output$pred <- renderPrint(paste({input$fragment}, 
     #                                simpleInterpolation(cleanInput(input$fragment))[1, "gram"]))
  }
)