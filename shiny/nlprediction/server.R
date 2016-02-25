library(shiny)
library(shinyjs)
library(grid)
source("predict.R")

shinyServer(
  function(input, output, session){
    toggle("inputBox")
    toggle("main")
    pred <- reactive({
      if(input$method == 'b')
        return(stupidBackoffFixed(cleanInput(input$fragment), hash = T, top = input$k, 
                                           bi.model = bi.model.hash, tri.model = tri.model.hash, 
                                           quad.model = quad.model.hash, quint.model = quint.model.hash))  
      p <- simpleInterpolation(cleanInput(input$fragment), hash = T, top = input$k, bi.model = bi.model,
                          tri.model = tri.model, quad.model = quad.model, quint.model = quint.model)
      p <- p[complete.cases(p),]
      })
     
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

    output$o <- renderPlot({
      if (pred()[1, "gram"] != ""){
      plot.new()
      gl <- grid.layout(1,1)
      vp.1 <- viewport(layout.pos.col = 1, layout.pos.row = 1, height=unit(.4, "npc"))
      vp.2 <- viewport(layout.pos.col = 2, layout.pos.row = 1)
      vp.3 <- viewport(layout.pos.col = 1, layout.pos.row = 2)
      vp.4 <- viewport(layout.pos.col = 2, layout.pos.row = 2)
      pushViewport(viewport(layout=gl))
      
      pushViewport(vp.1)
      g <- ggplot(data = pred(), aes(y=runif(nrow(pred()), 0, .5), x = scores, fill=n)) +
        theme_economist() + 
        theme(axis.text.y=element_blank(), axis.title.y=element_blank(), 
              panel.grid.major.y = element_blank(), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) + 
        geom_label(aes(label=gram), #size = pred()$n*2,
                   size = (sqrt(pred()$n)*3) + 2,
                   alpha=.25) + 
        labs(aes(size = 5)) +
        coord_cartesian(ylim = c(-.3,.8)) +
        guides(fill = F)
      print(g)}
  })
  }
)

  