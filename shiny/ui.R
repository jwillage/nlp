library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Next Word Prediction"),
  sidebarPanel(
    h3("Input text")
  ),
  mainPanel(
    h3("Prediction")
  )
))