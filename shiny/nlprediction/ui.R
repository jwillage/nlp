library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Next Word Prediction"),
  sidebarPanel(
    h3("Input text"),
    textInput("fragment", "Input text:"),
    submitButton("Submit")
  ),
  mainPanel(
    h3("Prediction"),
    verbatimTextOutput("pred")
  )
))