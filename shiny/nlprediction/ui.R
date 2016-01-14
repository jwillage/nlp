library(shiny)
shinyUI(
  fluidPage(
    headerPanel("Next Word Prediction"),
    fluidRow(align="center",
      textInput("fragment", "Input phrase below"),
      numericInput("k", "Number of results", 1, min = 1, max = 5, step = 1),
      submitButton("Submit")
    ),
    verbatimTextOutput("pred")
  )
)