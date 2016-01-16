library(shiny)
shinyUI(
  bootstrapPage(
    theme = "spacelab.css",
    includeCSS("www/spacelab.css"),
    #todo change class
    h1("Next Word Prediction", align = 'center', class='text-primary'),
    HTML("<br><br><div align = 'center'>
          <label class='control-label' for='fragment'>Input phrase </label>
          <input type='text' name = 'fragment'>  &nbsp;&nbsp;&nbsp;
          <label class='control-label' for='k'>Max results</label>
          <select id = 'k'>        
            <option value='1' selected='selected'>1</option>
            <option value='2'>2</option>
            <option value='3'>3</option>
            <option value='4'>4</option>
            <option value='5'>5</option>
          </select>&nbsp;&nbsp;&nbsp;
          <input type = 'Submit'>
        </div>")
    ,h3(textOutput("topPred"), align = "center", class='text-info')
    ,uiOutput("otherPred")
)
)