library(shiny)
library(shinyjs)

shinyUI(
  #TODO add tab
  #give credit for https://bootswatch.com/spacelab/ and ggtheme
  bootstrapPage(
    useShinyjs(),
    theme = "spacelab.css",
    includeCSS("www/spacelab.css"),

    h1("Next Word Prediction", align = 'center', class='text-primary'),
    h5(id = "inputBox", "Please wait, loading ...", class='text-warning', align='center'),
    hidden(div(id = "main", align = "center",
      HTML("<br><br><div align = 'center'>
          <label class='control-label' for='fragment'>Input phrase </label>
          <input type='text' name = 'fragment' style='width:70%'>  &nbsp;&nbsp;&nbsp;<br>
          <label class='control-label' for='k'>Max results</label>
          <select id = 'k'>        
            <option value='1' selected='selected'>1</option>
            <option value='2'>2</option>
            <option value='3'>3</option>
            <option value='4'>4</option>
            <option value='5'>5</option>
          </select>&nbsp;&nbsp;&nbsp;
          <label class='control-label' for='method'>Method</label>
          <select id = 'method'>
            <option value='b' selected='selected'>Backoff</option>
            <option value='i'>Interpolation</option>
          </select>&nbsp;&nbsp;&nbsp;
          <input type = 'Submit'>
        </div>")
    ,h3(textOutput("topPred"), align = "center", class='text-info')
    ,uiOutput("otherPred")
    ,plotOutput("o", width = "50%", height = "150px")
    ))
)
)