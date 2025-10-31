library(shiny)

sidebar <- sidebarPanel(
  h4("How to Use This App"),

  # Step 1
  tags$div(
    HTML(paste(
      tags$b('Step 1. Specify Model'),
      '. Enter your analysis model using lavaan syntax. Examples of ',
      'formula types that define a structural equation model include ',
      '(more information',
      tags$a(href = "http://lavaan.ugent.be/tutorial/syntax1.html",
              " here"), "):",
      sep = "")
    )
  ),

  tags$ul(
    tags$li(tags$code("=~"), '"is measured by"'),
    tags$li(tags$code("~"), '"is regressed on"'),
    tags$li(tags$code("~~"), '"is correlated with"')
  ),
  p('Click "Set Model" to set the analysis model and continue to Step 2.'
  ),

  # Step 2
  tags$div(
    HTML(paste(
      tags$b('Step 2. Visualize'),
      '. Ensure that the visualized model looks right, then click ',
      '"Proceed" to continue to Step 3. ', sep = "")),
    style = "padding-bottom: 10px;"
  ),

  # Step 3
  tags$div(
    HTML(paste(
      tags$b('Step 3. Set Parameter Values'),
      '. Fill in the "Value" column with the ',
      'population value for each parameter, then check the boxes in the ',
      '"Effect" column for the parameters you would like to detect. ',
      'Click "Confirm Parameter Values" to continue to Step 4.',
      sep = "")),
    style = "padding-bottom: 10px;"
  ),

  # Step 4
  tags$div(
    HTML(paste(
      tags$b('Step 4. Estimate Power'),
      '. Set your sample size and number of simulations, then click ',
      '"Estimate Power via Simulations" to run your power analysis.',
      sep = "")
    )
  ),
  downloadButton("report", "Download PDF Report")
)
