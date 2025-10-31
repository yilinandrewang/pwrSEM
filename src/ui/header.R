library(shiny);

header <- fluidRow(
  column(width = 12,
    HTML(paste(tags$strong("pwrSEM", style = "font-size:40px;"),
    "v0.2.0")),
    h4("Power Analysis for Parameter Estimation in Structural Equation Modeling"),
    HTML(paste(
      "If you find this app useful, please cite: Wang, Y. A., & ",
      "Rhemtulla, M. (", tags$a(
        href = "https://psyarxiv.com/pj67b",
        target = "_blank",
            "in press"),
      "). Power analysis for parameter estimation in structural ",
      "equation modeling: A discussion and tutorial. ",
      tags$i("Advances in Methods and Practices in Psychological Science."),
      sep = "")),
    style = "padding-bottom: 10px;"
  )
)
