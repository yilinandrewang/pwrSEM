tab6_ui <- tabPanel(
  "Resources", value = "tab6",
  br(),

  # App Tutorials
  h4("App Tutorials"),
  tags$div(
    HTML(paste(
      tags$a(href = "https://psyarxiv.com/pj67b",
              "Wang and Rhemtulla (in press)"),
      ' includes a tutorial of pwrSEM using a simple mediation model.',
      sep = "")),
    style = "padding-bottom: 10px;"
  ),

  tags$div(
    HTML(paste(
      'The ',
      tags$a(href = "https://osf.io/6m7xz/",
              "supplemental material"),
      ' includes an additional tutorial using a more complex model ',
      '(based on Zimmerman, Bandura, & Martinez-Pons, 1992).',
      sep = "")),
    style = "padding-bottom: 10px;"
  ),

  # Power to detect model misspecification
  h4("Power to Detect Model Misspecification"),
  p("Although not the focus of this Shiny app, we recognize that",
    "researchers may also want to conduct power analysis to detect",
    "model misspecification. As argued in our paper and elsewhere",
    "both types of power are important to sample size planning in SEM.",
    "Power analysis to detect model misspecification can be conducted",
    "with a variety of fit indices as effect sizes. The most popular",
    "approach by MacCallum, Browne, and Sugawara (1996) uses RMSEA and",
    "can be implemented via the calculator below."),

  # Set up MBS calculator
  wellPanel(

    # Inputs
    fluidRow(
      column(6,
              sliderInput("RMSEAnull", label = "Null RMSEA",
                          min = 0, max = 0.20, step = .01,
                          value = 0.05)),
      column(6,
              sliderInput("RMSEAalt", label = "Alternative RMSEA",
                          min = 0, max = 0.20, step = .01,
                          value = 0.10))
    ),
    fluidRow(
      column(3,
              numericInput("RMSEAalpha", label = "Alpha level",
                          value = .05, min = .001, max = .999,
                          step = .001)),
      column(3,
              numericInput(inputId = "df", label = "Degrees of freedom",
                          value = 1, min = 1, step = 1)),
      column(3,
              numericInput(inputId = "RMSEAn", label = "Sample size",
                          value = 200, min = 1, step = 1)),

      # Output
      column(3,
              verbatimTextOutput("RMSEApower"))
    )
  ),

  # Note on the Satorra-Saris approach
  tags$div(
    HTML(paste(
      "Alternatively, power to detect model misspecification ",
      "can be calculated with Satorra and Saris' (1985) approach, ",
      "which uses Chi-square likelihood-ratio. We direct interested ",
      "researchers to ",
      tags$a(href = "https://webpower.psychstat.org/models/sem02/",
              "its implementation"),
      " in WebPower (Zhang & Yuan, 2018).",
      sep = "")
      )
  ),
  br(),

  # Learning resources
  h4("Resources for Learning SEM"),
  tags$ul(
    tags$li(HTML(paste('Kline, R. B. (2016).', tags$i(
      'Principles and practice of structural equation modeling'),
      "(4th ed.). Guilford Press, New York, NY.", sep = " ")
      )
    ),
    tags$li(tags$a(
      href = "http://lavaan.ugent.be/resources/teaching.html",
      "Teaching materials for lavaan")
    ),
    tags$li(tags$a(
      href = paste("https://curranbauer.org/wp-content/uploads/",
                    "2019/04/SEM-R-notes-2019-3.pdf", sep = ""),
      paste("Structural Equation Modeling R Demonstration Notes, by",
            "Daniel J. Bauer and Patrick J. Curran", sep = " "))
    )
  )

)
