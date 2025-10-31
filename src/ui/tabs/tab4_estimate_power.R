tab4_ui <- tabPanel(
  "4. Estimate Power", value = "tab4",
    column(12,
      wellPanel(
        # Simulation setup
        fluidRow(
          column(4,
                numericInput(inputId = "sampleN",
                              label = "Set your sample size",
                              value = 200, min = 1, step = 1)),
          column(4,
                numericInput(inputId = "p_alpha",
                              label = "Set your alpha level",
                              value = .05, min = .001, max = 1)),
          column(4,
                numericInput(inputId = "seed",
                              label = "Set seed for simulations",
                              value = 42))),
        sliderInput(inputId = "ksim",
                    label = "Set number of simulations",
                    value = 100, min = 100, step = 100,
                    max = 10000),
        helpText('We recommend starting with a low number of',
                'simulations (e.g., 100) to get a rough',
                'estimate of power before confirming it',
                'with a higher number of simulations',
                '(e.g., 1000). The larger the number,',
                'the longer simulations will take.'),

        # Add button to initiate simulations
        actionButton(inputId = "sim",
                    label = "Estimate Power via Simulations"),
      )
    ),
    br(),
    # Display warning on model detection
    textOutput("step4_model_warning"),

    # Display warning on parameter table dimension
    textOutput("step4_dim_warning"),

    # Display warning on parameter selection
    textOutput("step4_para_warning"),

    # Display warning on parameter values
    textOutput("step4_para_all"),

    # Display results of simulations
    div(tableOutput("power"), style = "font-size:120%"),

    textOutput("powertable_note"),

    column(10,
      br(), br(),

      # Display histograms
      uiOutput("histograms"),

      plotOutput("histop"),
      textOutput("histop_note"),
      br(), br(),
      plotOutput("histoparam"),
      textOutput("histoparam_note")
    )

)
