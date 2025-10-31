tab2_ui <- tabPanel(
  "2. Visualize", value = "tab2",

  # Display model diagram
  plotOutput("plot"),
  helpText('Dotted edges represent fixed parameters; solid edges',
            'represent free parameters.'),

  # Display visualization options
  fluidRow(
    column(4,
      radioButtons(inputId = "structural",
                  label = "Show measurement model?",
                  choices = list("Yes" = 1,
                                  "No" = 2),
                  selected = 1, inline = T)),
    column(3,
      numericInput(inputId = "sizeMan",
                  label = "Size of manifest nodes",
                  value = 5, min = 1, step = 1,
                  max = 15)),
    column(3,
      numericInput(inputId = "sizeLat",
                  label = "Size of latent nodes",
                  value = 8, min = 1, step = 1,
                  max = 15)),
    column(2,
      numericInput(inputId = "rotation",
                  label = "Rotation",
                  value = 2, min = 1, step = 1,
                  max = 4))
  ),

  # Add navigation buttons
  actionButton(inputId = "tab2to1", label = "Back to Step 1"),
  actionButton(inputId = "tab2to3", label = "Proceed"),
)
