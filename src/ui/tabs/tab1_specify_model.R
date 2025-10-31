tab1_ui <- tabPanel(
  "1. Specify Model", value = "tab1",
  br(),
  column(
    8,
    wellPanel(
      # Create text box for users to enter analysis model
      textAreaInput(
        inputId = "text1",
        label = "Enter your analysis model below:",

        # Pre-fill with sample syntax
        value = "
X =~ x1 + x2 + x3
Y =~ y1 + y2 + y3

Y ~ X",
        # Allow users to resize text box
        resize = "both", rows = 12, cols = 80),

      # Add radio button for scale setting
      radioButtons(
        inputId = "stdlv.radio",
        label = "How would you like to set the scale of your latent factors?",
        choices = list("Fix variances of latent variables" = 1,
                        "Fix first factor loadings" = 2),
        selected = 1),

      # Add button for model setting
      actionButton(
        inputId = "clicks1",
        label = "Set Model")
    )
  )
)
