tab3_ui <- tabPanel(
  "3. Set Parameter Values", value = "tab3",
  br(),
  # Display instructions
  helpText('Your model parameter table is shown below.',
            'You can use it like an Excel spreadsheet.',
            '(e.g., double-click on a "Value" cell to edit).',
            br(),
            'Not sure what values to set the parameters at?',
            tags$ul(
              tags$li('If you need help with setting factor',
                      'loadings or latent regression coefficients,',
                      'click the "Help" tab for suggestions.'),
              tags$li('If you need help with setting residual',
                      'variances, enter factor loadings and regression',
                      'coefficients in the standardized metric, ',
                      tags$i('leave blank all other parameters,'),
                      'then click "Set Residual Variances for Me"',
                      'below. (Note that covariance parameters, if any,',
                      'still need to be set by users afterwards.)')
            )),

  # Display interactive parameter table
  rHandsontableOutput("AnalysisMod"),

  # Add buttons for various functions (see server() below for details)
  actionButton(inputId = "tab3to2",
                label = "Back to Step 2 (Values are Saved)"),
  actionButton(inputId = "autoRes",
                label = "Set Residual Variances for Me"),
  actionButton(inputId = "tab3to4",
                label = "Confirm Parameter Values"),

  # Display warning on model detection
  textOutput("step3_model_warning"),

  # Display warning on parameter table dimension
  textOutput("step3_dim_warning"),

  # Display warning on positive definite matrix
  textOutput("resid_warning"),

  # Display warning on standardized metric
  textOutput("resid_std"),

  # Display success on setting residuals
  textOutput("resid_success"),

  # Display warning on parameter selection
  textOutput("step3_para_warning"),

  # Display warning on parameter values
  textOutput("step3_para_all"),

  # Display success on parameter selection
  textOutput("step3_para_success"),

)
