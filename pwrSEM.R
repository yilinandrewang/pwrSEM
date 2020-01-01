#############################      pwrSEM      #############################
# Power Analysis for Parameter Estimation in Structural Equation Modeling ##

# Written by Y. Andre Wang

# Load packages
if(!require(shiny)){install.packages('shiny')}
if(!require(lavaan)){install.packages('lavaan')}
if(!require(semPlot)){install.packages('semPlot')}
if(!require(rhandsontable)){install.packages('rhandsontable')}
if(!require(semTools)){install.packages('semTools')}
if(!require(tidyr)){install.packages('tidyr')}
library(shiny); library(lavaan); library(semPlot); library(rhandsontable)
library(semTools); library(tidyr)


# Define UI ---------------------------------------------------------------

ui <- fluidPage(
  
  
# *- Set button and text colors -------------------------------------------

  tags$head(
    tags$style(HTML('#clicks1{background-color:#428BCA; color: white}',
                    '#tab2to3{background-color:#428BCA; color: white}',
                    '#tab3to4{background-color:#428BCA; color: white}',
                    '#autoRes{background-color:#4CAF50; color: white}',
                    '#sim{background-color:#428BCA; color: white}',
                    '#resid_warning{color: red}',
                    '#resid_std{color: red}',
                    '#resid_success{color: green}',
                    '#step3_para_warning{color: red}',
                    '#step3_para_all{color: red}',
                    '#step3_dim_warning{color: red}',
                    '#step3_para_success{color: blue}',
                    '#step3_model_warning{color: red}',
                    '#step4_para_warning{color: red}',
                    '#step4_para_all{color: red}',
                    '#step4_model_warning{color: red}',
                    '#step4_dim_warning{color: red}'))
  ),
  

# *- Set app header -------------------------------------------------------

  fluidRow(
    column(width = 12,
           h1("pwrSEM"),
           h4("Power Analysis for Parameter Estimation in Structural Equation Modeling"),
           p("If you find this app useful, please cite:",
             "Wang, Y. A., & Rhemtulla, M. (2020). Power analysis for ",
             "parameter estimation in structural equation modeling: ", 
             "A discussion and tutorial.")
    )
  ),
  

# *- Set sidebar with "how to" guide for the app --------------------------

  sidebarLayout(
    sidebarPanel(
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
      )
    )
    ,
    

# *- Set main interface ---------------------------------------------------

    mainPanel(
      tabsetPanel(
        id = "tabby",
        

# *--- Step 1 -------------------------------------------------------------

        tabPanel(
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
        ),


# *--- Step 2 -------------------------------------------------------------

        tabPanel(
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
          actionButton(inputId = "tab2to1",
                       label = "Back to Step 1"),
          actionButton(inputId = "tab2to3",
                       label = "Proceed")
        ),


# *--- Step 3 -------------------------------------------------------------

        tabPanel(
          "3. Set Parameter Values", value = "tab3",
          
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
          textOutput("step3_para_success")
          
        ),


# *--- Step 4 -------------------------------------------------------------

        tabPanel(
          "4. Estimate Power", value = "tab4",
          br(),
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
                                label = "Estimate Power via Simulations")
                 )
          ),
          
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
        ),


# *--- Help ---------------------------------------------------------------

        tabPanel(
          "Help", value = "tab5",
          br(),
          
          # Factor loading
          h4("What factor loading strength should I specify?"),
          p("If you have a measure with a known reliability estimate",
            "(e.g., Cronbach's alpha), you can estimate the average factor",
            "loading strength of individual items in that measure using the",
            "Spearman-Brown prophecy formula with the calculator below."),
          
          # Set up Spearman-Brown calculator
          wellPanel(
            
            # Inputs
            fluidRow(
              column(9,
                     sliderInput(
                       "alpha", 
                       label = paste("Reliability estimate of measure",
                                     "(e.g., Cronbach's alpha)"),
                       min = 0, max = .99, value = .80)),
              column(3,
                     numericInput("nitem", 
                                  label = "Number of items", 
                                  min = 1, value = 3))
            ),
            
            # Output
            verbatimTextOutput("lambda.est")
          ),
          
          # Structural effect size
          h4("What structural effect sizes should I specify?"),
          p("The effect size of a structural parameter (e.g., regression",
            "coefficient between two latent factors) in SEM is often",
            "different from the effect size estimated from regressions using",
            "observed variables, because the structural parameter estimate",
            "could be disattenuated from measurement error. For example, if",
            "prior research found a correlation of .3 between two raw scores,",
            "the effect size of the corresponding true scores is likely larger",
            "(though note this is not necessarily the case if the effect size",
            "from observed variables is estimated in a multivariate path",
            "model; see e.g., Cole & Preacher, 2014). The calculator below",
            "allows you to disattenuate the effect size (in correlation)",
            "between two observed variables using the Spearman's correction."
          ),
          
          # Set up Spearman's correction calculator
          wellPanel(
            
            # Inputs
            sliderInput("raw.corr", 
                        label = "Correlation between observed variables A and B",
                        min = 0, max = .99, value = .30),
            fluidRow(
              column(6,
                     sliderInput("reliability1", 
                                 label = "Reliability of variable A",
                                 min = 0, max = .99, value = .80)),
              column(6,
                     sliderInput("reliability2", 
                                 label = "Reliability of variable B",
                                 min = 0, max = .99, value = .80))),
            
            # Output
            verbatimTextOutput("latent.corr")
          )
        ),


# *--- Resources ----------------------------------------------------------

        tabPanel(
          "Resources", value = "tab6",
          br(),
          
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
      )
    )
  )
)



# Define server logic -----------------------------------------------------

server <- function(input, output, session) {
  
  
# Assign reactive values from boolean inputs ------------------------------
  
  # Radio button in Step 1
  stdlv <- reactive({
    if (input$stdlv.radio == 1) {stdlv <- TRUE} else {stdlv <- FALSE}
  })
  
  # Radio button in Step 2
  structural <- reactive({
    if (input$structural == 1) {structural <- FALSE} else {structural <- TRUE}
  })
  
  
# Store outputs of calculators --------------------------------------------
  
  # Estimated factor loading output
  output$lambda.est <- renderText({
    paste0("Estimated average factor loading per item: ",
           round(sqrt(input$alpha/(input$nitem + (1 - input$nitem)*input$alpha)
           ), 2)
    )
  })
  
  # Estimated structural effect size output
  output$latent.corr <- renderText({
    paste0("Estimated correlation between latent variables A and B: ",
           round(input$raw.corr/(sqrt(input$reliability1*input$reliability2)
           ), 2)
    )
  })
  
  # Power to detect model misspecification output
  output$RMSEApower <- renderText({
    paste0("Power: ", round(
      findRMSEApower(rmsea0 = input$RMSEAnull, 
                     rmseaA = input$RMSEAalt, 
                     df = input$df, n = input$RMSEAn, 
                     alpha = input$RMSEAalpha),
      3)
    )
  })
  
  
# Events reactive to cross-tab navigation buttons  ------------------------
  
  observeEvent(input$tab2to1, {
    updateTabsetPanel(session, "tabby", selected = "tab1")
  })
  
  observeEvent(input$tab2to3, {
    updateTabsetPanel(session, "tabby", selected = "tab3")
  })
  
  observeEvent(input$tab3to2, {
    updateTabsetPanel(session, "tabby", selected = "tab2")
  })
  
  
# Events reactive to "Set Model" in Step 1 --------------------------------
  
  # Assign reactive object
  mg <- eventReactive(input$clicks1, { 
    
    # Simulate data for visualization
    vis_dat <- as.data.frame(simulateData(input$text1, sample.nobs = 1000), 
                             empirical = T)
    
    # Generate fitted model for diagram display
    vis_fit <- sem(model = input$text1, data = vis_dat, std.lv = stdlv())
    
    # Generate parameter table
    am <- parameterTable(vis_fit)[, c(1:4, 8:9, 11)]
    am$effect <- FALSE # By default, no parameter is selected in "Effect"
    
    # Identify row numbers with different parameter types
    
    # regression coefficient
    am_idRG <- which(am$op == "~")
    
    # factor loading
    am_idMR <- which(am$op == "=~")
    # total variance
    am_idTV <- which(am$op == "~~" & am$lhs == am$rhs & 
                       am$lhs %in% lavNames(input$text1, type = "lv.x"))
    # residual variance
    am_idRV <- which(am$op == "~~" & am$lhs == am$rhs &
                       !(am$lhs %in% lavNames(input$text1, type = "lv.x")))
    # covariance
    am_idTC <- which(am$op == "~~" & am$lhs != am$rhs &
                       am$lhs %in% lavNames(input$text1, type = "lv.x"))
    # residual covariance
    am_idRC <- which(am$op == "~~" & am$lhs != am$rhs &
                       !(am$lhs %in% lavNames(input$text1, type = "lv.x")))
    # intercept
    am_idIT <- which(am$op == "~1")
    
    # labelled parameter
    am_idLB <- which(am$op == ":=")
    
    # Add description of each parameter by type
    am$description <- NA
    am$description[am_idRG] <- paste(am$lhs[am_idRG], "is regressed on", 
                                     am$rhs[am_idRG], sep = " ")
    am$description[am_idMR] <- paste(am$lhs[am_idMR], "is measured by", 
                                     am$rhs[am_idMR], sep = " ")
    am$description[am_idTV] <- paste("Total variance of", am$lhs[am_idTV], 
                                     sep = " ")
    am$description[am_idRV] <- paste("Residual variance of", am$lhs[am_idRV], 
                                     sep = " ")
    am$description[am_idTC] <- paste("Variance of", am$lhs[am_idTC], 
                                     "covaries with variance of", 
                                     am$rhs[am_idTC], sep = " ")
    am$description[am_idRC] <- paste("Residual of", am$lhs[am_idRC], 
                                     "covaries with residual of", 
                                     am$rhs[am_idRC], sep = " ")
    am$description[am_idIT] <- paste("Intercept of", am$lhs[am_idIT], sep = " ")
    am$description[am_idLB] <- "Labelled parameter"
    
    # Display parameter type
    am$type <- NA
    am$type[am_idRG] <- "regression coefficient"
    am$type[am_idMR] <- "factor loading"
    am$type[am_idTV] <- "total variance"
    am$type[am_idRV] <- "residual variance"
    am$type[am_idTC] <- "covariance"
    am$type[am_idRC] <- "residual covariance"
    am$type[am_idIT] <- "intercept"
    am$type[am_idLB] <- "labelled parameter"
    
    # Make table more readable
    am <- tidyr::unite(am, "parameter", lhs:rhs, sep = " ")
    am <- am[, c(1, 2, 5, 7, 4, 8, 6, 3)]
    names(am) <- c("Row", "Parameter", "Label", "Description", "Value", "Type", 
                   "Effect", "Free")
    
    # Return parameter table and diagram
    return(list(am, vis_fit))
    
  })
  

  observeEvent(input$clicks1, {  
    
    # Render interactive parameter table
    output$AnalysisMod <- renderRHandsontable({
      
      # Set table dimensions
      rhandsontable(mg()[[1]], rowHeaders = NULL, stretchH = "all", 
                    height = 300) %>%
        
        # Set all columns other than "Value" to read-only
        hot_col(col = c("Row", "Parameter", "Label", "Description", "Type",
                        "Free"), readOnly = T) %>%
        
        # Highlight cell selection
        hot_table(highlightCol = T, highlightRow = T) %>%
        
        # Disable row and column editing
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
    
    
    # Reader diagram
    output$plot <- renderPlot({
      semPaths(mg()[[2]], edge.color = 'black', curvature = 3,
               structural = structural(), sizeMan = input$sizeMan, 
               
               # Include visualization options
               sizeLat = input$sizeLat, rotation = input$rotation)
    })
    
    # Direct users to Step 2
    updateTabsetPanel(session, "tabby", selected = "tab2")
    
    # Clear out warning messages
    output$step3_para_warning <- output$step3_para_all <- 
      output$step3_dim_warning <- output$step3_para_success <- 
      output$step3_model_warning <- output$step4_para_warning <- 
      output$step4_para_all <- output$step4_model_warning <- 
      output$step4_dim_warning <- renderText("")
  })
  

# Events reactive to "Set Residual Variances for Me" in Step 3 ------------
  
  observeEvent(input$autoRes, {
    
    # Test if model is entered
    test_model_enter <- try(parameterTable(mg()[[2]]), silent = T)
    
    if (inherits(test_model_enter, "try-error")) {
      
      output$step3_model_warning <- renderText(
        "No model detected. Did you enter a model in Step 1?")
      
    } else {
      
      # Create one parameter table that we use later to obtain psi matrix
      text1.t <- parameterTable(mg()[[2]])
      text1.t$free <- 0
      
      # Test if dimensions of entered parameter value is correct
      # (e.g., if users copy a longer column from Excel to the app)
      test_model_dim <- try(
        text1.t[, "ustart"] <- hot_to_r(input$AnalysisMod)[, "Value"], silent = T)
      
      if (inherits(test_model_dim, "try-error")) {
        
        output$step3_dim_warning <- renderText(paste(
          "Incorrect dimensions of parameter table. Please regenerate",
          "the parameter table by resetting the model in Step 1.", sep = " "))
        
      } else {
        
        # Receive parameter values that users input
        text1.t[, "ustart"] <- hot_to_r(input$AnalysisMod)[, "Value"]
        text1.t[is.na(text1.t[, "ustart"] == T), "ustart"] <- 1
        text1.t <- text1.t[(text1.t$op) != ":=", ] # Exclude labelled parameter
        
        # Create another parameter table that we use later for other matrices
        PopMod.t <- parameterTable(mg()[[2]])
        PopMod.t[, "ustart"] <- hot_to_r(input$AnalysisMod)[, "Value"]
        PopMod.t[is.na(PopMod.t[, "ustart"] == T), "ustart"] <- 0
        PopMod.t <- PopMod.t[(PopMod.t$op) != ":=", ] # Exclude labelled parameter
        
        # Test model-implied covariance matrix
        test_dat_sT <- try(simulateData(text1.t, empirical = T, standardized = T, 
                                        sample.nobs = 1000), silent = T)
        
        if (inherits(test_dat_sT, "try-error")) {
          
          output$resid_warning <- renderText(paste(
            "Residual variances could not be calculated. Make sure the parameter",
            "values you entered can produce a positive definite model-implied",
            "covariance matrix.", seq = " ")
          )
          output$resid_std <- output$resid_success <- renderText("")
          
        } else {
          
          # Simulate data to obtain model-implied covariance matrix later
          dat_sF <- simulateData(text1.t, empirical = T, standardized = F, 
                                 sample.nobs = 1000)
          
          
          # Test if values are entered in standardized metric
          if (TRUE %in% (abs(hot_to_r(input$AnalysisMod)$Value) > 1)) {
            
            output$resid_std <- renderText(paste(
              "Residual variances could not be calculated. To automatically set",
              "residual variances, make sure the parameters you enter are in",
              "standardized metric.", seq = " ")
            )
            output$resid_warning <- output$resid_success <- renderText("")
            
            # Test if model-implied covariance matrix is positive definite
          } else if (
            inherits(try(sem(input$text1, dat_sF, std.lv = T), silent = T),
                     "try-error")) {
            
            output$resid_warning <- renderText(paste(
              "Residual variances could not be calculated. Make sure the",
              "parameter values you entered can produce a positive definite",
              "model-implied covariance matrix."))
            output$resid_std <- output$resid_success <- renderText("")
            
          } else {
            
            # Extract baseline matrices
            fit_sF <- sem(input$text1, dat_sF, std.lv = T)
            psi <- inspect(fit_sF, "coef")$psi
            beta <- inspect(fit_sF, "coef")$beta
            lambda <- inspect(fit_sF, "coef")$lambda
            I <- diag(dim(beta)[1]) 
            
            # Solve baseline covariance matrix of latent variables (not correct yet)
            sig <- solve(I - beta) %*% psi %*% solve(I - t(beta))
            diag(sig) <- 1
            
            # Solve for correct psi matrix
            psi <- (I - beta) %*% sig %*% (I - t(beta))
            
            psi2 <- diag(diag(psi))
            sig2 <- solve(I - beta) %*% psi2 %*% solve(I - t(beta))
            diag(sig2) <- 1
            psi <- (I - beta) %*% sig2 %*% (I - t(beta))
            
            while(sum(round(psi[lower.tri(psi)], 10)) != 0){
              psi2 <- diag(diag(psi))
              sig2 <- solve(I - beta) %*% psi2 %*% solve(I - t(beta))
              diag(sig2) <- 1
              psi <- (I - beta) %*% sig2 %*% (I - t(beta))
            }
            
            # Save and label residual values
            theta <- diag(dim(lambda)[1]) - diag(diag(lambda %*% sig2 %*% t(lambda)))
            rownames(theta) <- colnames(theta) <- rownames(lambda)
            
            # Assign values from theta (note that it only supplies correct residuals
            # of indicators; the rest will be overwritten by psi matrix next)
            PopMod.t$ustart[which(PopMod.t$lhs == PopMod.t$rhs & PopMod.t$op == "~~" & 
                                    PopMod.t$lhs %in% names(diag(theta)))] <- diag(theta)
            
            # Identify rows with residuals from psi matrix
            res_psi <- which(PopMod.t$lhs == PopMod.t$rhs & PopMod.t$op == "~~" &
                               PopMod.t$lhs %in% names(diag(psi)))
            
            # Assign values from psi back to parameter table
            PopMod.t$ustart[res_psi][order(
              match(PopMod.t$lhs[res_psi], names(diag(psi))))] <- diag(psi)
            
            # Create parameter table with calculated residuals
            mg_sr <- mg()[[1]]
            
            if (dim(mg_sr)[1] == length(PopMod.t$ustart)) {
              
              mg_sr$Value <- PopMod.t$ustart
              
            } else {
              
              mg_sr$Value <- c(
                PopMod.t$ustart, rep(
                  NA, abs(dim(mg_sr)[1] - length(PopMod.t$ustart))
                ))
              
            }
            
            # Render interactive parameter table again with all residuals set
            output$AnalysisMod <- renderRHandsontable({
              rhandsontable(mg_sr, rowHeaders = NULL, stretchH = "all", height = 300) %>%
                hot_col(col = c("Row", "Parameter", "Label", "Description", 
                                "Type", "Free"), readOnly = T) %>%
                hot_table(highlightCol = T, highlightRow = T) %>%
                hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
            })
            
            output$resid_success <-
              renderText("Residual variances are automatically set.")
            output$resid_std <- output$resid_warning <- 
              output$step3_para_all <- renderText("")
            
          }
          
        }
        
      }
      
    }
  }
  )
  
  
# Events reactive to "Confirm Parameter Values" in Step 3 -----------------

  observeEvent(input$tab3to4, {

    test_model_enter <- try(parameterTable(mg()[[2]]), silent = T)
    
    # Test if model is entered
    if (inherits(test_model_enter, "try-error")) {
      
      output$step3_model_warning <- renderText(
        "No model detected. Did you enter a model in Step 1?")
      
    } else {
      
      text1.t <- parameterTable(mg()[[2]])
      text1.t$free <- 0
      test_model_dim <- try(
        text1.t[, "ustart"] <- hot_to_r(input$AnalysisMod)[, "Value"], silent = T)
      
      
      # Test if parameter table has correct dimensions
      if (inherits(test_model_dim, "try-error")) {
        
        output$step3_dim_warning <- renderText(paste(
          "Incorrect dimensions of parameter table. Please regenerate",
          "the parameter table by resetting the model in Step 1.", sep = " "))
        output$step3_model_warning <-
          renderText("")
        
        # Test if all parameters are specified
      } else if (TRUE %in% is.na(hot_to_r(input$AnalysisMod)$Value)) {
        
        output$step3_para_all <- 
          renderText("All parameter values need to be specified.")
        output$step3_model_warning <- output$step3_dim_warning <- 
          output$step3_para_warning <- output$step3_para_success <- 
          output$step4_para_warning <- renderText("")
        
        # Test if at least one target effect is selected
      } else if (!(TRUE %in% hot_to_r(input$AnalysisMod)$Effect)) {
        
        output$step3_para_warning <- 
          renderText("Please select at least one parameter as the target effect.")
        output$step3_model_warning <- output$step3_dim_warning <-
          output$step3_para_success <- output$step3_para_all <-
          output$step4_para_all <- renderText("")
        
      } else {
        
        PopMod.t <- parameterTable(mg()[[2]])
        PopMod.t[, "ustart"] <- hot_to_r(input$AnalysisMod)[, "Value"]

        
        # Test if model-implied covariance matrix is positive definite
        if (inherits(try(simulateData(PopMod.t, sample.nobs = input$sampleN), 
                         silent = T), "try-error")) {
          
          output$step3_model_warning <- renderText(paste(
            "Your model-implied covariance matrix is not positive definite.",
            "Make sure the parameter values you enter can produce a positive",
            "definite model-implied covariance matrix."))
          
          # Confirm parameter values
        } else {
          
          updateTabsetPanel(session, "tabby", selected = "tab4")
          output$step3_para_success <- 
            renderText("Parameter values confirmed.")
          output$step3_model_warning <- output$step3_dim_warning <-
            output$step3_para_warning <- output$step3_para_all <- 
            output$step4_para_warning <- output$step4_para_all <- 
            renderText("")
        }
      }
    }
  }
  )
  
  # Events reactive to "Estimate Power via Simulations" in Step 4 -----------
  
  observeEvent(input$sim, {
    
    test_model_enter <- try(parameterTable(mg()[[2]]), silent = T)
    
    # Test if model is entered
    if (inherits(test_model_enter, "try-error")) {
      output$step4_model_warning <- renderText(
        "No model detected. Did you enter a model in Step 1?")
      
    } else {
      
      text1.t <- parameterTable(mg()[[2]])
      text1.t$free <- 0
      
      test_model_dim <- try(
        text1.t[, "ustart"] <- hot_to_r(input$AnalysisMod)[, "Value"], silent = T)
      
      
      # Test if parameter table has correct dimensions
      if (inherits(test_model_dim, "try-error")) {
        
        output$step4_dim_warning <- renderText(paste(
          "Incorrect dimensions of parameter table. Please regenerate",
          "the parameter table by resetting the model in Step 1.", sep = " "))
        output$step4_model_warning <-
          renderText("")
        
        # Test if all parameters are specified
      } else if (
        TRUE %in% is.na(hot_to_r(input$AnalysisMod)$Value)) {
        
        output$step4_para_all <- 
          renderText("All parameter values need to be specified.")
        output$step4_para_warning <-
          renderText("")
        
        # Test if at least one target effect is selected
      } else if (!(TRUE %in% hot_to_r(input$AnalysisMod)$Effect)) {
        
        output$step4_para_warning <- 
          renderText("Please select at least one parameter as the target effect.")
        output$step4_para_all <- 
          renderText("")
        
      } else {
        
        PopMod.t <- parameterTable(mg()[[2]])
        PopMod.t[, "ustart"] <- hot_to_r(input$AnalysisMod)[, "Value"]
        
        
        # Test if model-implied covariance matrix is positive definite
        if (inherits(try(simulateData(PopMod.t, sample.nobs = input$sampleN),
                         silent = T), "try-error")) {
          
          output$step4_para_warning <- renderText(paste(
            "Simulations could not be run because the model-implied covariance",
            "matrix is not positive definite. Make sure the parameter values",
            "you entered in Step 3 can produce a positive definite model-implied",
            "covariance matrix."))
          
        } else {
          
          output$step4_para_warning <- output$step4_para_all <- renderText("")
          
          # Create dataframe to store results
          results <- NULL
          
          # Set simulation seed based on user input
          set.seed(input$seed)
          
          # Define population model and target parameter based on user input
          PopMod.t <- parameterTable(mg()[[2]])
          PopMod.t[, "ustart"] <- hot_to_r(input$AnalysisMod)[, "Value"]
          target <- which(hot_to_r(input$AnalysisMod)$Effect == TRUE)
          
          # Set progress bar
          withProgress(message = 'Simulating', value = 0, {
            
            # Loop by iteration
            for (i in 1:input$ksim) {
              
              # Simulate and store data based on sample size input
              data <- simulateData(PopMod.t, sample.nobs = input$sampleN)
              data <- as.data.frame(data)
              
              # Fit analysis model to data
              fit <- sem(model = input$text1, data = data, std.lv = stdlv())
              
              # Store parameter row
              results <- rbind(results, parameterEstimates(fit)[target, ])
              
              # Display progress bar
              incProgress(1/input$ksim, detail = paste("sample", i, "of", 
                                                       input$ksim))
            }
            
            # Convergence rate
            conv <- (input$ksim - sum(is.na(results$pvalue)))/input$ksim
            
            # Create placeholder powertable and CI tables
            powertable <- as.data.frame(matrix(NA, nrow = length(target), ncol = 5))
            colnames(powertable) <- c("Parameter", "Value", "Median", "Power", 
                                      "Power (All Cases)")
            
            ci_table <- as.data.frame(matrix(NA, nrow = length(target), ncol = 3))
            colnames(ci_table) <- c("Parameter", "est.ci.lower", "est.ci.upper")
            
            # add parameter column in results for later identification
            results$Parameter <- paste(results$lhs, results$op, 
                                       results$rhs, sep = " ")
            
            lapply(1:length(target), function(i) {
              
              # row names of results for a given parameter
              ii <- seq(from = i, to = dim(results)[1], by = length(target))
              
              # row names of results for a given parameter with non-NA estimates
              ii.est <- which(is.na(results$est) == F)[which(
                is.na(results$est) == F) %in% ii]
              
              # dataframe with non-NA estimates
              results.est <- results[ii.est, ]
              
              # lower and upper bounds of 95% of non-NA parameter estimates
              
              if (round(length(results.est$est) * 0.025) == 0) {
                est.ci.lower <- "inf"
              } else {
                est.ci.lower <- round(sort(results.est$est)[
                  length(results.est$est) * 0.025], 2)
              }
              
              if (round(length(results.est$est) * 0.975) == 0) {
                est.ci.upper <- "inf"
              } else {
                est.ci.upper <- round(sort(results.est$est)[
                  length(results.est$est) * 0.975], 2)
              }
              
              
              # number of iterations with significant p-values
              n_sig <- length(which(results[ii, ]$pvalue <= input$p_alpha))
              
              # power (denominator = # all iterations)
              power <- n_sig/(conv * input$ksim)
              
              # power (denominator = # all iterations)
              powerksim <- n_sig/input$ksim
              
              # variance of power across simulations
              n_sig_var <- power * conv * input$ksim * (1 - power)
              
              # print power table
              powertable[i, "Parameter"] <<- results[i, "Parameter"]
              powertable[i, "Value"] <<- hot_to_r(input$AnalysisMod)$Value[target[i]]
              powertable[i, "Median"] <<- median(results[ii, ]$est)
              powertable[i, "Power"] <<- power
              powertable[i, "Power (All Cases)"] <<- powerksim
              
              # print CI table
              ci_table[i, "Parameter"] <<- results.est[i, "Parameter"]
              ci_table[i, "est.ci.lower"] <<- est.ci.lower
              ci_table[i, "est.ci.upper"] <<- est.ci.upper
              
            })
            
            
            # Render table of power analysis results
            output$power <- renderTable({
              powertable
            }, digits = 2, align = "l")
            
            # Add note on power based on convergence rate
            output$powertable_note <- renderText({
              paste('Convergence rate is ', round(conv, 3), '. ', 
                    'Value is the population parameter value as set in Step 3. ',
                    'Median is the median of simulated estimates of a parameter. ',
                    'Power is estimated from all simulations with converged ',
                    'models. Power (All Cases) is estimated from all ', 
                    'simulations, including those with non-converged models ',
                    '(which had no parameter estimates and were counted as ',
                    'failure to reject the null).',
                    sep = "")
            })
            
            # Select parameter for histogram displays
            output$histograms <- renderUI(
              selectInput("para_hist", 
                          label = "Select parameter to display histograms",
                          choices = powertable$Parameter)
            )
            
            # Render histogram of p-values
            output$histop <- renderPlot({
              hist(results[results$Parameter == input$para_hist, ]$pvalue, 
                   breaks = 50, 
                   col = "#75dbd9", border = "white",
                   xlab = "p-values of the Estimated Parameter",
                   ylab = "Number of Simulated Samples",
                   main = "Histogram of Estimated p-Values",
                   xlim = c(0, 1))
              abline(v = input$p_alpha, lwd = 2)
            })
            
            # Footnote
            output$histop_note <- renderText({
              paste('Vertical solid line indicates alpha level.')
            })
            
            # Render histogram of parameter estimates
            output$histoparam <- renderPlot({
              hist(results[results$Parameter == input$para_hist, ]$est, breaks = 100, 
                   col = "#75AADB", border = "white",
                   xlab = "Estimated Parameter Value",
                   ylab = "Number of Simulated Samples",
                   main = "Histogram of Estimated Parameter Values")
              abline(v = hot_to_r(input$AnalysisMod)$Value[which(
                hot_to_r(input$AnalysisMod)$Parameter == input$para_hist)], lwd = 2)
              abline(v = powertable$Median[which(
                powertable$Parameter == input$para_hist)], lty = 3, lwd = 2)
            })
            
            # Footnote
            output$histoparam_note <- renderText({
              paste('95% of parameter estimates fall within the interval [',
                    ci_table[ci_table$Parameter == input$para_hist, ]$est.ci.lower,
                    ', ', 
                    ci_table[ci_table$Parameter == input$para_hist, ]$est.ci.upper,
                    ']. Vertical solid line ',
                    'indicates the population value you set for the parameter; ',
                    'vertical dotted line indicates the median of parameter ',
                    'estimates from the simulated samples.', sep = "")
            })
          })
        }
        
      }
    }
  }
)
  
}


# Run the app -------------------------------------------------------------

shinyApp(ui = ui, server = server)
