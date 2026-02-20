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
if(!require(ggplot2)){install.packages('ggplot2')}
if(!require(rmarkdown)){install.packages('rmarkdown')}
if(!require(knitr)){install.packages('knitr')}
if(!require(qgraph)){install.packages('qgraph')}
library(shiny)
library(lavaan)
library(semPlot)
library(rhandsontable)
library(semTools)
library(tidyr)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(qgraph)


# Source the UI component files
source("ui/header.R")
source("ui/sidebar.R")
source("ui/main_panel.R")

# Source the server functions files
source("server/set_model.R")
source("server/set_residual_variances.R")
source("server/confirm_parameter_values.R")
source("server/estimate_power.R")

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
  header,
# *- Set sidebar with "how to" guide for the app --------------------------
  sidebarLayout(sidebar,
# *- Set main interface ---------------------------------------------------
  main_panel
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

  # Create a reactive value to store powertable
  results <- reactiveVal(list(powertable = NULL, power_note = NULL, data = NULL, histop = NULL, p_alpha = NULL, histop_note = NULL, histoparam_note = NULL))


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

  mg <- set_model(input, output, session, stdlv, structural)

# Events reactive to "Set Residual Variances for Me" in Step 3 ------------

  set_residual_variances(input, output, session, mg)

# Events reactive to "Confirm Parameter Values" in Step 3 -----------------

  confirm_parameter_values(input, output, session, mg, stdlv)


# Events reactive to "Estimate Power via Simulations" in Step 4 -----------

  estimate_power(input, output, session, mg, stdlv, results)


# Events reactive to "download pdf report"  -----------



  # Create downloadable report in markdown TINYTEX NEEDS TO BE INSTALLED
  output$report <- downloadHandler(
    filename = function() {
      paste0("pwrSEM-report-", Sys.Date(), ".pdf")
    },
    content = function(file) {

      # Copy report template to a temp dir (needed for deployment)
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Save the path-diagram plot as a PNG
      plot_file <- file.path(tempdir(), "plot.png")
      png(plot_file, width = 800, height = 600)
      qgraph(mg()[[2]],
            edge.color = "black",
            curvature   = 3,
            structural  = structural(),
            sizeMan     = input$sizeMan,
            sizeLat     = input$sizeLat,
            rotation    = input$rotation)
      dev.off()

      # Snapshot current results (avoids repeated reactive reads)
      res <- results()

      # Build params list for the Rmd
      params <- list(
        model           = input$text1,
        model_plot      = plot_file,
        parameter_table = hot_to_r(input$AnalysisMod),
        sample_size     = input$sampleN,
        alpha_lvl       = input$p_alpha,
        seed            = input$seed,
        nsims           = input$ksim,
        power_table     = res$powertable,
        power_note      = res$power_note,
        data            = res$data,           # full sim_results data frame
        histop          = res$histop,         # default selected param (unused now — report loops all)
        p_alpha         = res$p_alpha,
        histop_note     = res$histop_note,
        histoparam_note = res$histoparam_note, # named character vector
        ci_table        = res$ci_table
      )

      rmarkdown::render(
        tempReport,
        output_file = file,
        params      = params,
        envir       = new.env(parent = globalenv())
      )
    }
  )


}


# Run the app -------------------------------------------------------------

shinyApp(ui = ui, server = server)
