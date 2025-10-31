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
      paste("report-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      print("results")
      print(results)

      # Save the plot from mg()[[2]] as an image
      plot_file <- file.path(tempdir(), "plot.png")
      png(plot_file, width = 800, height = 600)
      qgraph(mg()[[2]], edge.color = 'black', curvature = 3,
             structural = structural(), sizeMan = input$sizeMan,
             sizeLat = input$sizeLat, rotation = input$rotation)
      dev.off()

      # Save the p-values histogram as an image
      # histop_file <- file.path(tempdir(), "histop.png")

      # histop_val <- results()$histop
      # value <- if (length(histop_val) == 0 || isTRUE(histop_val == "")) {
      #   numeric(0)
      # } else {
      #   as.numeric(histop_val)
      # }
      # png(histop_file, width = 800, height = 600)
      # tryCatch({
      #   hist(value,
      #         breaks = 50,
      #         col = "#75dbd9", border = "white",
      #         xlab = "p-values of the Estimated Parameter",
      #         ylab = "Number of Simulated Samples",
      #         main = "Histogram of Estimated p-Values",
      #         xlim = c(0, 1))
      #   abline(v = input$p_alpha, lwd = 2)
      #   dev.off()
      # }, error = function(e) {
      #   # Handle error: plot a message
      #   message("Error plotting histogram:", e$message)
      #   dev.off()
      # })


      # # Save the estimated parameter values histogram as an image
      # histoparam_file <- file.path(tempdir(), "histoparam.png")
      # png(histoparam_file, width = 800, height = 600)
      # histoparam_val <- results()$histoparam
      # value <- if (length(histoparam_val) == 0 || isTRUE(histoparam_val == "")) {
      #   numeric(0)
      # } else {
      #   as.numeric(histoparam_val)
      # }
      # tryCatch({
      #   hist(value,
      #         breaks = 100,
      #         col = "#75AADB", border = "white",
      #         xlab = "Estimated Parameter Value",
      #         ylab = "Number of Simulated Samples",
      #         main = "Histogram of Estimated Parameter Values")
      #   abline(v = hot_to_r(input$AnalysisMod)$Value[which(
      #     hot_to_r(input$AnalysisMod)$Parameter == input$para_hist)], lwd = 2)
      #   abline(v = results()$powertable$Median[which(
      #     results()$powertable$Parameter == input$para_hist)], lty = 3, lwd = 2)
      #   dev.off()
      # }, error = function(e) {
      #   # Handle error: plot a message
      #   message("Error plotting histogram:", e$message)
      #   dev.off()
      # })


      #list(powertable = NULL, power_note = NULL, histop = NULL, histop_note = NULL, histoparam = NULL, histoparam_note = NULL)

      # Set up parameters to pass to Rmd document
      params <- list(model = input$text1,
                    model_plot = plot_file,
                    parameter_table = hot_to_r(input$AnalysisMod),
                    sample_size = input$sampleN,
                    alpha_lvl = input$p_alpha,
                    seed=input$seed,
                    nsims=input$ksim,
                    power_table=results()$powertable,
                    power_note=results()$power_note,
                    data=results()$data,
                    histop=results()$histop,
                    p_alpha=results()$p_alpha,
                    histop_note=results()$histop_note,
                    histoparam_note=results()$histoparam_note)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    })
}


# Run the app -------------------------------------------------------------

shinyApp(ui = ui, server = server)
