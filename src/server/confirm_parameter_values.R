confirm_parameter_values <- function(input, output, session, mg, stdlv) {
  observeEvent(input$tab3to4, {
    test_model_enter <- try(parameterTable(mg()[[3]]), silent = T)

    # Test if model is entered
    if (inherits(test_model_enter, "try-error")) {

      output$step3_model_warning <- renderText(
        "No model detected. Did you enter a model in Step 1?")

    } else {

      text1.t <- parameterTable(mg()[[3]])
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

        PopMod.t <- parameterTable(mg()[[3]])
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
          # Create dataframe to store results
          try_results <- NULL
          error <- FALSE
          target <- which(hot_to_r(input$AnalysisMod)$Effect == TRUE)
          # Loop by iteration
          for (i in 1:5) {
            # Simulate and store data based on sample size input
            try_data <- simulateData(PopMod.t, sample.nobs = input$sampleN)
            try_data <- as.data.frame(try_data)
            try_fit <- tryCatch(sem(model = input$text1, data = try_data, std.lv = stdlv()),error = function(e) e)
            if(is(try_fit, "error")) {
              output$step3_para_warning <- renderText(paste(
                "Simulations could not be run because the model is incorrectly specified.",
                geterrmessage()))
              error <- TRUE
              break;
            }
            try_fit <- sem(model = input$text1, data = try_data, std.lv = stdlv())

            # Store parameter row
            try_results <- rbind(try_results, parameterEstimates(try_fit)[target, ])
          }
          if(!error) {

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
  })
}
