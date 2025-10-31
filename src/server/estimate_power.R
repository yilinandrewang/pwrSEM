estimate_power <- function(input, output, session, mg, stdlv, results) {
  observeEvent(input$sim, {
    # Initialize powertable
    temp_powertable <- NULL

    test_model_enter <- try(parameterTable(mg()[[3]]), silent = T)

    # Test if model is entered
    if (inherits(test_model_enter, "try-error")) {
      output$step4_model_warning <- renderText(
        "No model detected. Did you enter a model in Step 1?")

    } else {

      text1.t <- parameterTable(mg()[[3]])
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

        PopMod.t <- parameterTable(mg()[[3]])
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

          error <- FALSE

          # Set simulation seed based on user input
          set.seed(input$seed)

          # Define population model and target parameter based on user input
          PopMod.t <- parameterTable(mg()[[3]])
          PopMod.t[, "ustart"] <- hot_to_r(input$AnalysisMod)[, "Value"]
          PopMod.t <- PopMod.t[, c(
            "id", "lhs", "op", "rhs", "user", "block", "group", "free",
            "ustart", "exo", "label", "plabel")]
          target <- which(hot_to_r(input$AnalysisMod)$Effect == TRUE)

          # Set progress bar
          withProgress(message = 'Simulating', value = 0, {

            # Loop by iteration
            for (i in 1:input$ksim) {

              # Simulate and store data based on sample size input
              data <- simulateData(PopMod.t, sample.nobs = input$sampleN)
              data <- as.data.frame(data)

              # Fit analysis model to data
              fit <- tryCatch(sem(model = input$text1, data = data, std.lv = stdlv()),error = function(e) e)
              if(is(fit, "error")) {
                output$step4_para_warning <- renderText(paste(
                  "Simulations could not be run because the model is incorrectly specified.",
                  geterrmessage()))
                error <- TRUE
                break
              }
              fit <- sem(model = input$text1, data = data, std.lv = stdlv())

              # Store parameter row
              results <- rbind(results, parameterEstimates(fit)[target, ])

              # Display progress bar
              incProgress(1/input$ksim, detail = paste("sample", i, "of",
                                                       input$ksim))
            }

            if(!error) {

              # Convergence rate
              conv <- (input$ksim - sum(is.na(results$pvalue)))/input$ksim

              # Create placeholder powertable and CI tables
              temp_powertable <- as.data.frame(matrix(NA, nrow = length(target), ncol = 5))
              colnames(temp_powertable) <- c("Parameter", "Value", "Median", "Power",
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
                temp_powertable[i, "Parameter"] <<- results[i, "Parameter"]
                temp_powertable[i, "Value"] <<- hot_to_r(input$AnalysisMod)$Value[target[i]]
                temp_powertable[i, "Median"] <<- median(results[ii, ]$est)
                temp_powertable[i, "Power"] <<- power
                temp_powertable[i, "Power (All Cases)"] <<- powerksim

                # print CI table
                ci_table[i, "Parameter"] <<- results.est[i, "Parameter"]
                ci_table[i, "est.ci.lower"] <<- est.ci.lower
                ci_table[i, "est.ci.upper"] <<- est.ci.upper

              })

              print("ci_table")
              print(ci_table)


              # Render table of power analysis results
              output$power <- renderTable({
                temp_powertable
              }, digits = 2, align = "l")

              # Add note on power based on convergence rate

              temp_power_note <- paste('Convergence rate is ', round(conv, 3), '. ',
                      'Value is the population parameter value as set in Step 3. ',
                      'Median is the median of simulated estimates of a parameter. ',
                      'Power is estimated from all simulations with converged ',
                      'models. Power (All Cases) is estimated from all ',
                      'simulations, including those with non-converged models ',
                      '(which had no parameter estimates and were counted as ',
                      'failure to reject the null).',
                      sep = "")
              output$powertable_note <- renderText({temp_power_note})

              # Select parameter for histogram displays
              output$histograms <- renderUI(
                selectInput("para_hist",
                            label = "Select parameter to display histograms",
                            choices = temp_powertable$Parameter,
                            selected = temp_powertable$Parameter[1])
              )

              print("print(results)")
              print(results)

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

              temp_histop_note <- paste('Vertical solid line indicates alpha level.')

              # Footnote
              output$histop_note <- renderText({
                temp_histop_note
              })


              temp_histoparam <- results[results$Parameter == input$para_hist, ]$est


              # Render histogram of parameter estimates
              output$histoparam <- renderPlot({
                hist(results[results$Parameter == input$para_hist, ]$est, breaks = 100,
                     col = "#75AADB", border = "white",
                     xlab = "Estimated Parameter Value",
                     ylab = "Number of Simulated Samples",
                     main = "Histogram of Estimated Parameter Values")
                abline(v = hot_to_r(input$AnalysisMod)$Value[which(
                  hot_to_r(input$AnalysisMod)$Parameter == input$para_hist)], lwd = 2)
                abline(v = temp_powertable$Median[which(
                  temp_powertable$Parameter == input$para_hist)], lty = 3, lwd = 2)
              })


              temp_histoparam_note <- paste('95% of parameter estimates fall within the interval [',
                      ci_table[ci_table$Parameter == input$para_hist, ]$est.ci.lower,
                      ', ',
                      ci_table[ci_table$Parameter == input$para_hist, ]$est.ci.upper,
                      ']. Vertical solid line ',
                      'indicates the population value you set for the parameter; ',
                      'vertical dotted line indicates the median of parameter ',
                      'estimates from the simulated samples.', sep = "")
              # Footnote
              output$histoparam_note <- renderText({
              temp_histoparam_note
              })

              observe({
                req(input$para_hist)  # Wait until histogram object/data is ready

                # Then assign the updated list to your reactiveVal
                results(list(
                  powertable = temp_powertable,
                  power_note = temp_power_note,
                  data = results,
                  histop = input$para_hist,
                  p_alpha = input$p_alpha,
                  histop_note = temp_histop_note,
                  histoparam_note = temp_histoparam_note
                ))
              })
            }
          })
          print(results)
        }
      }
    }
  })
}
