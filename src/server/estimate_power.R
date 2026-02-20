estimate_power <- function(input, output, session, mg, stdlv, results) {
  observeEvent(input$sim, {

    test_model_enter <- try(parameterTable(mg()[[3]]), silent = TRUE)

    # Test if model is entered
    if (inherits(test_model_enter, "try-error")) {
      output$step4_model_warning <- renderText(
        "No model detected. Did you enter a model in Step 1?")

    } else {

      text1.t <- parameterTable(mg()[[3]])
      text1.t$free <- 0

      test_model_dim <- try(
        text1.t[, "ustart"] <- hot_to_r(input$AnalysisMod)[, "Value"],
        silent = TRUE)

      # Test if parameter table has correct dimensions
      if (inherits(test_model_dim, "try-error")) {

        output$step4_dim_warning <- renderText(paste(
          "Incorrect dimensions of parameter table. Please regenerate",
          "the parameter table by resetting the model in Step 1."))
        output$step4_model_warning <- renderText("")

      # Test if all parameters are specified
      } else if (TRUE %in% is.na(hot_to_r(input$AnalysisMod)$Value)) {

        output$step4_para_all     <- renderText("All parameter values need to be specified.")
        output$step4_para_warning <- renderText("")

      # Test if at least one target effect is selected
      } else if (!(TRUE %in% hot_to_r(input$AnalysisMod)$Effect)) {

        output$step4_para_warning <- renderText(
          "Please select at least one parameter as the target effect.")
        output$step4_para_all <- renderText("")

      } else {

        PopMod.t <- parameterTable(mg()[[3]])
        PopMod.t[, "ustart"] <- hot_to_r(input$AnalysisMod)[, "Value"]

        # Test if model-implied covariance matrix is positive definite
        if (inherits(
          try(simulateData(PopMod.t, sample.nobs = input$sampleN), silent = TRUE),
          "try-error")) {

          output$step4_para_warning <- renderText(paste(
            "Simulations could not be run because the model-implied covariance",
            "matrix is not positive definite. Make sure the parameter values",
            "you entered in Step 3 can produce a positive definite model-implied",
            "covariance matrix."))

        } else {

          output$step4_para_warning <- output$step4_para_all <- renderText("")

          # NOTE: named sim_results (not 'results') to avoid shadowing
          # the reactiveVal passed into this function
          sim_results <- NULL
          error       <- FALSE

          set.seed(input$seed)

          PopMod.t <- parameterTable(mg()[[3]])
          PopMod.t[, "ustart"] <- hot_to_r(input$AnalysisMod)[, "Value"]
          PopMod.t <- PopMod.t[, c("id", "lhs", "op", "rhs", "user", "block",
                                    "group", "free", "ustart", "exo", "label",
                                    "plabel")]
          target <- which(hot_to_r(input$AnalysisMod)$Effect == TRUE)

          withProgress(message = "Simulating", value = 0, {

            for (i in 1:input$ksim) {

              data <- as.data.frame(
                simulateData(PopMod.t, sample.nobs = input$sampleN))

              fit <- tryCatch(
                sem(model = input$text1, data = data, std.lv = stdlv()),
                error = function(e) e)

              if (is(fit, "error")) {
                output$step4_para_warning <- renderText(paste(
                  "Simulations could not be run because the model is",
                  "incorrectly specified.", geterrmessage()))
                error <- TRUE
                break
              }

              fit <- sem(model = input$text1, data = data, std.lv = stdlv())
              sim_results <- rbind(sim_results, parameterEstimates(fit)[target, ])

              incProgress(1/input$ksim,
                          detail = paste("sample", i, "of", input$ksim))
            }

            if (!error) {

              # Convergence rate
              conv <- (input$ksim - sum(is.na(sim_results$pvalue))) / input$ksim

              # Placeholder tables
              temp_powertable <- as.data.frame(
                matrix(NA, nrow = length(target), ncol = 5))
              colnames(temp_powertable) <- c("Parameter", "Value", "Median",
                                             "Power", "Power (All Cases)")

              ci_table <- as.data.frame(
                matrix(NA, nrow = length(target), ncol = 3))
              colnames(ci_table) <- c("Parameter", "est.ci.lower", "est.ci.upper")

              # Label each row with its parameter name
              sim_results$Parameter <- paste(
                sim_results$lhs, sim_results$op, sim_results$rhs, sep = " ")

              lapply(1:length(target), function(i) {

                ii <- seq(from = i, to = nrow(sim_results), by = length(target))

                ii.est <- which(!is.na(sim_results$est))
                ii.est <- ii.est[ii.est %in% ii]
                results.est <- sim_results[ii.est, ]

                est.ci.lower <- if (round(nrow(results.est) * 0.025) == 0) {
                  "inf"
                } else {
                  round(sort(results.est$est)[round(nrow(results.est) * 0.025)], 2)
                }

                est.ci.upper <- if (round(nrow(results.est) * 0.975) == 0) {
                  "inf"
                } else {
                  round(sort(results.est$est)[round(nrow(results.est) * 0.975)], 2)
                }

                n_sig     <- sum(sim_results[ii, ]$pvalue <= input$p_alpha,
                                 na.rm = TRUE)
                power     <- n_sig / (conv * input$ksim)
                powerksim <- n_sig / input$ksim

                temp_powertable[i, "Parameter"]         <<- sim_results[i, "Parameter"]
                temp_powertable[i, "Value"]             <<- hot_to_r(input$AnalysisMod)$Value[target[i]]
                temp_powertable[i, "Median"]            <<- median(sim_results[ii, ]$est, na.rm = TRUE)
                temp_powertable[i, "Power"]             <<- power
                temp_powertable[i, "Power (All Cases)"] <<- powerksim

                ci_table[i, "Parameter"]    <<- sim_results[i, "Parameter"]
                ci_table[i, "est.ci.lower"] <<- est.ci.lower
                ci_table[i, "est.ci.upper"] <<- est.ci.upper
              })

              # Render power table
              output$power <- renderTable({ temp_powertable }, digits = 2, align = "l")

              temp_power_note <- paste0(
                "Convergence rate is ", round(conv, 3), ". ",
                "Value is the population parameter value as set in Step 3. ",
                "Median is the median of simulated estimates of a parameter. ",
                "Power is estimated from all simulations with converged models. ",
                "Power (All Cases) is estimated from all simulations, including ",
                "those with non-converged models (which had no parameter estimates ",
                "and were counted as failure to reject the null).")
              output$powertable_note <- renderText({ temp_power_note })

              # Render parameter selector dropdown
              output$histograms <- renderUI({
                selectInput("para_hist",
                            label    = "Select parameter to display histograms",
                            choices  = temp_powertable$Parameter,
                            selected = temp_powertable$Parameter[1])
              })

              # Build notes (named vectors, one entry per parameter)
              temp_histop_note <- "Vertical solid line indicates alpha level."

              temp_histoparam_note <- sapply(temp_powertable$Parameter, function(p) {
                ci_row <- ci_table[ci_table$Parameter == p, ]
                paste0(
                  "95% of parameter estimates fall within the interval [",
                  ci_row$est.ci.lower, ", ", ci_row$est.ci.upper, "]. ",
                  "Vertical solid line indicates the population value you set ",
                  "for the parameter; vertical dotted line indicates the median ",
                  "of parameter estimates from the simulated samples.")
              })
              names(temp_histoparam_note) <- temp_powertable$Parameter

              # local() freezes a snapshot of simulation data so render closures
              # are not affected if the user re-runs simulations later
              local({
                snap_sim         <- sim_results
                snap_pt          <- temp_powertable
                snap_ci          <- ci_table
                snap_alpha       <- input$p_alpha
                snap_histop_note <- temp_histop_note
                snap_param_note  <- temp_histoparam_note
                snap_power_note  <- temp_power_note

                # p-value histogram — req() waits for selectInput to exist
                output$histop <- renderPlot({
                  req(input$para_hist)
                  d <- snap_sim[snap_sim$Parameter == input$para_hist, ]
                  req(nrow(d) > 0)
                  hist(d$pvalue,
                       breaks = 50, col = "#75dbd9", border = "white",
                       xlab = "p-values of the Estimated Parameter",
                       ylab = "Number of Simulated Samples",
                       main = "Histogram of Estimated p-Values",
                       xlim = c(0, 1))
                  abline(v = snap_alpha, lwd = 2)
                })

                output$histop_note <- renderText({ snap_histop_note })

                # Parameter estimate histogram
                output$histoparam <- renderPlot({
                  req(input$para_hist)
                  d <- snap_sim[snap_sim$Parameter == input$para_hist, ]
                  req(nrow(d) > 0)
                  hist(d$est,
                       breaks = 100, col = "#75AADB", border = "white",
                       xlab = "Estimated Parameter Value",
                       ylab = "Number of Simulated Samples",
                       main = "Histogram of Estimated Parameter Values")
                  mod_df  <- hot_to_r(input$AnalysisMod)
                  pop_val <- mod_df$Value[mod_df$Parameter == input$para_hist]
                  med_val <- snap_pt$Median[snap_pt$Parameter == input$para_hist]
                  if (length(pop_val) == 1 && !is.na(pop_val))
                    abline(v = as.numeric(pop_val), lwd = 2)
                  if (length(med_val) == 1 && !is.na(med_val))
                    abline(v = as.numeric(med_val), lty = 3, lwd = 2)
                })

                output$histoparam_note <- renderText({
                  req(input$para_hist)
                  snap_param_note[[input$para_hist]]
                })

                # Update reactiveVal so PDF report has all data it needs
                results(list(
                  powertable      = snap_pt,
                  power_note      = snap_power_note,
                  data            = snap_sim,
                  histop          = snap_pt$Parameter[1],
                  p_alpha         = snap_alpha,
                  histop_note     = snap_histop_note,
                  histoparam_note = snap_param_note,
                  ci_table        = snap_ci
                ))

              }) # end local()

            } # end if (!error)
          }) # end withProgress
        }
      }
    }
  })
}
