set_residual_variances <- function(input, output, session, mg) {
  observeEvent(input$autoRes, {
    # Test if model is entered
    test_model_enter <- try(parameterTable(mg()[[3]]), silent = T)

    if (inherits(test_model_enter, "try-error")) {

      output$step3_model_warning <- renderText(
        "No model detected. Did you enter a model in Step 1?")

    } else {

      # Create one parameter table that we use later to obtain psi matrix
      text1.t <- parameterTable(mg()[[3]])
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
        PopMod.t <- parameterTable(mg()[[3]])
        PopMod.t[, "ustart"] <- hot_to_r(input$AnalysisMod)[, "Value"]
        PopMod.t[is.na(PopMod.t[, "ustart"] == T), "ustart"] <- 0
        PopMod.t <- PopMod.t[(PopMod.t$op) != ":=", ] # Exclude labelled parameter

        # Test model-implied covariance matrix
        text1.t <- text1.t[, c("id", "lhs", "op", "rhs", "user", "block", "group",
                                "free", "ustart", "exo", "label", "plabel")]
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
            siq <- tryCatch(solve(I - beta) %*% psi %*% solve(I - t(beta)), error = function(e) e)
            if(is(siq, "error")) {
              output$resid_warning <- renderText(paste(
                "Residual variances could not be calculated."))
            } else {
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
  })
}
