
set_model <- function(input, output, session, stdlv, structural, values) {
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

    # Generate the plot object
    plot_object <- semPaths(vis_fit, edge.color = 'black', curvature = 3,
                            structural = structural(), sizeMan = input$sizeMan,
                            sizeLat = input$sizeLat, rotation = input$rotation)

    # Return parameter table and diagram
    return(list(am, plot_object, vis_fit))

  })




  observeEvent(input$clicks1, {

    # Render interactive parameter table
    output$AnalysisMod <- renderRHandsontable({

      # Set table dimensions
      rhandsontable(mg()[[1]], rowHeaders = NULL, stretchH = "all",
                    height = 400) %>%

        # Set all columns other than "Value" to read-only
        hot_col(col = c("Row", "Parameter", "Label", "Description", "Type",
                        "Free"), readOnly = T) %>%

        # Highlight cell selection
        hot_table(highlightCol = T, highlightRow = T) %>%

        # Disable row and column editing
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })

    # Render diagram
    output$plot <- renderPlot({
      mg()[[2]]  # Print the plot object to render it
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



  # Return the reactive expression
  return(mg)
}
