library(shiny)

source("ui/tabs/tab1_specify_model.R")
source("ui/tabs/tab2_visualize.R")
source("ui/tabs/tab3_set_parameters.R")
source("ui/tabs/tab4_estimate_power.R")
source("ui/tabs/tab5_help.R")
source("ui/tabs/tab6_resources.R")

main_panel <- mainPanel(
  tabsetPanel(
    id = "tabby",
# *--- Specify model -------------------------------------------------------------
    tab1_ui,
# *--- Visualize Model -------------------------------------------------------------
    tab2_ui,
# *--- Set Parameters -------------------------------------------------------------
    tab3_ui,
# *--- Estimate Power -------------------------------------------------------------
    tab4_ui,
# *--- Help ---------------------------------------------------------------
    tab5_ui,
# *--- Resources ----------------------------------------------------------
    tab6_ui
  )
)
