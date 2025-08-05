# app.R
# Load all required libraries FIRST
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(readr)
library(fastDummies)

# Set options
options(shiny.maxRequestSize = 30*1024^2)  # 30MB max file size

# Null-coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Source all module functions
source("R/module1_functions.R")
source("R/module2_functions.R")
source("R/module3_functions.R")

# Source UI and server modules
source("modules/module1_ui.R")
source("modules/module1_server.R")
source("modules/module2_ui.R")
source("modules/module2_server.R")
source("modules/module3_ui.R")
source("modules/module3_server.R")

# Update UI to include Module 2
ui <- dashboardPage(
  dashboardHeader(title = "Property Tax Revenue Simulator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Module 1: Data Input", tabName = "module1", 
               icon = icon("database")),
      menuItem("Module 2: Value Parameters", tabName = "module2", 
               icon = icon("calculator")),
      menuItem("Module 3: Tax Parameters", tabName = "module3", 
               icon = icon("percent")),
      menuItem("Module 4: Revenue", tabName = "module4", 
               icon = icon("dollar-sign")),
      menuItem("Module 5: Analysis", tabName = "module5", 
               icon = icon("chart-bar")),
      menuItem("Module 6: GIS", tabName = "module6", 
               icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      module1_ui("module1"),
      module2_ui("module2"),
      module3_ui("module3"),
      
      # Placeholder for other modules
      tabItem(tabName = "module4", h2("Module 4: Coming Soon")),
      tabItem(tabName = "module5", h2("Module 5: Coming Soon")),
      tabItem(tabName = "module6", h2("Module 6: Coming Soon"))
    )
  )
)

# Update the server function:
server <- function(input, output, session) {
  # Module 1 server
  processed_data <- module1_server("module1")
  
  # Module 2 server - pass processed data
  param_configs <- module2_server("module2", processed_data)
  
  # Module 3 server
  tax_configs <- module3_server("module3", processed_data, param_configs)
}

# Run the app
shinyApp(ui, server)