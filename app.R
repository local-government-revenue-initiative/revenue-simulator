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

# Source all module functions
source("R/module1_functions.R")
source("R/module2_functions.R")

# Source UI and server modules
source("modules/module1_ui.R")
source("modules/module1_server.R")
source("modules/module2_ui.R")
source("modules/module2_server.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Property Tax Revenue Simulator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Module 1: Data Input", tabName = "module1", 
               icon = icon("database")),
      menuItem("Module 2: Parameters", tabName = "module2", 
               icon = icon("sliders-h")),
      menuItem("Module 3: Revenue", tabName = "module3", 
               icon = icon("calculator")),
      menuItem("Module 4: Analysis", tabName = "module4", 
               icon = icon("chart-bar")),
      menuItem("Module 5: GIS", tabName = "module5", 
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
      
      # Placeholder for other modules
      tabItem(tabName = "module3", h2("Module 3: Coming Soon")),
      tabItem(tabName = "module4", h2("Module 4: Coming Soon")),
      tabItem(tabName = "module5", h2("Module 5: Coming Soon"))
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Module 1 server - returns processed data
  processed_data <- module1_server("module1")
  
  # Module 2 server - returns configurations
  configurations <- module2_server("module2", processed_data)
  
  # Pass data to other modules when implemented
  # module3_server("module3", processed_data, configurations)
  # etc.
}

# Run the app
shinyApp(ui, server)