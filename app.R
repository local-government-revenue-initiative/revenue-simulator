# app.R
# Property Tax Revenue Simulator - Complete Application

# Load all required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(fastDummies)
library(ggplot2)
library(scales)
library(sf)
library(leaflet)


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
source("R/module4_functions.R")
source("R/module5_functions.R")  # Added for Module 5
source("R/module6_functions.R")

# Source UI and server modules
source("modules/module1_ui.R")
source("modules/module1_server.R")
source("modules/module2_ui.R")
source("modules/module2_server.R")
source("modules/module3_ui.R")
source("modules/module3_server.R")
source("modules/module4_ui.R")
source("modules/module4_server.R")
source("modules/module5_ui.R")     # Added for Module 5
source("modules/module5_server.R")  # Added for Module 5
source("modules/module6_ui.R")
source("modules/module6_server.R")

# Define UI
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
      menuItem("Module 5: Tax Burden Analysis", tabName = "module5",  # Updated
               icon = icon("balance-scale")),                          # Updated icon
      menuItem("Module 6: GIS", tabName = "module6", 
               icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      # Add custom CSS for Module 5
      tags$style(HTML("
        /* Module 5 specific styles */
        .info-box {
          min-height: 90px;
        }
        
        .info-box-icon {
          height: 90px;
          line-height: 90px;
        }
        
        .info-box-content {
          padding: 5px 10px;
          margin-left: 90px;
        }
        
        /* Value box improvements for Module 5 */
        .small-box {
          border-radius: 5px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        
        .small-box:hover {
          box-shadow: 0 3px 6px rgba(0,0,0,0.16), 0 3px 6px rgba(0,0,0,0.23);
        }
        
        /* Tab panel styling */
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3c8dbc;
        }
        
        /* Progress bar styling */
        .progress-bar {
          background-color: #3c8dbc;
        }
        
        /* Existing custom styles from original app */
        .btn-primary {
          background-color: #3498db;
          border-color: #2980b9;
        }
        
        .btn-primary:hover {
          background-color: #2980b9;
          border-color: #21618c;
        }
        
        .btn-success {
          background-color: #27ae60;
          border-color: #229954;
        }
        
        .btn-success:hover {
          background-color: #229954;
          border-color: #1e8449;
        }
        
        .btn-lg {
          padding: 10px 20px;
          font-size: 18px;
        }
        
        .box-header {
          background-color: #f5f5f5;
        }
        
        .dataTables_wrapper {
          margin-top: 10px;
        }
        
        @media (max-width: 768px) {
          .content-wrapper, .right-side {
            margin-left: 0 !important;
          }
        }

      /* Enhanced Business License Category Styles */
      .category-header:hover {
        background-color: #e9ecef !important;
      }
      .category-header {
        transition: background-color 0.2s ease;
      }        
      ")
    ),

    # Add missing JavaScript function for toggling all categories
tags$script(HTML("
  function toggleAllCategories(scenarioSuffix, expand) {
    var categories = document.querySelectorAll('[id^=\"category_content_\"][id$=\"_' + scenarioSuffix + '\"]');
    var headers = document.querySelectorAll('[id^=\"category_header_\"][id$=\"_' + scenarioSuffix + '\"]');
    
    categories.forEach(function(content, index) {
      var header = headers[index];
      var icon = header.querySelector('i');
      
      if (expand) {
        content.style.display = 'block';
        if (icon) icon.className = icon.className.replace('fa-chevron-down', 'fa-chevron-up');
      } else {
        content.style.display = 'none';
        if (icon) icon.className = icon.className.replace('fa-chevron-up', 'fa-chevron-down');
      }
    });
  }
"))
  ),
    
    tabItems(
      module1_ui("module1"),
      module2_ui("module2"),
      module3_ui("module3"),
      module4_ui("module4"),
      module5_ui("module5"),  # Now using actual module instead of placeholder
      module6_ui("module6")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Module 1 server - returns processed data
  processed_data <- module1_server("module1")
  
  # Module 2 server - returns parameter configurations  
  param_configs <- module2_server("module2", processed_data)
  
  # Module 3 server - returns tax configurations
  tax_configs <- module3_server("module3", processed_data, param_configs)
  
  # Module 4 server - returns revenue data
  revenue_data <- module4_server("module4", processed_data, param_configs, tax_configs)
  
  # Module 5 server - tax burden analysis
  analysis_results <- module5_server("module5", revenue_data)
  
  # Module 6 server - GIS layer filtering
  gis_results <- module6_server("module6", revenue_data)
}

# Run the application
shinyApp(ui = ui, server = server)