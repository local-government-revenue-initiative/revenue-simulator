# app.R
# Property Tax Revenue Simulator - Complete Application
# Updated for simplified data loading approach

# Load all required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)
library(sf)
library(leaflet)
library(shinycssloaders)
library(shinybusy)
library(shinytoastr)

# Set options
options(shiny.maxRequestSize = 100 * 1024^2) # 100MB max file size for large RDS files

# Null-coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Source all module functions
source("R/module1_functions.R")
source("R/module2_functions.R")
source("R/module3_functions.R")
source("R/module4_functions.R")
source("R/module5_functions.R")
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
source("modules/module5_ui.R")
source("modules/module5_server.R")
source("modules/module6_ui.R")
source("modules/module6_server.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Property Tax Revenue Simulator"),

  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Module 1: Data Input",
        tabName = "module1",
        icon = icon("database")
      ),
      menuItem(
        "Module 2: Value Parameters",
        tabName = "module2",
        icon = icon("calculator")
      ),
      menuItem(
        "Module 3: Tax Parameters",
        tabName = "module3",
        icon = icon("money-bill-wave")
      ),
      menuItem(
        "Module 4: Revenue",
        tabName = "module4",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Module 5: Tax Burden",
        tabName = "module5",
        icon = icon("balance-scale")
      ),
      menuItem(
        "Module 6: Map",
        tabName = "module6",
        icon = icon("map")
      )
    )
  ),

  dashboardBody(
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(HTML(
        "
        .content-wrapper {
          overflow-y: auto;
        }
        .box-header {
          cursor: pointer;
        }
        .collapsed-box .box-body {
          display: none;
        }
      "
      )),
      # JavaScript for collapsible boxes
      tags$script(HTML(
        "
        $(document).on('click', '.box-header', function() {
          var box = $(this).closest('.box');
          var content = box.find('.box-body');
          var icon = $(this).find('.fa-chevron-down, .fa-chevron-up');
          
          if (content.is(':visible')) {
            content.slideUp();
            if (icon.length) icon.removeClass('fa-chevron-up').addClass('fa-chevron-down');
          } else {
            content.slideDown();
            if (icon.length) icon.removeClass('fa-chevron-down').addClass('fa-chevron-up');
          }
        });
      "
      ))
    ),

    tabItems(
      module1_ui("module1"),
      module2_ui("module2"),
      module3_ui("module3"),
      module4_ui("module4"),
      module5_ui("module5"),
      module6_ui("module6")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Module 1 server - returns authentication status, data, and parameters
  module1_return <- module1_server("module1")

  # Extract components from Module 1 return
  # These are reactive expressions that other modules can use

  # Processed data (combined property/business data)
  processed_data <- reactive({
    req(module1_return$authenticated())
    module1_return$combined_data()
  })

  # Parameter tables from the loaded city data
  param_additions <- reactive({
    module1_return$param_additions()
  })

  param_features <- reactive({
    module1_return$param_features()
  })

  param_prop_struct_type <- reactive({
    module1_return$param_prop_struct_type()
  })

  param_tax_min_rate <- reactive({
    module1_return$param_tax_min_rate()
  })

  param_license <- reactive({
    module1_return$param_license()
  })

  # Module 2 Server - receives data and default parameters
  # Returns both configs and calculated values
  module2_return <- module2_server(
    "module2",
    processed_data,
    param_additions,
    param_features,
    param_prop_struct_type
  )

  property_configs <- module2_return$configs
  calculated_property_values <- module2_return$calculated_values

  # Module 3 Server - receives data, property configs, calculated values, and tax parameters
  tax_configs <- module3_server(
    "module3",
    processed_data,
    property_configs,
    calculated_property_values,
    param_tax_min_rate,
    param_license
  )

  # Module 4 Server - receives everything needed for revenue calculations
  revenue_data <- module4_server(
    "module4",
    processed_data,
    property_configs,
    tax_configs,
    calculated_property_values
  )

  # Module 5 server - tax burden analysis
  analysis_results <- module5_server("module5", revenue_data)

  # Module 6 server - GIS visualization
  gis_results <- module6_server("module6", revenue_data)
}

# Run the application
shinyApp(ui = ui, server = server)
