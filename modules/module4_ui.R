# modules/module4_ui.R

module4_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "module4",
    fluidRow(
      box(
        title = "Module 4: Revenue Calculations and Analysis",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        
        # Control panel
        fluidRow(
          column(12,
                 p("Calculate and compare revenue across all three scenarios."),
                 actionButton(ns("calculate_revenue"), "Calculate Revenue for All Scenarios", 
                              icon = icon("calculator"), class = "btn-success btn-lg"),
                 br(),
                 br()
          )
        ),
        
        hr(),
        
        # Summary cards
        fluidRow(
          column(4,
                 valueBoxOutput(ns("existing_total_revenue"))),
          column(4,
                 valueBoxOutput(ns("scenario_a_total_revenue"))),
          column(4,
                 valueBoxOutput(ns("scenario_b_total_revenue")))
        ),
        
        # Tabbed interface for different views
        tabsetPanel(id = ns("revenue_tabs"),
                    
                    # Tab 1: Summary Comparison
                    tabPanel("Summary Comparison",
                             br(),
                             fluidRow(
                               column(12,
                                      h4("Chart 1A: Revenue by Type - All Properties"),
                                      plotOutput(ns("revenue_by_type_plot"), height = "400px")
                               )
                             ),
                             br(),
                             fluidRow(
                               column(12,
                                      h4("Chart 1B: Revenue by Type - Filtered to Compliers"),
                                      plotOutput(ns("revenue_by_type_compliers_plot"), height = "400px")
                               )
                             ),
                             br(),
                             fluidRow(
                               column(12,
                                      h4("Chart 1C: Revenue with Filtering Options"),
                                      fluidRow(
                                        column(4,
                                               selectInput(ns("filter_type"),
                                                           "Filter By:",
                                                           choices = c("No Filter" = "none",
                                                                       "Structure Types" = "structure",
                                                                       "Property Types" = "property",
                                                                       "License Categories" = "category",
                                                                       "License Subcategories" = "subcategory"))
                                        ),
                                        column(8,
                                               uiOutput(ns("filter_values_ui"))
                                        )
                                      ),
                                      plotOutput(ns("revenue_filtered_plot"), height = "400px")
                               )
                             )
                    ),
                    
                    # Tab 2: Detailed Data
                    tabPanel("Detailed Data",
                             br(),
                             fluidRow(
                               column(4,
                                      selectInput(ns("detailed_scenario"),
                                                  "Select Scenario:",
                                                  choices = c("Existing" = "existing",
                                                              "Scenario A" = "scenario_a",
                                                              "Scenario B" = "scenario_b"))
                               ),
                               column(4,
                                      numericInput(ns("detailed_rows"),
                                                   "Number of rows to display:",
                                                   value = 100,
                                                   min = 10,
                                                   max = 5000,
                                                   step = 100)
                               ),
                               column(4,
                                      br(),
                                      downloadButton(ns("download_data"), 
                                                     "Download Full Dataset",
                                                     class = "btn-primary")
                               )
                             ),
                             br(),
                             DT::dataTableOutput(ns("detailed_data_table"))
                    )
        )
      )
    )
  )
}