# modules/module2_ui.R

module2_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "module2",
    fluidRow(
      box(
        title = "Module 2: Parameter Configuration",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        
        # Scenario selector
        fluidRow(
          column(4,
                 selectInput(ns("scenario_select"), 
                             "Select Scenario to Configure:",
                             choices = c("Existing" = "existing",
                                         "Alternative A" = "alt_a",
                                         "Alternative B" = "alt_b"),
                             selected = "existing")
          ),
          column(4,
                 actionButton(ns("copy_existing"), 
                              "Copy from Existing", 
                              icon = icon("copy"),
                              class = "btn-info")
          ),
          column(4,
                 downloadButton(ns("download_config"), 
                                "Download Configuration",
                                class = "btn-success")
          )
        ),
        
        hr(),
        
        # Configuration tabs
        tabsetPanel(id = ns("config_tabs"),
                    tabPanel("Inflation Adjustment",
                             br(),
                             fluidRow(
                               column(6,
                                      numericInput(ns("inflation_rate"),
                                                   "Inflation Adjustment Percentage:",
                                                   value = 0,
                                                   min = -100,
                                                   max = 1000,
                                                   step = 0.1)
                               ),
                               column(6,
                                      helpText("Enter the inflation adjustment as a percentage.",
                                               "Positive values increase property values,",
                                               "negative values decrease them.")
                               )
                             )
                    ),
                    
                    tabPanel("Property Types",
                             br(),
                             helpText("Set tax rates and minimum tax amounts for each property type."),
                             br(),
                             uiOutput(ns("property_types_ui"))
                    ),
                    
                    tabPanel("Property Features",
                             br(),
                             helpText("Set weights for property features used in value estimation.",
                                      "Higher weights mean the feature has more impact on property value."),
                             br(),
                             fluidRow(
                               column(6,
                                      actionButton(ns("reset_weights"), 
                                                   "Reset to Defaults", 
                                                   icon = icon("undo"),
                                                   class = "btn-warning btn-sm")
                               ),
                               column(6,
                                      actionButton(ns("normalize_weights"), 
                                                   "Normalize Weights", 
                                                   icon = icon("balance-scale"),
                                                   class = "btn-info btn-sm")
                               )
                             ),
                             br(),
                             uiOutput(ns("property_features_ui"))
                    ),
                    
                    tabPanel("Business Categories",
                             br(),
                             helpText("Set tax rates and minimum tax amounts for each business category."),
                             br(),
                             uiOutput(ns("business_categories_ui"))
                    ),
                    
                    tabPanel("Advanced Options",
                             br(),
                             h4("Area and Value Bands (Optional)"),
                             helpText("Set different tax rates based on property area or estimated value ranges."),
                             br(),
                             
                             h5("Area-based Tax Bands"),
                             fluidRow(
                               column(3, numericInput(ns("area_band1_max"), "Band 1 Max (sq ft):", 100)),
                               column(3, numericInput(ns("area_band1_rate"), "Band 1 Rate (%):", 1)),
                               column(3, numericInput(ns("area_band2_max"), "Band 2 Max (sq ft):", 500)),
                               column(3, numericInput(ns("area_band2_rate"), "Band 2 Rate (%):", 1.5))
                             ),
                             fluidRow(
                               column(3, numericInput(ns("area_band3_max"), "Band 3 Max (sq ft):", 1000)),
                               column(3, numericInput(ns("area_band3_rate"), "Band 3 Rate (%):", 2)),
                               column(3, helpText("Above Band 3:")),
                               column(3, numericInput(ns("area_band4_rate"), "Rate (%):", 2.5))
                             ),
                             
                             hr(),
                             
                             h5("Value-based Tax Bands"),
                             fluidRow(
                               column(3, numericInput(ns("value_band1_max"), "Band 1 Max ($):", 50000)),
                               column(3, numericInput(ns("value_band1_rate"), "Band 1 Rate (%):", 0.5)),
                               column(3, numericInput(ns("value_band2_max"), "Band 2 Max ($):", 200000)),
                               column(3, numericInput(ns("value_band2_rate"), "Band 2 Rate (%):", 1))
                             ),
                             fluidRow(
                               column(3, numericInput(ns("value_band3_max"), "Band 3 Max ($):", 500000)),
                               column(3, numericInput(ns("value_band3_rate"), "Band 3 Rate (%):", 1.5)),
                               column(3, helpText("Above Band 3:")),
                               column(3, numericInput(ns("value_band4_rate"), "Rate (%):", 2))
                             )
                    )
        ),
        
        hr(),
        
        # Save configuration
        fluidRow(
          column(12,
                 actionButton(ns("save_config"), 
                              "Save Configuration", 
                              icon = icon("save"),
                              class = "btn-primary btn-lg")
          )
        ),
        br(),
        verbatimTextOutput(ns("save_status"))
      )
    ),
    
    # Preview section
    fluidRow(
      box(
        title = "Configuration Summary",
        width = 12,
        status = "info",
        collapsible = TRUE,
        collapsed = TRUE,
        verbatimTextOutput(ns("config_summary"))
      )
    ),
    
    # Comparison section
    fluidRow(
      box(
        title = "Scenario Comparison",
        width = 12,
        status = "success",
        collapsible = TRUE,
        collapsed = TRUE,
        DT::dataTableOutput(ns("scenario_comparison"))
      )
    )
  )
}