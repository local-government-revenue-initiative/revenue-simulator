# modules/module2_ui.R

module2_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "module2",
    fluidRow(
      box(
        title = "Module 2: Parameters to Calculate Property and Business Values",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        
        # Scenario action buttons
        fluidRow(
          column(12,
                 p("Configure parameters for all three scenarios. Changes are saved automatically."),
                 actionButton(ns("copy_existing_to_a"), "Copy Existing → Scenario A", 
                              icon = icon("copy"), class = "btn-info btn-sm"),
                 actionButton(ns("copy_existing_to_b"), "Copy Existing → Scenario B", 
                              icon = icon("copy"), class = "btn-info btn-sm"),
                 actionButton(ns("copy_a_to_b"), "Copy Scenario A → Scenario B", 
                              icon = icon("copy"), class = "btn-info btn-sm"),
                 actionButton(ns("reset_all"), "Reset All to Defaults", 
                              icon = icon("undo"), class = "btn-warning btn-sm")
          )
        ),
        
        hr(),
        
        # Tabbed interface for different parameter groups
        tabsetPanel(id = ns("param_tabs"),
                    
                    # Tab 1: Base Value and Inflation
                    tabPanel("Base Value & Inflation",
                             br(),
                             fluidRow(
                               column(4,
                                      h4("Existing Scenario"),
                                      numericInput(ns("base_value_existing"), 
                                                   label = "Base Value", 
                                                   value = 231.859128,
                                                   step = 0.01),
                                      numericInput(ns("inflation_existing"), 
                                                   label = "Inflation Adjustment %", 
                                                   value = 0,
                                                   step = 0.1),
                                      helpText("0% = no adjustment, 50% = 50% inflation"),
                                      h5("Inflation-Adjusted Base Value:"),
                                      verbatimTextOutput(ns("adjusted_base_existing")),
                                      numericInput(ns("area_weight_existing"), 
                                                   label = "Area Weight", 
                                                   value = 0.5,
                                                   step = 0.01),
                                      helpText("Freetown uses 0.5")
                               ),
                               column(4,
                                      h4("Alternative Scenario A"),
                                      numericInput(ns("base_value_scenario_a"), 
                                                   label = "Base Value", 
                                                   value = 231.859128,
                                                   step = 0.01),
                                      numericInput(ns("inflation_scenario_a"), 
                                                   label = "Inflation Adjustment %", 
                                                   value = 0,
                                                   step = 0.1),
                                      helpText("0% = no adjustment, 50% = 50% inflation"),
                                      h5("Inflation-Adjusted Base Value:"),
                                      verbatimTextOutput(ns("adjusted_base_scenario_a")),
                                      numericInput(ns("area_weight_scenario_a"), 
                                                   label = "Area Weight", 
                                                   value = 0.5,
                                                   step = 0.01),
                                      helpText("Freetown uses 0.5")
                               ),
                               column(4,
                                      h4("Alternative Scenario B"),
                                      numericInput(ns("base_value_scenario_b"), 
                                                   label = "Base Value", 
                                                   value = 231.859128,
                                                   step = 0.01),
                                      numericInput(ns("inflation_scenario_b"), 
                                                   label = "Inflation Adjustment %", 
                                                   value = 0,
                                                   step = 0.1),
                                      helpText("0% = no adjustment, 50% = 50% inflation"),
                                      h5("Inflation-Adjusted Base Value:"),
                                      verbatimTextOutput(ns("adjusted_base_scenario_b")),
                                      numericInput(ns("area_weight_scenario_b"), 
                                                   label = "Area Weight", 
                                                   value = 0.5,
                                                   step = 0.01),
                                      helpText("Freetown uses 0.5")
                               )
                             )
                    ),
                    
                    # Tab 2: Property Feature Weights
                    tabPanel("Property Feature Weights",
                             br(),
                             p("Weights for property features typically range from -250 to 250. Negative weights decrease value, positive weights increase value."),
                             br(),
                             
                             # Dynamic UI for each scenario's features with collapsible sections
                             fluidRow(
                               column(4,
                                      h4("Existing Scenario"),
                                      div(id = ns("feature_sections_existing"), 
                                          style = "max-height: 600px; overflow-y: auto;",
                                          uiOutput(ns("features_ui_existing"))
                                      )
                               ),
                               column(4,
                                      h4("Alternative Scenario A"),
                                      div(id = ns("feature_sections_scenario_a"), 
                                          style = "max-height: 600px; overflow-y: auto;",
                                          uiOutput(ns("features_ui_scenario_a"))
                                      )
                               ),
                               column(4,
                                      h4("Alternative Scenario B"),
                                      div(id = ns("feature_sections_scenario_b"), 
                                          style = "max-height: 600px; overflow-y: auto;",
                                          uiOutput(ns("features_ui_scenario_b"))
                                      )
                               )
                             )
                    ),
                    
                    # Tab 3: Structure Type Weights
                    tabPanel("Structure Type Weights",
                             br(),
                             p("Weights for structure types can range up to 5,000. These weights are applied to commercial and institutional property types."),
                             br(),
                             
                             fluidRow(
                               column(4,
                                      h4("Existing Scenario"),
                                      uiOutput(ns("structure_ui_existing"))
                               ),
                               column(4,
                                      h4("Alternative Scenario A"),
                                      uiOutput(ns("structure_ui_scenario_a"))
                               ),
                               column(4,
                                      h4("Alternative Scenario B"),
                                      uiOutput(ns("structure_ui_scenario_b"))
                               )
                             )
                    ),
                    
                    # Tab 4: Data Preview
                    tabPanel("Data Preview",
                             br(),
                             fluidRow(
                               column(4,
                                      selectInput(ns("preview_scenario"), 
                                                  "Select Scenario to Preview:",
                                                  choices = c("Existing" = "existing",
                                                              "Scenario A" = "scenario_a",
                                                              "Scenario B" = "scenario_b"),
                                                  selected = "existing")
                               ),
                               column(4,
                                      numericInput(ns("preview_rows"), 
                                                   "Number of rows to preview:",
                                                   value = 100,
                                                   min = 10,
                                                   max = 1000,
                                                   step = 10)
                               ),
                               column(4,
                                      br(),
                                      actionButton(ns("calculate_preview"), 
                                                   "Calculate Preview", 
                                                   icon = icon("calculator"),
                                                   class = "btn-primary")
                               )
                             ),
                             br(),
                             DT::dataTableOutput(ns("preview_table"))
                    )
        ),
        
        hr(),
        
        # Configuration summary
        fluidRow(
          column(12,
                 h4("Configuration Summary"),
                 verbatimTextOutput(ns("config_summary"))
          )
        )
      )
    )
  )
}