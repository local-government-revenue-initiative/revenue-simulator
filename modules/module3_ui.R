# modules/module3_ui.R

module3_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "module3",
    fluidRow(
      box(
        title = "Module 3: Tax Rates and Minimums Configuration",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        
        # Action buttons
        fluidRow(
          column(12,
                 p("Configure tax rates and minimums for all three scenarios."),
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
        
        # Tabbed interface
        tabsetPanel(id = ns("tax_tabs"),
                    
                    # Tab 1: Property Tax Configuration 
                    tabPanel("Property Tax",
                             br(),
                             fluidRow(
                               column(12,
                                      h4("Property Tax Configuration"),
                                      p("Set tax rates and minimums for different property types. Optionally use value-based logic slots for progressive taxation.")
                               )
                             ),
                             br(),
                             
                             # Three scenarios side by side
                             fluidRow(
                               # Existing Scenario------------------------------
                               column(4,
                                      h4("Existing Scenario"),
                                      
                                      # Domestic Properties
                                      wellPanel(
                                        h5("Domestic Properties"),
                                        checkboxInput(ns("use_slots_domestic_existing"),
                                                      "Use value-based logic slots",
                                                      value = FALSE),
                                        
                                        conditionalPanel(
                                          condition = paste0("!input['", ns("use_slots_domestic_existing"), "']"),
                                          # Simple configuration
                                          fluidRow(
                                            column(6,
                                                   numericInput(ns("domestic_min_existing"),
                                                                "Minimum Tax:",
                                                                value = 200,
                                                                min = 0)),
                                            column(6,
                                                   numericInput(ns("domestic_rate_existing"),
                                                                "Tax Rate (%):",
                                                                value = 2.5,
                                                                min = 0,
                                                                step = 0.1))
                                          )
                                        ),
                                        
                                        conditionalPanel(
                                          condition = paste0("input['", ns("use_slots_domestic_existing"), "']"),
                                          # Logic slots configuration
                                          h6("Logic Slot Ranges:"),
                                          fluidRow(
                                            column(4, p("Slot 1:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("domestic_slot1_min_existing"), "Min:", value = 0, min = 0)),
                                            column(4, numericInput(ns("domestic_slot1_max_existing"), "Max:", value = 350, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 2:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("domestic_slot2_min_existing"), "Min:", value = 350, min = 0)),
                                            column(4, numericInput(ns("domestic_slot2_max_existing"), "Max:", value = 700, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 3:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("domestic_slot3_min_existing"), "Min:", value = 700, min = 0)),
                                            column(4, p("Max: No limit", style = "padding-top: 25px;"))
                                          ),
                                          hr(),
                                          h6("Tax Configuration per Slot:"),
                                          # Slot 1
                                          p("Logic Slot 1:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("domestic_slot1_min_tax_existing"), "Min Tax:", value = 100, min = 0)),
                                            column(6, numericInput(ns("domestic_slot1_rate_existing"), "Rate (%):", value = 2.0, min = 0, step = 0.1))
                                          ),
                                          # Slot 2
                                          p("Logic Slot 2:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("domestic_slot2_min_tax_existing"), "Min Tax:", value = 200, min = 0)),
                                            column(6, numericInput(ns("domestic_slot2_rate_existing"), "Rate (%):", value = 2.5, min = 0, step = 0.1))
                                          ),
                                          # Slot 3
                                          p("Logic Slot 3:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("domestic_slot3_min_tax_existing"), "Min Tax:", value = 300, min = 0)),
                                            column(6, numericInput(ns("domestic_slot3_rate_existing"), "Rate (%):", value = 3.0, min = 0, step = 0.1))
                                          )
                                        )
                                      ),
                                      
                                      # Commercial Properties
                                      wellPanel(
                                        h5("Commercial Properties"),
                                        checkboxInput(ns("use_slots_commercial_existing"),
                                                      "Use value-based logic slots",
                                                      value = FALSE),
                                        
                                        conditionalPanel(
                                          condition = paste0("!input['", ns("use_slots_commercial_existing"), "']"),
                                          # Simple configuration
                                          fluidRow(
                                            column(6,
                                                   numericInput(ns("commercial_min_existing"),
                                                                "Minimum Tax:",
                                                                value = 200,
                                                                min = 0)),
                                            column(6,
                                                   numericInput(ns("commercial_rate_existing"),
                                                                "Tax Rate (%):",
                                                                value = 4,
                                                                min = 0,
                                                                step = 0.1))
                                          )
                                        ),
                                        
                                        conditionalPanel(
                                          condition = paste0("input['", ns("use_slots_commercial_existing"), "']"),
                                          # Logic slots configuration
                                          h6("Logic Slot Ranges:"),
                                          fluidRow(
                                            column(4, p("Slot 1:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("commercial_slot1_min_existing"), "Min:", value = 0, min = 0)),
                                            column(4, numericInput(ns("commercial_slot1_max_existing"), "Max:", value = 350, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 2:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("commercial_slot2_min_existing"), "Min:", value = 350, min = 0)),
                                            column(4, numericInput(ns("commercial_slot2_max_existing"), "Max:", value = 700, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 3:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("commercial_slot3_min_existing"), "Min:", value = 700, min = 0)),
                                            column(4, p("Max: No limit", style = "padding-top: 25px;"))
                                          ),
                                          hr(),
                                          h6("Tax Configuration per Slot:"),
                                          # Slot 1
                                          p("Logic Slot 1:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("commercial_slot1_min_tax_existing"), "Min Tax:", value = 150, min = 0)),
                                            column(6, numericInput(ns("commercial_slot1_rate_existing"), "Rate (%):", value = 3.0, min = 0, step = 0.1))
                                          ),
                                          # Slot 2
                                          p("Logic Slot 2:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("commercial_slot2_min_tax_existing"), "Min Tax:", value = 200, min = 0)),
                                            column(6, numericInput(ns("commercial_slot2_rate_existing"), "Rate (%):", value = 4.0, min = 0, step = 0.1))
                                          ),
                                          # Slot 3
                                          p("Logic Slot 3:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("commercial_slot3_min_tax_existing"), "Min Tax:", value = 400, min = 0)),
                                            column(6, numericInput(ns("commercial_slot3_rate_existing"), "Rate (%):", value = 5.0, min = 0, step = 0.1))
                                          )
                                        )
                                      ),
                                      
                                      # Institutional Properties
                                      wellPanel(
                                        h5("Institutional Properties"),
                                        checkboxInput(ns("use_slots_institutional_existing"),
                                                      "Use value-based logic slots",
                                                      value = FALSE),
                                        
                                        conditionalPanel(
                                          condition = paste0("!input['", ns("use_slots_institutional_existing"), "']"),
                                          # Simple configuration
                                          fluidRow(
                                            column(6,
                                                   numericInput(ns("institutional_min_existing"),
                                                                "Minimum Tax:",
                                                                value = 200,
                                                                min = 0)),
                                            column(6,
                                                   numericInput(ns("institutional_rate_existing"),
                                                                "Tax Rate (%):",
                                                                value = 2.5,
                                                                min = 0,
                                                                step = 0.1))
                                          )
                                        ),
                                        
                                        conditionalPanel(
                                          condition = paste0("input['", ns("use_slots_institutional_existing"), "']"),
                                          # Logic slots configuration
                                          h6("Logic Slot Ranges:"),
                                          fluidRow(
                                            column(4, p("Slot 1:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("institutional_slot1_min_existing"), "Min:", value = 0, min = 0)),
                                            column(4, numericInput(ns("institutional_slot1_max_existing"), "Max:", value = 350, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 2:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("institutional_slot2_min_existing"), "Min:", value = 350, min = 0)),
                                            column(4, numericInput(ns("institutional_slot2_max_existing"), "Max:", value = 700, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 3:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("institutional_slot3_min_existing"), "Min:", value = 700, min = 0)),
                                            column(4, p("Max: No limit", style = "padding-top: 25px;"))
                                          ),
                                          hr(),
                                          h6("Tax Configuration per Slot:"),
                                          # Slot 1
                                          p("Logic Slot 1:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("institutional_slot1_min_tax_existing"), "Min Tax:", value = 100, min = 0)),
                                            column(6, numericInput(ns("institutional_slot1_rate_existing"), "Rate (%):", value = 2.0, min = 0, step = 0.1))
                                          ),
                                          # Slot 2
                                          p("Logic Slot 2:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("institutional_slot2_min_tax_existing"), "Min Tax:", value = 200, min = 0)),
                                            column(6, numericInput(ns("institutional_slot2_rate_existing"), "Rate (%):", value = 2.5, min = 0, step = 0.1))
                                          ),
                                          # Slot 3
                                          p("Logic Slot 3:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("institutional_slot3_min_tax_existing"), "Min Tax:", value = 300, min = 0)),
                                            column(6, numericInput(ns("institutional_slot3_rate_existing"), "Rate (%):", value = 3.0, min = 0, step = 0.1))
                                          )
                                        )
                                      )
                               ),
                               
                               # Scenario A - Similar structure-----------------
                               column(4,
                                      h4("Alternative Scenario A"),
                                      
                                      # Domestic Properties
                                      wellPanel(
                                        h5("Domestic Properties"),
                                        checkboxInput(ns("use_slots_domestic_scenario_a"),
                                                      "Use value-based logic slots",
                                                      value = FALSE),
                                        
                                        conditionalPanel(
                                          condition = paste0("!input['", ns("use_slots_domestic_scenario_a"), "']"),
                                          # Simple configuration
                                          fluidRow(
                                            column(6,
                                                   numericInput(ns("domestic_min_scenario_a"),
                                                                "Minimum Tax:",
                                                                value = 200,
                                                                min = 0)),
                                            column(6,
                                                   numericInput(ns("domestic_rate_scenario_a"),
                                                                "Tax Rate (%):",
                                                                value = 2.5,
                                                                min = 0,
                                                                step = 0.1))
                                          )
                                        ),
                                        
                                        conditionalPanel(
                                          condition = paste0("input['", ns("use_slots_domestic_scenario_a"), "']"),
                                          # Logic slots configuration
                                          h6("Logic Slot Ranges:"),
                                          fluidRow(
                                            column(4, p("Slot 1:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("domestic_slot1_min_scenario_a"), "Min:", value = 0, min = 0)),
                                            column(4, numericInput(ns("domestic_slot1_max_scenario_a"), "Max:", value = 350, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 2:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("domestic_slot2_min_scenario_a"), "Min:", value = 350, min = 0)),
                                            column(4, numericInput(ns("domestic_slot2_max_scenario_a"), "Max:", value = 700, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 3:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("domestic_slot3_min_scenario_a"), "Min:", value = 700, min = 0)),
                                            column(4, p("Max: No limit", style = "padding-top: 25px;"))
                                          ),
                                          hr(),
                                          h6("Tax Configuration per Slot:"),
                                          # Slot 1
                                          p("Logic Slot 1:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("domestic_slot1_min_tax_scenario_a"), "Min Tax:", value = 100, min = 0)),
                                            column(6, numericInput(ns("domestic_slot1_rate_scenario_a"), "Rate (%):", value = 2.0, min = 0, step = 0.1))
                                          ),
                                          # Slot 2
                                          p("Logic Slot 2:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("domestic_slot2_min_tax_scenario_a"), "Min Tax:", value = 200, min = 0)),
                                            column(6, numericInput(ns("domestic_slot2_rate_scenario_a"), "Rate (%):", value = 2.5, min = 0, step = 0.1))
                                          ),
                                          # Slot 3
                                          p("Logic Slot 3:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("domestic_slot3_min_tax_scenario_a"), "Min Tax:", value = 300, min = 0)),
                                            column(6, numericInput(ns("domestic_slot3_rate_scenario_a"), "Rate (%):", value = 3.0, min = 0, step = 0.1))
                                          )
                                        )
                                      ),
                                      
                                      # Commercial Properties
                                      wellPanel(
                                        h5("Commercial Properties"),
                                        checkboxInput(ns("use_slots_commercial_scenario_a"),
                                                      "Use value-based logic slots",
                                                      value = FALSE),
                                        
                                        conditionalPanel(
                                          condition = paste0("!input['", ns("use_slots_commercial_scenario_a"), "']"),
                                          # Simple configuration
                                          fluidRow(
                                            column(6,
                                                   numericInput(ns("commercial_min_scenario_a"),
                                                                "Minimum Tax:",
                                                                value = 200,
                                                                min = 0)),
                                            column(6,
                                                   numericInput(ns("commercial_rate_scenario_a"),
                                                                "Tax Rate (%):",
                                                                value = 4,
                                                                min = 0,
                                                                step = 0.1))
                                          )
                                        ),
                                        
                                        conditionalPanel(
                                          condition = paste0("input['", ns("use_slots_commercial_scenario_a"), "']"),
                                          # Logic slots configuration
                                          h6("Logic Slot Ranges:"),
                                          fluidRow(
                                            column(4, p("Slot 1:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("commercial_slot1_min_scenario_a"), "Min:", value = 0, min = 0)),
                                            column(4, numericInput(ns("commercial_slot1_max_scenario_a"), "Max:", value = 350, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 2:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("commercial_slot2_min_scenario_a"), "Min:", value = 350, min = 0)),
                                            column(4, numericInput(ns("commercial_slot2_max_scenario_a"), "Max:", value = 700, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 3:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("commercial_slot3_min_scenario_a"), "Min:", value = 700, min = 0)),
                                            column(4, p("Max: No limit", style = "padding-top: 25px;"))
                                          ),
                                          hr(),
                                          h6("Tax Configuration per Slot:"),
                                          # Slot 1
                                          p("Logic Slot 1:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("commercial_slot1_min_tax_scenario_a"), "Min Tax:", value = 150, min = 0)),
                                            column(6, numericInput(ns("commercial_slot1_rate_scenario_a"), "Rate (%):", value = 3.0, min = 0, step = 0.1))
                                          ),
                                          # Slot 2
                                          p("Logic Slot 2:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("commercial_slot2_min_tax_scenario_a"), "Min Tax:", value = 200, min = 0)),
                                            column(6, numericInput(ns("commercial_slot2_rate_scenario_a"), "Rate (%):", value = 4.0, min = 0, step = 0.1))
                                          ),
                                          # Slot 3
                                          p("Logic Slot 3:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("commercial_slot3_min_tax_scenario_a"), "Min Tax:", value = 400, min = 0)),
                                            column(6, numericInput(ns("commercial_slot3_rate_scenario_a"), "Rate (%):", value = 5.0, min = 0, step = 0.1))
                                          )
                                        )
                                      ),
                                      
                                      # Institutional Properties
                                      wellPanel(
                                        h5("Institutional Properties"),
                                        checkboxInput(ns("use_slots_institutional_scenario_a"),
                                                      "Use value-based logic slots",
                                                      value = FALSE),
                                        
                                        conditionalPanel(
                                          condition = paste0("!input['", ns("use_slots_institutional_scenario_a"), "']"),
                                          # Simple configuration
                                          fluidRow(
                                            column(6,
                                                   numericInput(ns("institutional_min_scenario_a"),
                                                                "Minimum Tax:",
                                                                value = 200,
                                                                min = 0)),
                                            column(6,
                                                   numericInput(ns("institutional_rate_scenario_a"),
                                                                "Tax Rate (%):",
                                                                value = 2.5,
                                                                min = 0,
                                                                step = 0.1))
                                          )
                                        ),
                                        
                                        conditionalPanel(
                                          condition = paste0("input['", ns("use_slots_institutional_scenario_a"), "']"),
                                          # Logic slots configuration
                                          h6("Logic Slot Ranges:"),
                                          fluidRow(
                                            column(4, p("Slot 1:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("institutional_slot1_min_scenario_a"), "Min:", value = 0, min = 0)),
                                            column(4, numericInput(ns("institutional_slot1_max_scenario_a"), "Max:", value = 350, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 2:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("institutional_slot2_min_scenario_a"), "Min:", value = 350, min = 0)),
                                            column(4, numericInput(ns("institutional_slot2_max_scenario_a"), "Max:", value = 700, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 3:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("institutional_slot3_min_scenario_a"), "Min:", value = 700, min = 0)),
                                            column(4, p("Max: No limit", style = "padding-top: 25px;"))
                                          ),
                                          hr(),
                                          h6("Tax Configuration per Slot:"),
                                          # Slot 1
                                          p("Logic Slot 1:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("institutional_slot1_min_tax_scenario_a"), "Min Tax:", value = 100, min = 0)),
                                            column(6, numericInput(ns("institutional_slot1_rate_scenario_a"), "Rate (%):", value = 2.0, min = 0, step = 0.1))
                                          ),
                                          # Slot 2
                                          p("Logic Slot 2:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("institutional_slot2_min_tax_scenario_a"), "Min Tax:", value = 200, min = 0)),
                                            column(6, numericInput(ns("institutional_slot2_rate_scenario_a"), "Rate (%):", value = 2.5, min = 0, step = 0.1))
                                          ),
                                          # Slot 3
                                          p("Logic Slot 3:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("institutional_slot3_min_tax_scenario_a"), "Min Tax:", value = 300, min = 0)),
                                            column(6, numericInput(ns("institutional_slot3_rate_scenario_a"), "Rate (%):", value = 3.0, min = 0, step = 0.1))
                                          )
                                        )
                                      )
                               ),
                               
                               # Scenario B - Similar structure-----------------
                               column(4,
                                      h4("Alternative Scenario B"),
                                      
                                      # Domestic Properties
                                      wellPanel(
                                        h5("Domestic Properties"),
                                        checkboxInput(ns("use_slots_domestic_scenario_b"),
                                                      "Use value-based logic slots",
                                                      value = FALSE),
                                        
                                        conditionalPanel(
                                          condition = paste0("!input['", ns("use_slots_domestic_scenario_b"), "']"),
                                          # Simple configuration
                                          fluidRow(
                                            column(6,
                                                   numericInput(ns("domestic_min_scenario_b"),
                                                                "Minimum Tax:",
                                                                value = 200,
                                                                min = 0)),
                                            column(6,
                                                   numericInput(ns("domestic_rate_scenario_b"),
                                                                "Tax Rate (%):",
                                                                value = 2.5,
                                                                min = 0,
                                                                step = 0.1))
                                          )
                                        ),
                                        
                                        conditionalPanel(
                                          condition = paste0("input['", ns("use_slots_domestic_scenario_b"), "']"),
                                          # Logic slots configuration
                                          h6("Logic Slot Ranges:"),
                                          fluidRow(
                                            column(4, p("Slot 1:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("domestic_slot1_min_scenario_b"), "Min:", value = 0, min = 0)),
                                            column(4, numericInput(ns("domestic_slot1_max_scenario_b"), "Max:", value = 350, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 2:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("domestic_slot2_min_scenario_b"), "Min:", value = 350, min = 0)),
                                            column(4, numericInput(ns("domestic_slot2_max_scenario_b"), "Max:", value = 700, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 3:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("domestic_slot3_min_scenario_b"), "Min:", value = 700, min = 0)),
                                            column(4, p("Max: No limit", style = "padding-top: 25px;"))
                                          ),
                                          hr(),
                                          h6("Tax Configuration per Slot:"),
                                          # Slot 1
                                          p("Logic Slot 1:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("domestic_slot1_min_tax_scenario_b"), "Min Tax:", value = 100, min = 0)),
                                            column(6, numericInput(ns("domestic_slot1_rate_scenario_b"), "Rate (%):", value = 2.0, min = 0, step = 0.1))
                                          ),
                                          # Slot 2
                                          p("Logic Slot 2:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("domestic_slot2_min_tax_scenario_b"), "Min Tax:", value = 200, min = 0)),
                                            column(6, numericInput(ns("domestic_slot2_rate_scenario_b"), "Rate (%):", value = 2.5, min = 0, step = 0.1))
                                          ),
                                          # Slot 3
                                          p("Logic Slot 3:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("domestic_slot3_min_tax_scenario_b"), "Min Tax:", value = 300, min = 0)),
                                            column(6, numericInput(ns("domestic_slot3_rate_scenario_b"), "Rate (%):", value = 3.0, min = 0, step = 0.1))
                                          )
                                        )
                                      ),
                                      
                                      # Commercial Properties
                                      wellPanel(
                                        h5("Commercial Properties"),
                                        checkboxInput(ns("use_slots_commercial_scenario_b"),
                                                      "Use value-based logic slots",
                                                      value = FALSE),
                                        
                                        conditionalPanel(
                                          condition = paste0("!input['", ns("use_slots_commercial_scenario_b"), "']"),
                                          # Simple configuration
                                          fluidRow(
                                            column(6,
                                                   numericInput(ns("commercial_min_scenario_b"),
                                                                "Minimum Tax:",
                                                                value = 200,
                                                                min = 0)),
                                            column(6,
                                                   numericInput(ns("commercial_rate_scenario_b"),
                                                                "Tax Rate (%):",
                                                                value = 4,
                                                                min = 0,
                                                                step = 0.1))
                                          )
                                        ),
                                        
                                        conditionalPanel(
                                          condition = paste0("input['", ns("use_slots_commercial_scenario_b"), "']"),
                                          # Logic slots configuration
                                          h6("Logic Slot Ranges:"),
                                          fluidRow(
                                            column(4, p("Slot 1:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("commercial_slot1_min_scenario_b"), "Min:", value = 0, min = 0)),
                                            column(4, numericInput(ns("commercial_slot1_max_scenario_b"), "Max:", value = 350, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 2:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("commercial_slot2_min_scenario_b"), "Min:", value = 350, min = 0)),
                                            column(4, numericInput(ns("commercial_slot2_max_scenario_b"), "Max:", value = 700, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 3:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("commercial_slot3_min_scenario_b"), "Min:", value = 700, min = 0)),
                                            column(4, p("Max: No limit", style = "padding-top: 25px;"))
                                          ),
                                          hr(),
                                          h6("Tax Configuration per Slot:"),
                                          # Slot 1
                                          p("Logic Slot 1:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("commercial_slot1_min_tax_scenario_b"), "Min Tax:", value = 150, min = 0)),
                                            column(6, numericInput(ns("commercial_slot1_rate_scenario_b"), "Rate (%):", value = 3.0, min = 0, step = 0.1))
                                          ),
                                          # Slot 2
                                          p("Logic Slot 2:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("commercial_slot2_min_tax_scenario_b"), "Min Tax:", value = 200, min = 0)),
                                            column(6, numericInput(ns("commercial_slot2_rate_scenario_b"), "Rate (%):", value = 4.0, min = 0, step = 0.1))
                                          ),
                                          # Slot 3
                                          p("Logic Slot 3:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("commercial_slot3_min_tax_scenario_b"), "Min Tax:", value = 400, min = 0)),
                                            column(6, numericInput(ns("commercial_slot3_rate_scenario_b"), "Rate (%):", value = 5.0, min = 0, step = 0.1))
                                          )
                                        )
                                      ),
                                      
                                      # Institutional Properties
                                      wellPanel(
                                        h5("Institutional Properties"),
                                        checkboxInput(ns("use_slots_institutional_scenario_b"),
                                                      "Use value-based logic slots",
                                                      value = FALSE),
                                        
                                        conditionalPanel(
                                          condition = paste0("!input['", ns("use_slots_institutional_scenario_b"), "']"),
                                          # Simple configuration
                                          fluidRow(
                                            column(6,
                                                   numericInput(ns("institutional_min_scenario_b"),
                                                                "Minimum Tax:",
                                                                value = 200,
                                                                min = 0)),
                                            column(6,
                                                   numericInput(ns("institutional_rate_scenario_b"),
                                                                "Tax Rate (%):",
                                                                value = 2.5,
                                                                min = 0,
                                                                step = 0.1))
                                          )
                                        ),
                                        
                                        conditionalPanel(
                                          condition = paste0("input['", ns("use_slots_institutional_scenario_b"), "']"),
                                          # Logic slots configuration
                                          h6("Logic Slot Ranges:"),
                                          fluidRow(
                                            column(4, p("Slot 1:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("institutional_slot1_min_scenario_b"), "Min:", value = 0, min = 0)),
                                            column(4, numericInput(ns("institutional_slot1_max_scenario_b"), "Max:", value = 350, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 2:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("institutional_slot2_min_scenario_b"), "Min:", value = 350, min = 0)),
                                            column(4, numericInput(ns("institutional_slot2_max_scenario_b"), "Max:", value = 700, min = 0))
                                          ),
                                          fluidRow(
                                            column(4, p("Slot 3:", style = "font-weight: bold;")),
                                            column(4, numericInput(ns("institutional_slot3_min_scenario_b"), "Min:", value = 700, min = 0)),
                                            column(4, p("Max: No limit", style = "padding-top: 25px;"))
                                          ),
                                          hr(),
                                          h6("Tax Configuration per Slot:"),
                                          # Slot 1
                                          p("Logic Slot 1:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("institutional_slot1_min_tax_scenario_b"), "Min Tax:", value = 100, min = 0)),
                                            column(6, numericInput(ns("institutional_slot1_rate_scenario_b"), "Rate (%):", value = 2.0, min = 0, step = 0.1))
                                          ),
                                          # Slot 2
                                          p("Logic Slot 2:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("institutional_slot2_min_tax_scenario_b"), "Min Tax:", value = 200, min = 0)),
                                            column(6, numericInput(ns("institutional_slot2_rate_scenario_b"), "Rate (%):", value = 2.5, min = 0, step = 0.1))
                                          ),
                                          # Slot 3
                                          p("Logic Slot 3:", style = "font-weight: bold;"),
                                          fluidRow(
                                            column(6, numericInput(ns("institutional_slot3_min_tax_scenario_b"), "Min Tax:", value = 300, min = 0)),
                                            column(6, numericInput(ns("institutional_slot3_rate_scenario_b"), "Rate (%):", value = 3.0, min = 0, step = 0.1))
                                          )
                                        )
                                      )
                               )
                             )
                    ),
                    
                    # Tab 2: Business License Configuration---------------------
                    tabPanel("Business License",
                             br(),
                             fluidRow(
                               column(12,
                                      h4("Business License Configuration"),
                                      p("Set license fees for different business subcategories. Choose between minimum tax with rate or flat tax. Optionally use logic slots based on business value or area.")
                               )
                             ),
                             br(),
                             
                             # Three scenarios side by side
                             fluidRow(
                               # Existing Scenario
                               column(4,
                                      h4("Existing Scenario"),
                                      
                                      # Business subcategories configuration
                                      h5("Business Subcategories"),
                                      div(style = "max-height: 600px; overflow-y: auto;",
                                          uiOutput(ns("business_subcategories_existing"))
                                      )
                               ),
                               
                               # Scenario A
                               column(4,
                                      h4("Alternative Scenario A"),
                                      
                                      # Business subcategories configuration
                                      h5("Business Subcategories"),
                                      div(style = "max-height: 600px; overflow-y: auto;",
                                          uiOutput(ns("business_subcategories_scenario_a"))
                                      )
                               ),
                               
                               # Scenario B
                               column(4,
                                      h4("Alternative Scenario B"),
                                      
                                      # Business subcategories configuration
                                      h5("Business Subcategories"),
                                      div(style = "max-height: 600px; overflow-y: auto;",
                                          uiOutput(ns("business_subcategories_scenario_b"))
                                      )
                               )
                             )
                    ),
                    
                    # Tab 3: Preview
                    tabPanel("Preview",
                             br(),
                             fluidRow(
                               column(4,
                                      selectInput(ns("preview_scenario"),
                                                  "Select Scenario:",
                                                  choices = c("Existing" = "existing",
                                                              "Scenario A" = "scenario_a",
                                                              "Scenario B" = "scenario_b")),
                                      actionButton(ns("calculate_preview"),
                                                   "Calculate Tax Preview",
                                                   class = "btn-primary")
                               )
                             ),
                             br(),
                             DT::dataTableOutput(ns("tax_preview_table"))
                    )
        )
      )
    )
  )
}