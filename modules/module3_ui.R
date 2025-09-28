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
                             
                             # Three scenarios side by side using helper functions
                             fluidRow(
                               create_scenario_column(ns, "Existing Scenario", "existing"),
                               create_scenario_column(ns, "Alternative Scenario A", "scenario_a"),
                               create_scenario_column(ns, "Alternative Scenario B", "scenario_b")
                             )
                    ),
                    
                    # Tab 2: Business License Configuration - now using helper functions
                    tabPanel("Business License",
                             br(),
                             fluidRow(
                               column(12,
                                      h4("Business License Configuration"),
                                      p("Set license fees for different business subcategories. Choose between minimum tax with rate or flat tax. Optionally use logic slots based on business value or area.")
                               )
                             ),
                             br(),
                             
                             # Three scenarios side by side using helper functions
                             fluidRow(
                               create_business_license_scenario_column(ns, "Existing Scenario", "existing"),
                               create_business_license_scenario_column(ns, "Alternative Scenario A", "scenario_a"),
                               create_business_license_scenario_column(ns, "Alternative Scenario B", "scenario_b")
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