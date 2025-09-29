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
                    
          # Tab 3: Preview - Restructured with two sub-tabs
          tabPanel("Preview",
                  br(),
                  tabsetPanel(id = ns("preview_tabs"),
                              
                              # Property Tax Preview Sub-tab
                              tabPanel("Property Tax Preview",
                                        br(),
                                        fluidRow(
                                          column(4,
                                                selectInput(ns("property_preview_scenario"),
                                                            "Select Scenario:",
                                                            choices = c("Existing" = "existing",
                                                                        "Scenario A" = "scenario_a", 
                                                                        "Scenario B" = "scenario_b")),
                                                numericInput(ns("property_preview_rows"),
                                                              "Number of rows to preview:",
                                                              value = 25,
                                                              min = 1,
                                                              max = 1000,
                                                              step = 1),
                                                actionButton(ns("calculate_property_preview"),
                                                              "Calculate Property Tax Preview",
                                                              class = "btn-primary")
                                          ),
                                          column(8,
                                                # Summary info box for property tax
                                                conditionalPanel(
                                                  condition = paste0("output['", ns("property_preview_summary"), "']"),
                                                  div(
                                                    style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                                                    h5("Property Tax Preview Summary", style = "margin-top: 0;"),
                                                    htmlOutput(ns("property_preview_summary"))
                                                  )
                                                )
                                          )
                                        ),
                                        br(),
                                        fluidRow(
                                          column(12,
                                                div(
                                                  style = "margin-bottom: 10px;",
                                                  conditionalPanel(
                                                    condition = paste0("output['", ns("property_tax_preview_table"), "']"),
                                                    downloadButton(ns("download_property_preview"), 
                                                                    "Download Property Tax Preview", 
                                                                    class = "btn-success btn-sm",
                                                                    icon = icon("download"))
                                                  )
                                                ),
                                                DT::dataTableOutput(ns("property_tax_preview_table"))
                                          )
                                        )
                              ),
                              
                              # Business License Preview Sub-tab
                              tabPanel("Business License Preview",
                                        br(),
                                        fluidRow(
                                          column(4,
                                                selectInput(ns("business_preview_scenario"),
                                                            "Select Scenario:",
                                                            choices = c("Existing" = "existing",
                                                                        "Scenario A" = "scenario_a", 
                                                                        "Scenario B" = "scenario_b")),
                                                numericInput(ns("business_preview_rows"),
                                                              "Number of rows to preview:",
                                                              value = 25,
                                                              min = 1,
                                                              max = 1000,
                                                              step = 1),
                                                actionButton(ns("calculate_business_preview"),
                                                              "Calculate Business License Preview",
                                                              class = "btn-primary")
                                          ),
                                          column(8,
                                                # Summary info box for business license
                                                conditionalPanel(
                                                  condition = paste0("output['", ns("business_preview_summary"), "']"),
                                                  div(
                                                    style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                                                    h5("Business License Preview Summary", style = "margin-top: 0;"),
                                                    htmlOutput(ns("business_preview_summary"))
                                                  )
                                                )
                                          )
                                        ),
                                        br(),
                                        fluidRow(
                                          column(12,
                                                div(
                                                  style = "margin-bottom: 10px;",
                                                  conditionalPanel(
                                                    condition = paste0("output['", ns("business_license_preview_table"), "']"),
                                                    downloadButton(ns("download_business_preview"), 
                                                                    "Download Business License Preview", 
                                                                    class = "btn-success btn-sm",
                                                                    icon = icon("download"))
                                                  )
                                                ),
                                                DT::dataTableOutput(ns("business_license_preview_table"))
                                          )
                                        )
                              )
                  )
          )
        )
      )
    )
  )
}