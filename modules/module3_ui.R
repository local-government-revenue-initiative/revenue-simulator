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
          column(
            12,
            p("Configure tax rates and minimums for all three scenarios."),
            actionButton(
              ns("copy_existing_to_a"),
              "Copy Existing → Scenario A",
              icon = icon("copy"),
              class = "btn-info btn-sm"
            ),
            actionButton(
              ns("copy_existing_to_b"),
              "Copy Existing → Scenario B",
              icon = icon("copy"),
              class = "btn-info btn-sm"
            ),
            actionButton(
              ns("copy_a_to_b"),
              "Copy Scenario A → Scenario B",
              icon = icon("copy"),
              class = "btn-info btn-sm"
            ),
            actionButton(
              ns("reset_all"),
              "Reset All to Defaults",
              icon = icon("undo"),
              class = "btn-warning btn-sm"
            )
          )
        ),

        hr(),

        # Tabbed interface
        tabsetPanel(
          id = ns("tax_tabs"),

          # Tab 1: Property Tax Configuration
          tabPanel(
            "Property Tax",
            br(),
            fluidRow(
              column(
                12,
                h4("Property Tax Configuration"),
                p(
                  "Set tax rates and minimums for different property types. Optionally use value-based logic slots for progressive taxation."
                )
              )
            ),
            br(),

            # Three scenarios side by side using helper functions
            fluidRow(
              create_scenario_column(ns, "Existing Scenario", "existing"),
              create_scenario_column(
                ns,
                "Alternative Scenario A",
                "scenario_a"
              ),
              create_scenario_column(ns, "Alternative Scenario B", "scenario_b")
            )
          ),

          # Tab 2: Business License Configuration - now using helper functions
          tabPanel(
            "Business License",
            br(),
            fluidRow(
              column(
                12,
                h4("Business License Configuration"),
                p(
                  "Set license fees for different business subcategories. Choose between minimum tax with rate or flat tax. Optionally use logic slots based on business value or area."
                )
              )
            ),
            br(),

            # Three scenarios side by side using helper functions
            fluidRow(
              create_business_license_scenario_column(
                ns,
                "Existing Scenario",
                "existing"
              ),
              create_business_license_scenario_column(
                ns,
                "Alternative Scenario A",
                "scenario_a"
              ),
              create_business_license_scenario_column(
                ns,
                "Alternative Scenario B",
                "scenario_b"
              )
            )
          ),

          # Tab 3: Preview - Restructured with two sub-tabs
          tabPanel(
            "Preview",
            br(),
            tabsetPanel(
              id = ns("preview_tabs"),

              # Property Tax Preview Sub-tab
              tabPanel(
                "Property Tax Preview",
                br(),
                fluidRow(
                  column(
                    4,
                    selectInput(
                      ns("property_preview_scenario"),
                      "Select Scenario:",
                      choices = c(
                        "Existing" = "existing",
                        "Scenario A" = "scenario_a",
                        "Scenario B" = "scenario_b"
                      )
                    ),
                    numericInput(
                      ns("property_preview_rows"),
                      "Number of rows to preview:",
                      value = 25,
                      min = 1,
                      max = 1000,
                      step = 1
                    ),
                    actionButton(
                      ns("calculate_property_preview"),
                      "Calculate Property Tax Preview",
                      class = "btn-primary"
                    )
                  ),
                  column(
                    8,
                    # Summary info box for property tax
                    conditionalPanel(
                      condition = paste0(
                        "output['",
                        ns("property_preview_summary"),
                        "']"
                      ),
                      div(
                        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                        h5(
                          "Property Tax Preview Summary",
                          style = "margin-top: 0;"
                        ),
                        htmlOutput(ns("property_preview_summary"))
                      )
                    )
                  )
                ),
                br(),
                fluidRow(
                  column(
                    12,
                    div(
                      style = "margin-bottom: 10px;",
                      conditionalPanel(
                        condition = paste0(
                          "output['",
                          ns("property_tax_preview_table"),
                          "']"
                        ),
                        downloadButton(
                          ns("download_property_preview"),
                          "Download Property Tax Preview",
                          class = "btn-success btn-sm",
                          icon = icon("download")
                        )
                      )
                    ),
                    DT::dataTableOutput(ns("property_tax_preview_table"))
                  )
                )
              ),

              # Business License Preview Sub-tab
              tabPanel(
                "Business License Preview",
                br(),
                fluidRow(
                  column(
                    4,
                    selectInput(
                      ns("business_preview_scenario"),
                      "Select Scenario:",
                      choices = c(
                        "Existing" = "existing",
                        "Scenario A" = "scenario_a",
                        "Scenario B" = "scenario_b"
                      )
                    ),
                    numericInput(
                      ns("business_preview_rows"),
                      "Number of rows to preview:",
                      value = 25,
                      min = 1,
                      max = 1000,
                      step = 1
                    ),
                    actionButton(
                      ns("calculate_business_preview"),
                      "Calculate Business License Preview",
                      class = "btn-primary"
                    )
                  ),
                  column(
                    8,
                    # Summary info box for business license
                    conditionalPanel(
                      condition = paste0(
                        "output['",
                        ns("business_preview_summary"),
                        "']"
                      ),
                      div(
                        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                        h5(
                          "Business License Preview Summary",
                          style = "margin-top: 0;"
                        ),
                        htmlOutput(ns("business_preview_summary"))
                      )
                    )
                  )
                ),
                br(),
                fluidRow(
                  column(
                    12,
                    div(
                      style = "margin-bottom: 10px;",
                      conditionalPanel(
                        condition = paste0(
                          "output['",
                          ns("business_license_preview_table"),
                          "']"
                        ),
                        downloadButton(
                          ns("download_business_preview"),
                          "Download Business License Preview",
                          class = "btn-success btn-sm",
                          icon = icon("download")
                        )
                      )
                    ),
                    DT::dataTableOutput(ns("business_license_preview_table"))
                  )
                )
              )
            )
          ),

          # Tab 4: Save/Load Configurations
          tabPanel(
            "Save/Load Configurations",
            br(),
            fluidRow(
              column(
                12,
                h3("Save and Load Tax Configuration Settings"),
                p(
                  "Use these controls to save your current tax rate and minimum settings to JSON files, or load previously saved configurations."
                ),
                p(
                  "Saved configurations include: property tax rates and minimums (including progressive slots if enabled), and all business license parameters."
                ),

                hr()
              )
            ),

            # Three columns for three scenarios
            fluidRow(
              # Existing Scenario
              column(
                4,
                box(
                  title = "Existing Scenario",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,

                  h5("Download Current Configuration"),
                  p(
                    "Save the current Existing Scenario tax settings to a JSON file.",
                    style = "font-size: 12px; color: #666;"
                  ),
                  downloadButton(
                    ns("download_tax_config_existing"),
                    "Download Existing Configuration",
                    icon = icon("download"),
                    class = "btn-success btn-block"
                  ),

                  br(),
                  hr(),
                  br(),

                  h5("Upload Configuration"),
                  p(
                    "Load a previously saved configuration file.",
                    style = "font-size: 12px; color: #666;"
                  ),
                  fileInput(
                    ns("upload_tax_config_existing"),
                    label = NULL,
                    accept = c(".json"),
                    buttonLabel = "Browse...",
                    placeholder = "Select JSON file"
                  ),

                  helpText(
                    icon("info-circle"),
                    " After uploading, all tax parameters will be updated automatically.",
                    style = "color: #666; font-size: 11px;"
                  )
                )
              ),

              # Scenario A
              column(
                4,
                box(
                  title = "Scenario A",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,

                  h5("Download Current Configuration"),
                  p(
                    "Save the current Scenario A tax settings to a JSON file.",
                    style = "font-size: 12px; color: #666;"
                  ),
                  downloadButton(
                    ns("download_tax_config_scenario_a"),
                    "Download Scenario A Configuration",
                    icon = icon("download"),
                    class = "btn-success btn-block"
                  ),

                  br(),
                  hr(),
                  br(),

                  h5("Upload Configuration"),
                  p(
                    "Load a previously saved configuration file.",
                    style = "font-size: 12px; color: #666;"
                  ),
                  fileInput(
                    ns("upload_tax_config_scenario_a"),
                    label = NULL,
                    accept = c(".json"),
                    buttonLabel = "Browse...",
                    placeholder = "Select JSON file"
                  ),

                  helpText(
                    icon("info-circle"),
                    " After uploading, all tax parameters will be updated automatically.",
                    style = "color: #666; font-size: 11px;"
                  )
                )
              ),

              # Scenario B
              column(
                4,
                box(
                  title = "Scenario B",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,

                  h5("Download Current Configuration"),
                  p(
                    "Save the current Scenario B tax settings to a JSON file.",
                    style = "font-size: 12px; color: #666;"
                  ),
                  downloadButton(
                    ns("download_tax_config_scenario_b"),
                    "Download Scenario B Configuration",
                    icon = icon("download"),
                    class = "btn-success btn-block"
                  ),

                  br(),
                  hr(),
                  br(),

                  h5("Upload Configuration"),
                  p(
                    "Load a previously saved configuration file.",
                    style = "font-size: 12px; color: #666;"
                  ),
                  fileInput(
                    ns("upload_tax_config_scenario_b"),
                    label = NULL,
                    accept = c(".json"),
                    buttonLabel = "Browse...",
                    placeholder = "Select JSON file"
                  ),

                  helpText(
                    icon("info-circle"),
                    " After uploading, all tax parameters will be updated automatically.",
                    style = "color: #666; font-size: 11px;"
                  )
                )
              )
            ),

            br(),

            # Usage instructions
            fluidRow(
              column(
                12,
                box(
                  title = "How to Use Save/Load",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,

                  h5("Saving Configurations"),
                  tags$ol(
                    tags$li(
                      "Navigate to the 'Property Tax' and 'Business License' tabs and configure all settings"
                    ),
                    tags$li("Return to this 'Save/Load Configurations' tab"),
                    tags$li(
                      "Click the 'Download' button for the scenario you want to save"
                    ),
                    tags$li(
                      "A JSON file will be downloaded with a timestamped filename (e.g., module3_existing_20250117_143052.json)"
                    ),
                    tags$li(
                      "Store this file in a safe location or share it with colleagues"
                    )
                  ),

                  br(),

                  h5("Loading Configurations"),
                  tags$ol(
                    tags$li(
                      "Click 'Browse...' under the scenario you want to load into"
                    ),
                    tags$li(
                      "Select a previously saved JSON configuration file"
                    ),
                    tags$li(
                      "The system will automatically update all parameters to match the saved configuration"
                    ),
                    tags$li(
                      "A notification will confirm successful loading and show when the configuration was saved"
                    ),
                    tags$li(
                      "Navigate to 'Property Tax' and 'Business License' tabs to see all updated parameters"
                    )
                  ),

                  br(),

                  h5("What Gets Saved"),
                  tags$ul(
                    tags$li(
                      strong("Property Tax:"),
                      " Minimum tax amounts, tax rates, progressive slot settings (if enabled)"
                    ),
                    tags$li(
                      strong("Business License:"),
                      " Tax calculation methods, minimum amounts, tax rates, value bands, area bands"
                    ),
                    tags$li(
                      strong("All Scenarios:"),
                      " Each scenario's settings are saved independently"
                    )
                  ),

                  br(),

                  h5("Tips"),
                  tags$ul(
                    tags$li(
                      strong("Naming:"),
                      " Downloaded files include the scenario name and timestamp for easy identification"
                    ),
                    tags$li(
                      strong("Cloning:"),
                      " To clone Scenario A to Scenario B, download from A and upload to B"
                    ),
                    tags$li(
                      strong("Coordination:"),
                      " Use with Module 2 save/load to maintain complete policy configurations"
                    ),
                    tags$li(
                      strong("Validation:"),
                      " The system validates files to ensure they're Module 3 configurations"
                    ),
                    tags$li(
                      strong("Sharing:"),
                      " JSON files can be emailed or stored in cloud drives for team collaboration"
                    )
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
