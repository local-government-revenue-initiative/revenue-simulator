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
                                   column(
                                          12,
                                          p(
                                                 "Configure parameters for all three scenarios. Changes are saved automatically."
                                          ),
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

                            # Tabbed interface for different parameter groups
                            tabsetPanel(
                                   id = ns("param_tabs"),

                                   # Tab 1: Base Value and Inflation
                                   tabPanel(
                                          "Base Value & Inflation",
                                          br(),
                                          fluidRow(
                                                 column(
                                                        4,
                                                        h4("Existing Scenario"),
                                                        numericInput(
                                                               ns(
                                                                      "base_value_existing"
                                                               ),
                                                               label = "Base Value",
                                                               value = 231.859128,
                                                               step = 0.01
                                                        ),
                                                        numericInput(
                                                               ns(
                                                                      "inflation_existing"
                                                               ),
                                                               label = "Inflation Adjustment %",
                                                               value = 0,
                                                               step = 0.1
                                                        ),
                                                        helpText(
                                                               "0% = no adjustment, 50% = 50% inflation"
                                                        ),
                                                        h5(
                                                               "Inflation-Adjusted Base Value:"
                                                        ),
                                                        verbatimTextOutput(ns(
                                                               "adjusted_base_existing"
                                                        )),
                                                        numericInput(
                                                               ns(
                                                                      "area_weight_existing"
                                                               ),
                                                               label = "Area Weight",
                                                               value = 0.5,
                                                               step = 0.01
                                                        ),
                                                        helpText(
                                                               "Freetown uses 0.5"
                                                        )
                                                 ),
                                                 column(
                                                        4,
                                                        h4(
                                                               "Alternative Scenario A"
                                                        ),
                                                        numericInput(
                                                               ns(
                                                                      "base_value_scenario_a"
                                                               ),
                                                               label = "Base Value",
                                                               value = 231.859128,
                                                               step = 0.01
                                                        ),
                                                        numericInput(
                                                               ns(
                                                                      "inflation_scenario_a"
                                                               ),
                                                               label = "Inflation Adjustment %",
                                                               value = 0,
                                                               step = 0.1
                                                        ),
                                                        helpText(
                                                               "0% = no adjustment, 50% = 50% inflation"
                                                        ),
                                                        h5(
                                                               "Inflation-Adjusted Base Value:"
                                                        ),
                                                        verbatimTextOutput(ns(
                                                               "adjusted_base_scenario_a"
                                                        )),
                                                        numericInput(
                                                               ns(
                                                                      "area_weight_scenario_a"
                                                               ),
                                                               label = "Area Weight",
                                                               value = 0.5,
                                                               step = 0.01
                                                        ),
                                                        helpText(
                                                               "Freetown uses 0.5"
                                                        )
                                                 ),
                                                 column(
                                                        4,
                                                        h4(
                                                               "Alternative Scenario B"
                                                        ),
                                                        numericInput(
                                                               ns(
                                                                      "base_value_scenario_b"
                                                               ),
                                                               label = "Base Value",
                                                               value = 231.859128,
                                                               step = 0.01
                                                        ),
                                                        numericInput(
                                                               ns(
                                                                      "inflation_scenario_b"
                                                               ),
                                                               label = "Inflation Adjustment %",
                                                               value = 0,
                                                               step = 0.1
                                                        ),
                                                        helpText(
                                                               "0% = no adjustment, 50% = 50% inflation"
                                                        ),
                                                        h5(
                                                               "Inflation-Adjusted Base Value:"
                                                        ),
                                                        verbatimTextOutput(ns(
                                                               "adjusted_base_scenario_b"
                                                        )),
                                                        numericInput(
                                                               ns(
                                                                      "area_weight_scenario_b"
                                                               ),
                                                               label = "Area Weight",
                                                               value = 0.5,
                                                               step = 0.01
                                                        ),
                                                        helpText(
                                                               "Freetown uses 0.5"
                                                        )
                                                 )
                                          )
                                   ),

                                   # Tab 2: Property Feature Weights
                                   tabPanel(
                                          "Property Feature Weights",
                                          br(),
                                          p(
                                                 "Weights for property features typically range from -250 to 250. Negative weights decrease value, positive weights increase value."
                                          ),
                                          br(),

                                          # Dynamic UI for each scenario's features with collapsible sections
                                          fluidRow(
                                                 column(
                                                        4,
                                                        h4("Existing Scenario"),
                                                        div(
                                                               id = ns(
                                                                      "feature_sections_existing"
                                                               ),
                                                               style = "max-height: 600px; overflow-y: auto;",
                                                               uiOutput(ns(
                                                                      "features_ui_existing"
                                                               ))
                                                        )
                                                 ),
                                                 column(
                                                        4,
                                                        h4(
                                                               "Alternative Scenario A"
                                                        ),
                                                        div(
                                                               id = ns(
                                                                      "feature_sections_scenario_a"
                                                               ),
                                                               style = "max-height: 600px; overflow-y: auto;",
                                                               uiOutput(ns(
                                                                      "features_ui_scenario_a"
                                                               ))
                                                        )
                                                 ),
                                                 column(
                                                        4,
                                                        h4(
                                                               "Alternative Scenario B"
                                                        ),
                                                        div(
                                                               id = ns(
                                                                      "feature_sections_scenario_b"
                                                               ),
                                                               style = "max-height: 600px; overflow-y: auto;",
                                                               uiOutput(ns(
                                                                      "features_ui_scenario_b"
                                                               ))
                                                        )
                                                 )
                                          )
                                   ),

                                   # Tab 3: Structure Type Weights
                                   tabPanel(
                                          "Structure Type Weights",
                                          br(),
                                          p(
                                                 "Weights for structure types can range up to 5,000. These weights are applied to commercial and institutional property types."
                                          ),
                                          br(),

                                          fluidRow(
                                                 column(
                                                        4,
                                                        h4("Existing Scenario"),
                                                        uiOutput(ns(
                                                               "structure_ui_existing"
                                                        ))
                                                 ),
                                                 column(
                                                        4,
                                                        h4(
                                                               "Alternative Scenario A"
                                                        ),
                                                        uiOutput(ns(
                                                               "structure_ui_scenario_a"
                                                        ))
                                                 ),
                                                 column(
                                                        4,
                                                        h4(
                                                               "Alternative Scenario B"
                                                        ),
                                                        uiOutput(ns(
                                                               "structure_ui_scenario_b"
                                                        ))
                                                 )
                                          )
                                   ),

                                   # Tab 4: Data Preview
                                   tabPanel(
                                          "Data Preview",
                                          br(),
                                          fluidRow(
                                                 column(
                                                        4,
                                                        selectInput(
                                                               ns(
                                                                      "preview_scenario"
                                                               ),
                                                               "Select Scenario to Preview:",
                                                               choices = c(
                                                                      "Existing" = "existing",
                                                                      "Scenario A" = "scenario_a",
                                                                      "Scenario B" = "scenario_b"
                                                               ),
                                                               selected = "existing"
                                                        )
                                                 ),
                                                 column(
                                                        4,
                                                        br(),
                                                        helpText(
                                                               icon(
                                                                      "info-circle"
                                                               ),
                                                               "Preview shows ALL properties. Use column filters to search specific subsets."
                                                        )
                                                 ),
                                                 column(
                                                        4,
                                                        br(),
                                                        actionButton(
                                                               ns(
                                                                      "calculate_preview"
                                                               ),
                                                               "Calculate Preview",
                                                               icon = icon(
                                                                      "calculator"
                                                               ),
                                                               class = "btn-primary"
                                                        )
                                                 )
                                          ),
                                          br(),
                                          DT::dataTableOutput(ns(
                                                 "preview_table"
                                          ))
                                   ),
                                   # Tab 5: Save/Load Configurations
                                   tabPanel(
                                          "Save/Load Configurations",
                                          br(),
                                          fluidRow(
                                                 column(
                                                        12,
                                                        h3(
                                                               "Save and Load Complete Configurations"
                                                        ),
                                                        p(
                                                               "Use these controls to save your current parameter settings to JSON files, or load previously saved configurations."
                                                        ),
                                                        p(
                                                               "Saved configurations include: base values, inflation adjustments, area weights, all feature weights, and all structure type weights."
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

                                                               h5(
                                                                      "Download Current Configuration"
                                                               ),
                                                               p(
                                                                      "Save the current Existing Scenario parameters to a JSON file.",
                                                                      style = "font-size: 12px; color: #666;"
                                                               ),
                                                               downloadButton(
                                                                      ns(
                                                                             "download_config_existing"
                                                                      ),
                                                                      "Download Existing Configuration",
                                                                      icon = icon(
                                                                             "download"
                                                                      ),
                                                                      class = "btn-success btn-block"
                                                               ),

                                                               br(),
                                                               hr(),
                                                               br(),

                                                               h5(
                                                                      "Upload Configuration"
                                                               ),
                                                               p(
                                                                      "Load a previously saved configuration file.",
                                                                      style = "font-size: 12px; color: #666;"
                                                               ),
                                                               fileInput(
                                                                      ns(
                                                                             "upload_config_existing"
                                                                      ),
                                                                      label = NULL,
                                                                      accept = c(
                                                                             ".json"
                                                                      ),
                                                                      buttonLabel = "Browse...",
                                                                      placeholder = "Select JSON file"
                                                               ),

                                                               helpText(
                                                                      icon(
                                                                             "info-circle"
                                                                      ),
                                                                      " After uploading, all parameters will be updated automatically.",
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

                                                               h5(
                                                                      "Download Current Configuration"
                                                               ),
                                                               p(
                                                                      "Save the current Scenario A parameters to a JSON file.",
                                                                      style = "font-size: 12px; color: #666;"
                                                               ),
                                                               downloadButton(
                                                                      ns(
                                                                             "download_config_scenario_a"
                                                                      ),
                                                                      "Download Scenario A Configuration",
                                                                      icon = icon(
                                                                             "download"
                                                                      ),
                                                                      class = "btn-success btn-block"
                                                               ),

                                                               br(),
                                                               hr(),
                                                               br(),

                                                               h5(
                                                                      "Upload Configuration"
                                                               ),
                                                               p(
                                                                      "Load a previously saved configuration file.",
                                                                      style = "font-size: 12px; color: #666;"
                                                               ),
                                                               fileInput(
                                                                      ns(
                                                                             "upload_config_scenario_a"
                                                                      ),
                                                                      label = NULL,
                                                                      accept = c(
                                                                             ".json"
                                                                      ),
                                                                      buttonLabel = "Browse...",
                                                                      placeholder = "Select JSON file"
                                                               ),

                                                               helpText(
                                                                      icon(
                                                                             "info-circle"
                                                                      ),
                                                                      " After uploading, all parameters will be updated automatically.",
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

                                                               h5(
                                                                      "Download Current Configuration"
                                                               ),
                                                               p(
                                                                      "Save the current Scenario B parameters to a JSON file.",
                                                                      style = "font-size: 12px; color: #666;"
                                                               ),
                                                               downloadButton(
                                                                      ns(
                                                                             "download_config_scenario_b"
                                                                      ),
                                                                      "Download Scenario B Configuration",
                                                                      icon = icon(
                                                                             "download"
                                                                      ),
                                                                      class = "btn-success btn-block"
                                                               ),

                                                               br(),
                                                               hr(),
                                                               br(),

                                                               h5(
                                                                      "Upload Configuration"
                                                               ),
                                                               p(
                                                                      "Load a previously saved configuration file.",
                                                                      style = "font-size: 12px; color: #666;"
                                                               ),
                                                               fileInput(
                                                                      ns(
                                                                             "upload_config_scenario_b"
                                                                      ),
                                                                      label = NULL,
                                                                      accept = c(
                                                                             ".json"
                                                                      ),
                                                                      buttonLabel = "Browse...",
                                                                      placeholder = "Select JSON file"
                                                               ),

                                                               helpText(
                                                                      icon(
                                                                             "info-circle"
                                                                      ),
                                                                      " After uploading, all parameters will be updated automatically.",
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

                                                               h5(
                                                                      "Saving Configurations"
                                                               ),
                                                               tags$ol(
                                                                      tags$li(
                                                                             "Adjust all parameters (base value, inflation, area weight, feature weights, structure type weights) as desired"
                                                                      ),
                                                                      tags$li(
                                                                             "Click the 'Download' button for the scenario you want to save"
                                                                      ),
                                                                      tags$li(
                                                                             "A JSON file will be downloaded with a timestamped filename (e.g., module2_existing_20250117_143052.json)"
                                                                      ),
                                                                      tags$li(
                                                                             "Store this file in a safe location or share it with colleagues"
                                                                      )
                                                               ),

                                                               br(),

                                                               h5(
                                                                      "Loading Configurations"
                                                               ),
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
                                                                             "Navigate through the tabs to see all updated parameters"
                                                                      )
                                                               ),

                                                               br(),

                                                               h5("Tips"),
                                                               tags$ul(
                                                                      tags$li(
                                                                             strong(
                                                                                    "Naming:"
                                                                             ),
                                                                             " Downloaded files include the scenario name and timestamp for easy identification"
                                                                      ),
                                                                      tags$li(
                                                                             strong(
                                                                                    "Cloning:"
                                                                             ),
                                                                             " To clone Scenario A to Scenario B, download from A and upload to B"
                                                                      ),
                                                                      tags$li(
                                                                             strong(
                                                                                    "Archiving:"
                                                                             ),
                                                                             " Create a folder to organize configurations by policy scenario or date"
                                                                      ),
                                                                      tags$li(
                                                                             strong(
                                                                                    "Validation:"
                                                                             ),
                                                                             " The system validates files to ensure they're Module 2 configurations"
                                                                      ),
                                                                      tags$li(
                                                                             strong(
                                                                                    "Sharing:"
                                                                             ),
                                                                             " JSON files can be emailed or stored in cloud drives for team collaboration"
                                                                      )
                                                               )
                                                        )
                                                 )
                                          )
                                   )
                            ),

                            hr(),

                            # Configuration summary
                            fluidRow(
                                   column(
                                          12,
                                          h4("Configuration Summary"),
                                          verbatimTextOutput(ns(
                                                 "config_summary"
                                          ))
                                   )
                            )
                     )
              )
       )
}
