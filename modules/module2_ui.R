# modules/module2_ui.R
# Module 2: Value Parameters UI - Updated with dynamic parameter defaults

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

                            # General note
                            div(
                                   class = "alert alert-info",
                                   style = "margin-bottom: 20px;",
                                   HTML(
                                          "<strong><i class='fa fa-info-circle'></i> General Note:</strong><br>
            Avoid any specific currency or length of measurement unit. 
            For example, do not specify dollars or feet. 
            Default values are loaded from the city's parameter configuration."
                                   )
                            ),

                            # Main tabset
                            tabsetPanel(
                                   id = ns("main_tabs"),

                                   # Tab 1: Base Values and Area Weight
                                   tabPanel(
                                          "Base Values",
                                          br(),
                                          p(
                                                 "Configure the base value, inflation adjustment, and area weight for each scenario.",
                                                 "These parameters form the foundation of property value calculations."
                                          ),
                                          br(),

                                          fluidRow(
                                                 # Existing Scenario
                                                 column(
                                                        4,
                                                        box(
                                                               title = "Existing Scenario",
                                                               width = 12,
                                                               status = "primary",
                                                               solidHeader = TRUE,
                                                               uiOutput(ns(
                                                                      "base_params_existing"
                                                               ))
                                                        )
                                                 ),
                                                 # Scenario A
                                                 column(
                                                        4,
                                                        box(
                                                               title = "Alternative Scenario A",
                                                               width = 12,
                                                               status = "info",
                                                               solidHeader = TRUE,
                                                               uiOutput(ns(
                                                                      "base_params_scenario_a"
                                                               ))
                                                        )
                                                 ),
                                                 # Scenario B
                                                 column(
                                                        4,
                                                        box(
                                                               title = "Alternative Scenario B",
                                                               width = 12,
                                                               status = "info",
                                                               solidHeader = TRUE,
                                                               uiOutput(ns(
                                                                      "base_params_scenario_b"
                                                               ))
                                                        )
                                                 )
                                          ),

                                          hr(),

                                          # Calculate button
                                          fluidRow(
                                                 column(
                                                        12,
                                                        div(
                                                               style = "text-align: center;",
                                                               actionButton(
                                                                      ns(
                                                                             "calculate_values"
                                                                      ),
                                                                      "Calculate Property Values for All Scenarios",
                                                                      class = "btn-success btn-lg",
                                                                      icon = icon(
                                                                             "calculator"
                                                                      )
                                                               ),
                                                               br(),
                                                               br(),
                                                               helpText(
                                                                      "Click to calculate property and business values using the current parameters.",
                                                                      "Results will be available in Module 4."
                                                               )
                                                        )
                                                 )
                                          )
                                   ),

                                   # Tab 2: Property Feature Weights
                                   tabPanel(
                                          "Property Feature Weights",
                                          br(),
                                          p(
                                                 "Weights for property features typically range from -250 to 250. ",
                                                 "Negative weights decrease value, positive weights increase value. ",
                                                 "Default values are loaded from the city's parameter configuration."
                                          ),
                                          br(),

                                          fluidRow(
                                                 column(
                                                        4,
                                                        h4("Existing Scenario"),
                                                        div(
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
                                                 "Weights for structure types can range from -100 to 5,000. ",
                                                 "These weights are applied to commercial and institutional property types. ",
                                                 "Default values are loaded from the city's parameter configuration."
                                          ),
                                          br(),

                                          fluidRow(
                                                 column(
                                                        4,
                                                        h4("Existing Scenario"),
                                                        div(
                                                               style = "max-height: 600px; overflow-y: auto;",
                                                               uiOutput(ns(
                                                                      "structure_ui_existing"
                                                               ))
                                                        )
                                                 ),
                                                 column(
                                                        4,
                                                        h4(
                                                               "Alternative Scenario A"
                                                        ),
                                                        div(
                                                               style = "max-height: 600px; overflow-y: auto;",
                                                               uiOutput(ns(
                                                                      "structure_ui_scenario_a"
                                                               ))
                                                        )
                                                 ),
                                                 column(
                                                        4,
                                                        h4(
                                                               "Alternative Scenario B"
                                                        ),
                                                        div(
                                                               style = "max-height: 600px; overflow-y: auto;",
                                                               uiOutput(ns(
                                                                      "structure_ui_scenario_b"
                                                               ))
                                                        )
                                                 )
                                          )
                                   ),

                                   # Tab 4: Preview Calculations
                                   tabPanel(
                                          "Preview Calculations",
                                          br(),
                                          p(
                                                 "Preview calculated property values for a specific scenario. ",
                                                 "Use the search and filter features to find specific properties."
                                          ),

                                          fluidRow(
                                                 column(
                                                        4,
                                                        selectInput(
                                                               ns(
                                                                      "preview_scenario"
                                                               ),
                                                               label = "Select Scenario to Preview",
                                                               choices = c(
                                                                      "Existing Scenario" = "existing",
                                                                      "Scenario A" = "scenario_a",
                                                                      "Scenario B" = "scenario_b"
                                                               ),
                                                               selected = "existing"
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
                                                 ),
                                                 column(
                                                        4,
                                                        br(),
                                                        helpText(
                                                               "Click to generate a preview of calculated values. ",
                                                               "Use column filters to search specific subsets."
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
                                                               "Use these controls to save your current parameter settings to JSON files, ",
                                                               "or load previously saved configurations."
                                                        ),
                                                        p(
                                                               "Saved configurations include: base values, inflation adjustments, ",
                                                               "area weights, all feature weights, and all structure type weights."
                                                        ),
                                                        hr()
                                                 )
                                          ),

                                          # Three columns for scenarios
                                          fluidRow(
                                                 # Existing
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
                                                                      "Save the current parameters to a JSON file.",
                                                                      style = "font-size: 12px; color: #666;"
                                                               ),
                                                               downloadButton(
                                                                      ns(
                                                                             "download_config_existing"
                                                                      ),
                                                                      "Download Configuration",
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
                                                                      " After uploading, all parameters will be updated.",
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
                                                                      "Save the current parameters to a JSON file.",
                                                                      style = "font-size: 12px; color: #666;"
                                                               ),
                                                               downloadButton(
                                                                      ns(
                                                                             "download_config_scenario_a"
                                                                      ),
                                                                      "Download Configuration",
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
                                                                      " After uploading, all parameters will be updated.",
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
                                                                      "Save the current parameters to a JSON file.",
                                                                      style = "font-size: 12px; color: #666;"
                                                               ),
                                                               downloadButton(
                                                                      ns(
                                                                             "download_config_scenario_b"
                                                                      ),
                                                                      "Download Configuration",
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
                                                                      " After uploading, all parameters will be updated.",
                                                                      style = "color: #666; font-size: 11px;"
                                                               )
                                                        )
                                                 )
                                          ),

                                          # Tips section
                                          fluidRow(
                                                 column(
                                                        12,
                                                        hr(),
                                                        h4("Tips"),
                                                        tags$ul(
                                                               tags$li(
                                                                      strong(
                                                                             "Naming:"
                                                                      ),
                                                                      " Downloaded files include the scenario name and timestamp."
                                                               ),
                                                               tags$li(
                                                                      strong(
                                                                             "Cloning:"
                                                                      ),
                                                                      " To copy settings between scenarios, download from one and upload to another."
                                                               ),
                                                               tags$li(
                                                                      strong(
                                                                             "Archiving:"
                                                                      ),
                                                                      " Save configurations before making significant changes."
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
