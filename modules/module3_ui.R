# modules/module3_ui.R
# Module 3: Tax Parameters UI - Updated with parameter-based defaults

module3_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "module3",
    fluidRow(
      box(
        title = "Module 3: Tax Parameters (Property Tax & Business License)",
        width = 12,
        status = "primary",
        solidHeader = TRUE,

        # Info note
        div(
          class = "alert alert-info",
          style = "margin-bottom: 20px;",
          HTML(
            "<strong><i class='fa fa-info-circle'></i> Note:</strong><br>
            Configure property tax rates and business license fees for each scenario.
            Default values are loaded from the city's parameter configuration.
            Property tax is calculated as: max(property_value × rate, minimum)."
          )
        ),

        # Main tabset
        tabsetPanel(
          id = ns("main_tabs"),

          # Tab 1: Property Tax Configuration
          tabPanel(
            "Property Tax Rates",
            br(),
            p(
              "Configure property tax rates and minimums for each property type. ",
              "You can optionally enable value-based slots for more granular control."
            ),
            br(),

            fluidRow(
              column(
                4,
                h4("Existing Scenario"),
                uiOutput(ns("property_tax_ui_existing"))
              ),
              column(
                4,
                h4("Scenario A"),
                uiOutput(ns("property_tax_ui_scenario_a"))
              ),
              column(
                4,
                h4("Scenario B"),
                uiOutput(ns("property_tax_ui_scenario_b"))
              )
            )
          ),

          # Tab 2: Business License Configuration
          tabPanel(
            "Business License Fees",
            br(),
            p(
              "Configure business license calculation methods and rates by subcategory. ",
              "Different subcategories can use different calculation methods."
            ),
            br(),

            fluidRow(
              column(
                4,
                h4("Existing Scenario"),
                uiOutput(ns("business_license_ui_existing"))
              ),
              column(
                4,
                h4("Scenario A"),
                uiOutput(ns("business_license_ui_scenario_a"))
              ),
              column(
                4,
                h4("Scenario B"),
                uiOutput(ns("business_license_ui_scenario_b"))
              )
            )
          ),

          # Tab 3: Property Tax Preview
          tabPanel(
            "Property Tax Preview",
            br(),
            p("Preview property tax calculations for the selected scenario."),

            fluidRow(
              column(
                3,
                selectInput(
                  ns("property_preview_scenario"),
                  "Select Scenario:",
                  choices = c(
                    "Existing" = "existing",
                    "Scenario A" = "scenario_a",
                    "Scenario B" = "scenario_b"
                  ),
                  selected = "existing"
                )
              ),
              column(
                3,
                numericInput(
                  ns("property_preview_rows"),
                  "Number of Rows:",
                  value = 100,
                  min = 10,
                  max = 1000,
                  step = 10
                )
              ),
              column(
                3,
                br(),
                actionButton(
                  ns("calculate_property_preview"),
                  "Calculate Preview",
                  class = "btn-primary",
                  icon = icon("calculator")
                )
              )
            ),
            br(),
            DT::dataTableOutput(ns("property_preview_table"))
          ),

          # Tab 4: Business License Preview
          tabPanel(
            "Business License Preview",
            br(),
            p(
              "Preview business license calculations for the selected scenario."
            ),

            fluidRow(
              column(
                3,
                selectInput(
                  ns("business_preview_scenario"),
                  "Select Scenario:",
                  choices = c(
                    "Existing" = "existing",
                    "Scenario A" = "scenario_a",
                    "Scenario B" = "scenario_b"
                  ),
                  selected = "existing"
                )
              ),
              column(
                3,
                numericInput(
                  ns("business_preview_rows"),
                  "Number of Rows:",
                  value = 100,
                  min = 10,
                  max = 1000,
                  step = 10
                )
              ),
              column(
                3,
                br(),
                actionButton(
                  ns("calculate_business_preview"),
                  "Calculate Preview",
                  class = "btn-primary",
                  icon = icon("calculator")
                )
              )
            ),
            br(),
            DT::dataTableOutput(ns("business_preview_table"))
          ),

          # Tab 5: Save/Load Configuration
          tabPanel(
            "Save/Load Configuration",
            br(),
            fluidRow(
              column(
                12,
                h3("Save and Load Tax Configurations"),
                p(
                  "Save your current tax parameter settings to JSON files, ",
                  "or load previously saved configurations."
                ),
                hr()
              )
            ),

            fluidRow(
              # Existing
              column(
                4,
                box(
                  title = "Existing Scenario",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  h5("Download Configuration"),
                  downloadButton(
                    ns("download_tax_config_existing"),
                    "Download",
                    class = "btn-success btn-block"
                  ),
                  br(),
                  hr(),
                  br(),
                  h5("Upload Configuration"),
                  fileInput(
                    ns("upload_tax_config_existing"),
                    label = NULL,
                    accept = ".json"
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
                  h5("Download Configuration"),
                  downloadButton(
                    ns("download_tax_config_scenario_a"),
                    "Download",
                    class = "btn-success btn-block"
                  ),
                  br(),
                  hr(),
                  br(),
                  h5("Upload Configuration"),
                  fileInput(
                    ns("upload_tax_config_scenario_a"),
                    label = NULL,
                    accept = ".json"
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
                  h5("Download Configuration"),
                  downloadButton(
                    ns("download_tax_config_scenario_b"),
                    "Download",
                    class = "btn-success btn-block"
                  ),
                  br(),
                  hr(),
                  br(),
                  h5("Upload Configuration"),
                  fileInput(
                    ns("upload_tax_config_scenario_b"),
                    label = NULL,
                    accept = ".json"
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
