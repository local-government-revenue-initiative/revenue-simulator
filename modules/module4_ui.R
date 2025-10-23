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
          column(
            12,
            p("Calculate and compare revenue across all three scenarios."),
            actionButton(
              ns("calculate_revenue"),
              "Calculate Revenue for All Scenarios",
              icon = icon("calculator"),
              class = "btn-success btn-lg"
            ),
            br(),
            br()
          )
        ),

        hr(),

        # Summary cards with custom styling for larger boxes
        tags$style(HTML(
          "
          .small-box .inner h3 {
            font-size: 24px;
            font-weight: bold;
            white-space: nowrap;
            overflow: visible;
          }
          .small-box .inner p {
            font-size: 14px;
          }
          .small-box {
            min-height: 110px;
          }
          .small-box .inner {
            padding: 15px;
          }
        "
        )),
        fluidRow(
          column(4, valueBoxOutput(ns("existing_total_revenue"))),
          column(4, valueBoxOutput(ns("scenario_a_total_revenue"))),
          column(4, valueBoxOutput(ns("scenario_b_total_revenue")))
        ),

        # Tabbed interface for different views
        tabsetPanel(
          id = ns("revenue_tabs"),

          # Tab 1: Summary Comparison
          tabPanel(
            "Summary Comparison",
            br(),
            fluidRow(
              column(
                12,
                h4("Chart 1A: Revenue by Type - All Properties"),
                plotOutput(ns("revenue_by_type_plot"), height = "400px")
              )
            ),
            br(),
            fluidRow(
              column(
                12,
                h4("Chart 1B: Revenue by Type - Filtered to Compliers"),
                plotOutput(
                  ns("revenue_by_type_compliers_plot"),
                  height = "400px"
                )
              )
            ),
            br(),
            # Chart 1C ---------------------------------------------------------------
            fluidRow(
              column(
                12,
                h4("Chart 1C: Revenue with Filtering Options"),
                wellPanel(
                  # First row: Property/Structure/Business filters
                  fluidRow(
                    column(
                      3,
                      selectInput(
                        ns("filter_structure_types"),
                        "Structure Types:",
                        choices = c("All"),
                        selected = "All",
                        multiple = TRUE,
                        selectize = TRUE
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        ns("filter_property_types"),
                        "Property Types:",
                        choices = c("All"),
                        selected = "All",
                        multiple = TRUE,
                        selectize = TRUE
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        ns("filter_license_categories"),
                        "License Categories:",
                        choices = c("All"),
                        selected = "All",
                        multiple = TRUE,
                        selectize = TRUE
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        ns("filter_license_subcategories"),
                        "License Subcategories:",
                        choices = c("All"),
                        selected = "All",
                        multiple = TRUE,
                        selectize = TRUE
                      )
                    )
                  ),

                  hr(),

                  # NEW: Second row - Geographic filters header
                  fluidRow(
                    column(
                      12,
                      h5(strong("Geographic Filters")),
                      p(
                        style = "font-size: 12px; color: #666;",
                        "Filter revenue by location-based characteristics"
                      )
                    )
                  ),

                  # NEW: Third row - Geographic filter inputs
                  fluidRow(
                    column(
                      3,
                      checkboxGroupInput(
                        ns("filter_tourist_areas"),
                        label = "Tourist Areas:",
                        choices = c(
                          "Aberdeen/Lumley" = "aberdeen_lumley_tourist",
                          "Juba/Levuma" = "juba_levuma_tourist"
                        )
                      )
                    ),
                    column(
                      3,
                      checkboxGroupInput(
                        ns("filter_commercial_industrial"),
                        label = "Commercial/Industrial:",
                        choices = c(
                          "Commercial Corridors" = "buffered_commercial_corridors",
                          "CBD" = "cbd",
                          "Dock Industrial" = "dock_industrial",
                          "Kissy Industrial" = "kissy_industrial_area",
                          "Kissy Texaco Terminal" = "kissy_texaco_terminal_area",
                          "Grassfield Industrial Area" = "grassfield_industrial_area",
                          "Wellington Industrial" = "wellington_industrial_estate"
                        )
                      )
                    ),
                    column(
                      3,
                      checkboxGroupInput(
                        ns("filter_other_areas"),
                        label = "Other Areas:",
                        choices = c(
                          "Hazardous Zones" = "hazardous_zones",
                          "Informal Settlements" = "informal_settlements"
                        )
                      )
                    ),
                    column(
                      3,
                      selectizeInput(
                        ns("filter_wards"),
                        label = "Wards:",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = "Select wards...",
                          plugins = list('remove_button')
                        )
                      )
                    )
                  ),

                  # Action buttons
                  fluidRow(
                    column(
                      12,
                      actionButton(
                        ns("reset_filters"),
                        "Reset All Filters",
                        icon = icon("undo"),
                        class = "btn-warning btn-sm"
                      )
                    )
                  )
                ),
                plotOutput(ns("revenue_filtered_plot"), height = "400px")
              )
            ),
            br(),
            fluidRow(
              column(
                12,
                h4("Distribution of Total Tax Owed Across Scenarios"),
                plotOutput(
                  ns("tax_distribution_density_plot"),
                  height = "500px"
                )
              )
            )
          ),

          # Tab 2: Detailed Data
          tabPanel(
            "Detailed Data",
            br(),
            fluidRow(
              column(
                4,
                selectInput(
                  ns("detailed_scenario"),
                  "Select Scenario:",
                  choices = c(
                    "Existing" = "existing",
                    "Scenario A" = "scenario_a",
                    "Scenario B" = "scenario_b"
                  )
                )
              ),
              column(
                4,
                numericInput(
                  ns("detailed_rows"),
                  "Number of rows to display:",
                  value = 100,
                  min = 10,
                  max = 5000,
                  step = 100
                )
              ),
              column(
                4,
                br(),
                downloadButton(
                  ns("download_data"),
                  "Download Full Dataset",
                  class = "btn-primary"
                )
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
