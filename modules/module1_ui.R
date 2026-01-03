# modules/module1_ui.R
# Simplified Module 1: City Selection, Authentication, and Data Loading

module1_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "module1",
    fluidRow(
      # Main input box
      box(
        title = "Module 1: Data Input",
        width = 12,
        status = "primary",
        solidHeader = TRUE,

        # Beta testing notice
        div(
          class = "alert alert-warning",
          style = "margin-bottom: 20px; background-color: #fff3cd; border-color: #ffeaa7; color: #856404;",
          HTML(
            "<strong><i class='fa fa-exclamation-triangle'></i> Beta Testing Phase:</strong><br>
            This tool is currently in a beta testing phase. Reports of errors and suggestions are welcomed. 
            Please communicate them to Evan."
          )
        ),

        # Link to guide
        div(
          style = "margin-bottom: 20px;",
          HTML(
            "<strong><i class='fa fa-book'></i> User Guide:</strong> 
            <a href='https://docs.google.com/document/d/1Sh7cF1IgW6cRaLCgS_T8PnBTkNPqO3er2Li9E3KOGZg/edit?usp=sharing' 
               target='_blank' 
               style='color: #007bff; text-decoration: underline;'>
              Link to guide for using the simulator
            </a>"
          )
        ),

        hr(),

        # City selection and authentication
        h4("Step 1: Select City and Authenticate"),
        fluidRow(
          column(
            4,
            selectInput(
              ns("city_select"),
              label = "Select City",
              choices = c(
                "Choose a city..." = "",
                "Freetown" = "freetown",
                "Kenema" = "kenema",
                "Makeni" = "makeni"
              ),
              selected = ""
            )
          ),
          column(
            4,
            passwordInput(
              ns("password"),
              label = "Enter Password",
              placeholder = "Enter city password"
            )
          ),
          column(
            4,
            div(
              style = "margin-top: 25px;",
              actionButton(
                ns("load_data"),
                "Load Data",
                class = "btn-primary",
                icon = icon("sign-in-alt")
              )
            )
          )
        ),

        # Authentication status message
        uiOutput(ns("auth_status")),

        hr(),

        # Data preview section (only shown after successful authentication)
        conditionalPanel(
          condition = paste0("output['", ns("data_loaded"), "']"),

          h4("Step 2: Review Data Summary"),

          # Summary statistics
          fluidRow(
            column(
              4,
              valueBoxOutput(ns("total_properties"), width = 12)
            ),
            column(
              4,
              valueBoxOutput(ns("total_businesses"), width = 12)
            ),
            column(
              4,
              valueBoxOutput(ns("total_rows"), width = 12)
            )
          ),

          # Property type breakdown
          fluidRow(
            column(
              6,
              box(
                title = "Properties by Type",
                width = 12,
                status = "info",
                solidHeader = FALSE,
                tableOutput(ns("property_type_summary"))
              )
            ),
            column(
              6,
              box(
                title = "Payment Status",
                width = 12,
                status = "info",
                solidHeader = FALSE,
                tableOutput(ns("payment_summary"))
              )
            )
          ),

          hr(),

          # Data preview table
          h4("Step 3: Preview Data"),
          p(
            "Search for specific properties by ID to verify data loaded correctly."
          ),
          fluidRow(
            column(
              4,
              textInput(
                ns("search_property_id"),
                label = "Search by Property ID",
                placeholder = "e.g., FCC0000007"
              )
            ),
            column(
              2,
              div(
                style = "margin-top: 25px;",
                actionButton(
                  ns("search_btn"),
                  "Search",
                  class = "btn-info",
                  icon = icon("search")
                )
              )
            )
          ),
          DT::dataTableOutput(ns("data_preview")),

          hr(),

          # Confirmation to proceed
          div(
            class = "alert alert-success",
            style = "margin-top: 20px;",
            HTML(
              "<strong><i class='fa fa-check-circle'></i> Data Loaded Successfully!</strong><br>
              You can now proceed to Module 2 to configure value parameters."
            )
          )
        )
      )
    )
  )
}
