# modules/module1_ui.R

module1_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "module1",
    fluidRow(
      box(
        title = "Module 1: Data Input & Preprocessing",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        
        # Beta testing notice
        div(
          class = "alert alert-warning",
          style = "margin-bottom: 20px; background-color: #fff3cd; border-color: #ffeaa7; color: #856404;",
          HTML("<strong><i class='fa fa-exclamation-triangle'></i> Beta Testing Phase:</strong><br>
                This tool is currently in a beta testing phase. Reports of errors and suggestions are welcomed. 
                Please communicate them to Evan.")
        ),
        
        # File upload section
        h4("Step 1: Upload Data Files"),
        fluidRow(
          column(4,
                 fileInput(ns("property_file"), "Upload Property Data CSV",
                           accept = c(".csv")),
                 verbatimTextOutput(ns("property_status"))
          ),
          column(4,
                 fileInput(ns("payment_file"), "Upload Payment Data CSV",
                           accept = c(".csv")),
                 verbatimTextOutput(ns("payment_status"))
          ),
          column(4,
                 fileInput(ns("business_file"), "Upload Business Data CSV",
                           accept = c(".csv")),
                 verbatimTextOutput(ns("business_status"))
          )
        ),
        
        hr(),
        
        # Column mapping section
        h4("Step 2: Map Columns"),
        conditionalPanel(
          condition = paste0("output['", ns("files_uploaded"), "']"),
          
          tabsetPanel(
            tabPanel("Property Columns",
                     br(),
                     uiOutput(ns("property_mapping_ui")),
                     br(),
                     actionButton(ns("validate_property"), "Validate Property Mapping", 
                                  class = "btn-primary")
            ),
            tabPanel("Payment Columns",
                     br(),
                     uiOutput(ns("payment_mapping_ui")),
                     br(),
                     actionButton(ns("validate_payment"), "Validate Payment Mapping", 
                                  class = "btn-primary")
            ),
            tabPanel("Business Columns",
                     br(),
                     uiOutput(ns("business_mapping_ui")),
                     br(),
                     actionButton(ns("validate_business"), "Validate Business Mapping", 
                                  class = "btn-primary")
            )
          )
        ),
        
        hr(),
        
        # Processing section with explanatory text
        conditionalPanel(
          condition = paste0("output['", ns("mappings_validated"), "']"),
          h4("Step 3: Process Data"),
          
          # Add informative alert box
          div(
            class = "alert alert-info",
            style = "margin-bottom: 20px;",
            HTML("<strong><i class='fa fa-info-circle'></i> Data Processing Note:</strong><br><br>
                  The processed data will have a row for each property-business combination:<br>
                  <ul style='margin-bottom: 0;'>
                    <li>If a property has multiple uses (domestic, commercial, institutional), each appears as a separate row</li>
                    <li>If the property has businesses, they will be matched to the commercial row first (if available), 
                        then domestic, then institutional</li>
                    <li>Multiple businesses at the same property will result in multiple rows</li>
                    <li>Business data will not be duplicated across different property types</li>
                  </ul>")
          ),
          
          actionButton(ns("process_data"), "Process and Merge Data", 
                       class = "btn-success btn-lg"),
          br(),
          br(),
          verbatimTextOutput(ns("processing_status"))
        )
      )
    ),
    
    # Preview section
    conditionalPanel(
      condition = paste0("output['", ns("data_processed"), "']"),
      fluidRow(
        box(
          title = "Processed Data Preview",
          width = 12,
          status = "success",
          collapsible = TRUE,
          DT::dataTableOutput(ns("processed_preview"))
        )
      ),
      fluidRow(
        box(
          title = "Data Summary",
          width = 12,
          status = "info",
          collapsible = TRUE,
          
          # Add explanation at the top of the summary
          div(
            class = "well well-sm",
            style = "background-color: #f0f8ff; border-left: 4px solid #3498db;",
            HTML("<strong>Understanding the Processed Data:</strong><br>
                  Each row represents either:<br>
                  • A property with a specific use type (when no businesses are present), OR<br>
                  • A property-business combination (when businesses exist at that property)<br><br>
                  This means a single property ID may appear multiple times if it has multiple uses or multiple businesses.")
          ),
          
          verbatimTextOutput(ns("data_summary"))
        )
      )
    )
  )
}