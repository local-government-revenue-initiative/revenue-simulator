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
        
        # Processing section
        conditionalPanel(
          condition = paste0("output['", ns("mappings_validated"), "']"),
          h4("Step 3: Process Data"),
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
          verbatimTextOutput(ns("data_summary"))
        )
      )
    )
  )
}