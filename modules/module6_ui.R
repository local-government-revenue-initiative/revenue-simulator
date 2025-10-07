# modules/module6_ui.R
# Module 6: GIS Layer Revenue Filtering with Interactive Map

add_busy_bar(color = "#3c8dbc", height = "3px")

module6_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "module6",
    
    useToastr()

    # Header
    h2("Module 6: GIS Layer Revenue Analysis"),
    p("Filter and analyze revenue by geographic layers and ward boundaries"),
    
    fluidRow(
      # Left Panel: Controls
      column(
        width = 3,
        
        # Scenario Selection
        box(
          title = "Scenario Selection",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          
          selectInput(
            ns("scenario_select"),
            label = "Select Scenario:",
            choices = c("Existing", "Scenario A", "Scenario B"),
            selected = "Existing"
          )
        ),
        
        # GIS Layer Filters
        box(
          title = "Geographic Filters",
          width = NULL,
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          
          h5("Select GIS Layers:"),
          
          # Tourist Areas
          checkboxGroupInput(
            ns("tourist_filters"),
            label = "Tourist Areas:",
            choices = c(
              "Aberdeen/Lumley Tourist Area" = "aberdeen_lumley_tourist",
              "Juba/Levuma Tourist Area" = "juba_levuma_tourist"
            )
          ),
          
          # Commercial/Industrial
          checkboxGroupInput(
            ns("commercial_filters"),
            label = "Commercial/Industrial:",
            choices = c(
              "Commercial Corridors" = "buffered_commercial_corridors",
              "Central Business District" = "cbd",
              "Dock Industrial" = "dock_industrial",
              "Kissy Industrial Area" = "kissy_industrial_area",
              "Kissy Texaco Terminal" = "kissy_texaco_terminal_area",
              "Wellington Industrial Estate" = "wellington_industrial_estate"
            )
          ),
          
          # Other Areas
          checkboxGroupInput(
            ns("other_filters"),
            label = "Other Areas:",
            choices = c(
              "Hazardous Zones" = "hazardous_zones",
              "Informal Settlements" = "informal_settlements"
            )
          ),
          
          # Ward Selection
          h5("Ward Selection:"),
          selectizeInput(
            ns("ward_select"),
            label = NULL,
            choices = NULL,  # Will be populated dynamically
            multiple = TRUE,
            options = list(
              placeholder = "Select wards...",
              plugins = list('remove_button')
            )
          ),
          
          # Filter Actions
          br(),
          actionButton(
            ns("apply_filters"),
            label = "Apply Filters",
            icon = icon("filter"),
            class = "btn-primary",
            width = "100%"
          ),
          actionButton(
            ns("clear_filters"),
            label = "Clear All",
            icon = icon("times"),
            class = "btn-warning",
            width = "100%"
          )
        ),
        
        # Revenue Summary Box
        box(
          title = "Filtered Revenue Summary",
          width = NULL,
          status = "success",
          solidHeader = TRUE,
          
          valueBoxOutput(ns("total_revenue_box"), width = NULL),
          valueBoxOutput(ns("property_tax_box"), width = NULL),
          valueBoxOutput(ns("business_license_box"), width = NULL),
          
          hr(),
          
          h5("Properties in Selection:"),
          verbatimTextOutput(ns("property_count")),
          
          h5("Businesses in Selection:"),
          verbatimTextOutput(ns("business_count"))
        )
      ),
      
      # Right Panel: Map and Details
      column(
        width = 9,
        
        # Interactive Map
        box(
          title = "Interactive Revenue Map",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          
          # Map tabs
          tabsetPanel(
            id = ns("map_tabs"),
            
            # Main Map Tab
            tabPanel(
              "Revenue Map",
              icon = icon("map"),
              br(),
              withSpinner(
                leafletOutput(ns("revenue_map"), height = "600px"),
                type = 6,  # Different spinner types: 1-8
                color = "#3c8dbc",  # Match your theme color
                size = 1.5
              ),
              br(),
              
              # Map Controls
              fluidRow(
                column(
                  width = 6,
                  radioButtons(
                    ns("map_metric"),
                    label = "Display Metric:",
                    choices = c(
                      "Total Revenue" = "total",
                      "Property Tax" = "property_tax",
                      "Business License" = "business_license",
                      "Revenue Density" = "density"
                    ),
                    selected = "total",
                    inline = TRUE
                  )
                ),
                column(
                  width = 6,
                  radioButtons(
                    ns("map_aggregation"),
                    label = "Aggregation Level:",
                    choices = c(
                      "Individual Properties" = "property",
                      "Ward Level" = "ward",
                      "Heat Map" = "heat"
                    ),
                    selected = "ward",
                    inline = TRUE
                  )
                )
              )
            ),
            
            # Comparison Tab
            tabPanel(
              "Layer Comparison",
              icon = icon("chart-bar"),
              br(),
              withSpinner(plotOutput(ns("layer_comparison_plot")), height = "500px")),
              br(),
              DT::dataTableOutput(ns("layer_comparison_table"))
            ),
            
            # Ward Analysis Tab
            tabPanel(
              "Ward Analysis",
              icon = icon("building"),
              br(),
              withSpinner(
                plotOutput(ns("ward_revenue_plot"), height = "400px"),
                type = 4,
                color = "#3c8dbc"
              )
                br(),
              withSpinner(DT::dataTableOutput(ns("ward_details_table")))

            )
          )
        )
      )
    ),
    
    # Bottom Row: Additional Analysis
    fluidRow(
      # Distribution by Layer
      box(
        title = "Revenue Distribution by GIS Layer",
        width = 6,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        
        plotOutput(ns("layer_distribution_plot"), height = "300px")
      ),
      
      # Top Contributing Areas
      box(
        title = "Top Contributing Areas",
        width = 6,
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        
        DT::dataTableOutput(ns("top_areas_table"))
      )
    ),
    
    # Export Options
    fluidRow(
      box(
        title = "Export Options",
        width = 12,
        status = "success",
        solidHeader = FALSE,
        
        h5("Download Filtered Data:"),
        
        fluidRow(
          column(
            width = 3,
            downloadButton(
              ns("download_filtered_csv"),
              label = "Download CSV",
              icon = icon("file-csv"),
              class = "btn-success",
              width = "100%"
            )
          ),
          column(
            width = 3,
            downloadButton(
              ns("download_map_image"),
              label = "Download Map",
              icon = icon("image"),
              class = "btn-info",
              width = "100%"
            )
          ),
          column(
            width = 3,
            downloadButton(
              ns("download_report"),
              label = "Generate Report",
              icon = icon("file-pdf"),
              class = "btn-primary",
              width = "100%"
            )
          ),
          column(
            width = 3,
            downloadButton(
              ns("export_geojson"),
              label = "Export GeoJSON",
              icon = icon("globe"),
              class = "btn-warning",
              width = "100%"
            )
          )
        )
      )
    )
  )
}