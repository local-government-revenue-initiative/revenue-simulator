# modules/module5_ui.R

module5_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "module5",
    
    # Header and controls
    fluidRow(
      box(
        title = "Module 5: Tax Burden Analysis",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        
        # Control panel
        fluidRow(
          column(3,
                 selectInput(ns("analysis_scenario"),
                             "Primary Scenario for Analysis:",
                             choices = c("Scenario A" = "scenario_a",
                                         "Scenario B" = "scenario_b"),
                             selected = "scenario_a")
          ),
          column(2,
                 numericInput(ns("n_quantiles"),
                              "Number of Quantiles:",
                              value = 5,
                              min = 3,
                              max = 10)
          ),
          column(2,
                 numericInput(ns("overtax_threshold"),
                              "Overtax Threshold (%):",
                              value = 5,
                              min = 1,
                              max = 20,
                              step = 0.5)
          ),
          column(2,
                 checkboxInput(ns("filter_compliers"),
                               "Filter to Compliers Only",
                               value = FALSE)
          ),
          column(3,
                 br(),
                 actionButton(ns("run_analysis"),
                              "Run Analysis",
                              icon = icon("calculator"),
                              class = "btn-success btn-lg")
          )
        ),
        
        hr(),
        
        # Summary cards
        fluidRow(
          valueBoxOutput(ns("total_properties_box")),
          valueBoxOutput(ns("effective_rate_box")),
          valueBoxOutput(ns("gini_coefficient_box")),
          valueBoxOutput(ns("overtaxed_count_box"))
        )
      )
    ),
    
    # Main analysis tabs
    fluidRow(
      box(
        width = 12,
        
        tabsetPanel(id = ns("analysis_tabs"),
                    
                    # Tab 1: Progressivity Analysis
                    tabPanel("Progressivity Analysis",
                             icon = icon("chart-line"),
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Tax Burden Distribution by Property Value Quantiles"),
                                      p("This table shows how tax burden is distributed across property value quantiles."),
                                      DT::dataTableOutput(ns("quantile_burden_table"))
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(6,
                                      h4("Lorenz Curve - Tax Burden Distribution"),
                                      p("Curve shows cumulative tax share vs. cumulative property share. 
                  Closer to diagonal = more equal distribution."),
                                      plotOutput(ns("lorenz_curve_plot"), height = "400px")
                               ),
                               column(6,
                                      h4("Progressivity Index by Quantile"),
                                      p("Index > 1 means quantile pays more than proportional share of taxes."),
                                      plotOutput(ns("progressivity_index_plot"), height = "400px")
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Tax Share vs Value Share"),
                                      plotOutput(ns("tax_value_share_plot"), height = "350px")
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Progressivity Metrics Summary"),
                                      verbatimTextOutput(ns("progressivity_metrics"))
                               )
                             )
                    ),
                    
                    # Tab 2: Effective Tax Rates
                    tabPanel("Effective Tax Rates",
                             icon = icon("percentage"),
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Effective Tax Rate vs Property Value"),
                                      p("Shows relationship between property value and effective tax rate. 
                  Points above threshold line may be overtaxed."),
                                      plotOutput(ns("effective_rate_scatter"), height = "500px")
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(6,
                                      h4("Distribution of Effective Tax Rates by Quantile"),
                                      plotOutput(ns("effective_rate_boxplot"), height = "400px")
                               ),
                               column(6,
                                      h4("Average Effective Tax Rate by Property Type"),
                                      plotOutput(ns("effective_rate_by_type"), height = "400px")
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Effective Tax Rate Statistics"),
                                      DT::dataTableOutput(ns("effective_rate_stats_table"))
                               )
                             )
                    ),
                    
                    # Tab 3: Overtaxation Analysis
                    tabPanel("Overtaxation Risk",
                             icon = icon("exclamation-triangle"),
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Summary of Overtaxation Risk"),
                                      infoBoxOutput(ns("overtax_summary_low_value")),
                                      infoBoxOutput(ns("overtax_summary_business")),
                                      infoBoxOutput(ns("overtax_summary_total"))
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Properties at Risk of Overtaxation"),
                                      p("Properties with effective tax rates above the threshold, sorted by rate."),
                                      DT::dataTableOutput(ns("overtax_table"))
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(6,
                                      h4("Overtaxation by Value Quantile"),
                                      plotOutput(ns("overtax_by_quantile"), height = "400px")
                               ),
                               column(6,
                                      h4("Risk Categories Distribution"),
                                      plotOutput(ns("risk_categories_plot"), height = "400px")
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Combined Tax Burden Analysis (Property + Business)"),
                                      plotOutput(ns("combined_burden_plot"), height = "400px")
                               )
                             )
                    ),
                    
                    # Tab 4: Business Impact
                    tabPanel("Business Impact",
                             icon = icon("building"),
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Tax Burden Comparison: Properties With vs Without Businesses"),
                                      plotOutput(ns("business_comparison_plot"), height = "400px")
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(6,
                                      h4("Business License Share of Total Tax"),
                                      plotOutput(ns("business_share_plot"), height = "400px")
                               ),
                               column(6,
                                      h4("Effective Rates by Business Status"),
                                      plotOutput(ns("business_rates_boxplot"), height = "400px")
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Business Property Analysis by Quantile"),
                                      DT::dataTableOutput(ns("business_burden_table"))
                               )
                             )
                    ),
                    
                    # Tab 5: Scenario Comparison
                    tabPanel("Scenario Comparison",
                             icon = icon("exchange-alt"),
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Change in Tax Burden: Selected Scenario vs Existing"),
                                      plotOutput(ns("burden_change_waterfall"), height = "450px")
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(6,
                                      h4("Winners and Losers by Quantile"),
                                      DT::dataTableOutput(ns("winners_losers_table"))
                               ),
                               column(6,
                                      h4("Distribution of Tax Changes"),
                                      plotOutput(ns("tax_change_distribution"), height = "400px")
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Revenue Impact by Property Type"),
                                      plotOutput(ns("revenue_impact_plot"), height = "400px")
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Property-Level Changes"),
                                      p("Detailed view of tax changes for individual properties."),
                                      DT::dataTableOutput(ns("property_changes_table"))
                               )
                             )
                    ),
                    
                    # Tab 6: Special Cases
                    tabPanel("Special Cases",
                             icon = icon("flag"),
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Multi-Type Properties Analysis"),
                                      p("Properties that have multiple property types (e.g., both Commercial and Domestic)."),
                                      DT::dataTableOutput(ns("multi_type_properties"))
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(6,
                                      h4("Tax Concentration Analysis"),
                                      p("How much of total tax revenue comes from top taxpayers?"),
                                      plotOutput(ns("tax_concentration_plot"), height = "400px")
                               ),
                               column(6,
                                      h4("Concentration Metrics"),
                                      verbatimTextOutput(ns("concentration_metrics"))
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h4("Top 20 Taxpayers"),
                                      DT::dataTableOutput(ns("top_taxpayers_table"))
                               )
                             )
                    ),
                    
                    # Tab 7: Export Report
                    tabPanel("Export & Reports",
                             icon = icon("download"),
                             br(),
                             
                             fluidRow(
                               column(12,
                                      h3("Export Options"),
                                      br(),
                                      
                                      wellPanel(
                                        h4("Report Configuration"),
                                        fluidRow(
                                          column(4,
                                                 checkboxGroupInput(ns("report_sections"),
                                                                    "Include Sections:",
                                                                    choices = list(
                                                                      "Executive Summary" = "summary",
                                                                      "Progressivity Analysis" = "progressivity",
                                                                      "Effective Tax Rates" = "rates",
                                                                      "Overtaxation Risk" = "overtax",
                                                                      "Business Impact" = "business",
                                                                      "Scenario Comparison" = "comparison",
                                                                      "Special Cases" = "special"
                                                                    ),
                                                                    selected = c("summary", "progressivity", "overtax"))
                                          ),
                                          column(4,
                                                 radioButtons(ns("report_format"),
                                                              "Report Format:",
                                                              choices = list(
                                                                "PDF Report" = "pdf",
                                                                "HTML Report" = "html",
                                                                "Word Document" = "word"
                                                              ),
                                                              selected = "html")
                                          ),
                                          column(4,
                                                 br(),
                                                 actionButton(ns("generate_report"),
                                                              "Generate Report",
                                                              icon = icon("file-alt"),
                                                              class = "btn-primary btn-lg")
                                          )
                                        )
                                      ),
                                      
                                      br(),
                                      
                                      wellPanel(
                                        h4("Data Export"),
                                        p("Download the complete analysis dataset in Excel format."),
                                        fluidRow(
                                          column(4,
                                                 downloadButton(ns("download_excel_summary"),
                                                                "Download Summary Tables",
                                                                class = "btn-info")
                                          ),
                                          column(4,
                                                 downloadButton(ns("download_excel_detailed"),
                                                                "Download Detailed Data",
                                                                class = "btn-info")
                                          ),
                                          column(4,
                                                 downloadButton(ns("download_csv_all"),
                                                                "Download All Data (CSV)",
                                                                class = "btn-info")
                                          )
                                        )
                                      ),
                                      
                                      br(),
                                      
                                      conditionalPanel(
                                        condition = paste0("input['", ns("generate_report"), "'] > 0"),
                                        wellPanel(
                                          h4("Download Generated Report"),
                                          downloadButton(ns("download_report"),
                                                         "Download Report",
                                                         class = "btn-success btn-lg")
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