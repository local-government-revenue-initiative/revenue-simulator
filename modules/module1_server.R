# modules/module1_server.R
# Simplified Module 1: City Selection, Authentication, and Data Loading

module1_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Define city passwords (in production, consider more secure storage)
    city_passwords <- list(
      freetown = "6CW8FQR7+2X",
      kenema = "kenema_password",
      makeni = "makeni_password"
    )

    # Reactive values to store loaded data
    values <- reactiveValues(
      authenticated = FALSE,
      city = NULL,
      combined_data = NULL,
      param_additions = NULL,
      param_features = NULL,
      param_prop_struct_type = NULL,
      param_tax_min_rate = NULL,
      param_license = NULL,
      validation_warnings = NULL
    )

    # Reactive for data_loaded status (for conditional UI)
    output$data_loaded <- reactive({
      values$authenticated
    })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

    # Handle data loading and authentication
    observeEvent(input$load_data, {
      req(input$city_select, input$password)

      city <- input$city_select
      password <- input$password

      # Validate city selection
      if (city == "") {
        output$auth_status <- renderUI({
          div(
            class = "alert alert-danger",
            style = "margin-top: 15px;",
            HTML("<i class='fa fa-times-circle'></i> Please select a city.")
          )
        })
        return()
      }

      # Validate password
      correct_password <- city_passwords[[city]]
      if (is.null(correct_password) || password != correct_password) {
        output$auth_status <- renderUI({
          div(
            class = "alert alert-danger",
            style = "margin-top: 15px;",
            HTML(
              "<i class='fa fa-times-circle'></i> Incorrect password. Please try again."
            )
          )
        })
        values$authenticated <- FALSE
        return()
      }

      # Construct file path
      data_file <- file.path("data", paste0(city, "_data_all.rds"))

      # Check if file exists
      if (!file.exists(data_file)) {
        output$auth_status <- renderUI({
          div(
            class = "alert alert-danger",
            style = "margin-top: 15px;",
            HTML(
              paste0(
                "<i class='fa fa-times-circle'></i> Data file not found: ",
                data_file,
                "<br>Please ensure the data file is in the correct location."
              )
            )
          )
        })
        values$authenticated <- FALSE
        return()
      }

      # Load data with progress indicator
      withProgress(message = "Loading data...", value = 0, {
        incProgress(0.2, detail = "Reading data file...")

        tryCatch(
          {
            # Load the RDS file
            data_bundle <- readRDS(data_file)

            incProgress(0.2, detail = "Validating data structure...")

            # ============================================================
            # VALIDATE DATA BUNDLE
            # ============================================================
            validation <- validate_data_bundle(data_bundle)

            if (!validation$valid) {
              # Critical errors - cannot proceed
              output$auth_status <- renderUI({
                div(
                  class = "alert alert-danger",
                  style = "margin-top: 15px;",
                  HTML(
                    paste0(
                      "<i class='fa fa-times-circle'></i> <strong>Data validation failed:</strong><br>",
                      paste("• ", validation$errors, collapse = "<br>")
                    )
                  )
                )
              })
              values$authenticated <- FALSE
              return()
            }

            # Store warnings for display (non-critical issues)
            if (length(validation$warnings) > 0) {
              values$validation_warnings <- validation$warnings
            }

            incProgress(0.2, detail = "Extracting combined data...")

            # ============================================================
            # EXTRACT COMBINED DATA
            # ============================================================
            # Use the helper function for flexible naming
            values$combined_data <- extract_combined_data(data_bundle)

            if (is.null(values$combined_data)) {
              output$auth_status <- renderUI({
                div(
                  class = "alert alert-danger",
                  style = "margin-top: 15px;",
                  HTML(
                    "<i class='fa fa-times-circle'></i> Could not find combined data in the data bundle."
                  )
                )
              })
              values$authenticated <- FALSE
              return()
            }

            incProgress(0.2, detail = "Extracting parameters...")

            # ============================================================
            # EXTRACT PARAMETER TABLES
            # ============================================================
            values$param_additions <- data_bundle[["param_additions"]]
            values$param_features <- data_bundle[["param_features"]]
            values$param_prop_struct_type <- data_bundle[[
              "param_prop_struct_type"
            ]]
            values$param_tax_min_rate <- data_bundle[["param_tax_min_rate"]]
            values$param_license <- data_bundle[["param_license"]]

            incProgress(0.1, detail = "Finalizing...")

            # Mark as authenticated and store city
            values$authenticated <- TRUE
            values$city <- city

            # ============================================================
            # BUILD SUCCESS MESSAGE (with optional warnings)
            # ============================================================
            success_message <- paste0(
              "<i class='fa fa-check-circle'></i> Successfully loaded data for ",
              tools::toTitleCase(city),
              "."
            )

            # Add data summary
            n_properties <- length(unique(values$combined_data$id_property))
            n_rows <- nrow(values$combined_data)
            success_message <- paste0(
              success_message,
              "<br><small>",
              format(n_properties, big.mark = ","),
              " unique properties, ",
              format(n_rows, big.mark = ","),
              " total rows",
              "</small>"
            )

            # Add parameter status
            param_status <- c()
            if (!is.null(values$param_additions)) {
              param_status <- c(param_status, "additions")
            }
            if (!is.null(values$param_features)) {
              param_status <- c(param_status, "features")
            }
            if (!is.null(values$param_prop_struct_type)) {
              param_status <- c(param_status, "structure types")
            }
            if (!is.null(values$param_tax_min_rate)) {
              param_status <- c(param_status, "tax rates")
            }
            if (!is.null(values$param_license)) {
              param_status <- c(param_status, "licenses")
            }

            if (length(param_status) > 0) {
              success_message <- paste0(
                success_message,
                "<br><small>Parameters loaded: ",
                paste(param_status, collapse = ", "),
                "</small>"
              )
            }

            # Include warnings if any
            if (length(validation$warnings) > 0) {
              warning_html <- paste0(
                "<hr style='margin: 10px 0;'>",
                "<i class='fa fa-exclamation-triangle' style='color: #856404;'></i> ",
                "<strong>Warnings:</strong><br>",
                paste("• ", validation$warnings, collapse = "<br>")
              )
              success_message <- paste0(success_message, warning_html)
            }

            output$auth_status <- renderUI({
              div(
                class = if (length(validation$warnings) > 0) {
                  "alert alert-warning"
                } else {
                  "alert alert-success"
                },
                style = "margin-top: 15px;",
                HTML(success_message)
              )
            })

            incProgress(0.1, detail = "Done!")
          },
          error = function(e) {
            output$auth_status <- renderUI({
              div(
                class = "alert alert-danger",
                style = "margin-top: 15px;",
                HTML(
                  paste0(
                    "<i class='fa fa-times-circle'></i> Error loading data: ",
                    e$message
                  )
                )
              )
            })
            values$authenticated <- FALSE
          }
        )
      })
    })

    # ========================================================================
    # SUMMARY OUTPUTS
    # ========================================================================

    # Output: Total properties value box
    output$total_properties <- renderValueBox({
      req(values$combined_data)
      n_props <- length(unique(values$combined_data$id_property))
      valueBox(
        value = format(n_props, big.mark = ","),
        subtitle = "Unique Properties",
        icon = icon("home"),
        color = "blue"
      )
    })

    # Output: Total businesses value box
    output$total_businesses <- renderValueBox({
      req(values$combined_data)
      n_biz <- if ("id_business" %in% names(values$combined_data)) {
        sum(!is.na(values$combined_data$id_business))
      } else {
        0
      }
      valueBox(
        value = format(n_biz, big.mark = ","),
        subtitle = "Businesses",
        icon = icon("briefcase"),
        color = "green"
      )
    })

    # Output: Total rows value box
    output$total_rows <- renderValueBox({
      req(values$combined_data)
      valueBox(
        value = format(nrow(values$combined_data), big.mark = ","),
        subtitle = "Total Data Rows",
        icon = icon("database"),
        color = "purple"
      )
    })

    # Output: Property type summary table
    output$property_type_summary <- renderTable({
      req(values$combined_data)
      if ("property_type" %in% names(values$combined_data)) {
        summary_df <- as.data.frame(table(values$combined_data$property_type))
        names(summary_df) <- c("Property Type", "Count")
        summary_df$Count <- format(summary_df$Count, big.mark = ",")
        summary_df
      } else {
        data.frame(
          Message = "Property type column not found"
        )
      }
    })

    # Output: Payment status summary table
    output$payment_summary <- renderTable({
      req(values$combined_data)
      if ("made_payment" %in% names(values$combined_data)) {
        summary_df <- as.data.frame(table(values$combined_data$made_payment))
        names(summary_df) <- c("Made Payment", "Count")
        summary_df$`Made Payment` <- ifelse(
          summary_df$`Made Payment` == "TRUE" |
            summary_df$`Made Payment` == TRUE,
          "Yes",
          "No"
        )
        summary_df$Count <- format(summary_df$Count, big.mark = ",")
        summary_df
      } else {
        data.frame(
          Message = "Payment status column not found"
        )
      }
    })

    # ========================================================================
    # DATA PREVIEW
    # ========================================================================

    # Reactive for filtered preview data
    preview_data <- reactiveVal(NULL)

    # Search functionality
    observeEvent(input$search_btn, {
      req(values$combined_data)
      search_term <- input$search_property_id

      if (is.null(search_term) || search_term == "") {
        # Show first 100 rows if no search term
        filtered <- head(values$combined_data, 100)
      } else {
        # Filter by property ID (partial match)
        filtered <- values$combined_data[
          grepl(
            search_term,
            values$combined_data$id_property,
            ignore.case = TRUE
          ),
        ]
      }

      if (nrow(filtered) > 0) {
        # Select key columns for preview
        key_cols <- c(
          "id_property",
          "property_type",
          "property_area",
          "commercial_type",
          "made_payment",
          "id_business",
          "business_category",
          "business_sub_category",
          "business_area",
          "ward_number"
        )
        available_cols <- key_cols[key_cols %in% names(filtered)]
        preview_data(filtered[, available_cols, drop = FALSE])
      } else {
        preview_data(NULL)
      }
    })

    # Initialize preview on authentication
    observe({
      req(values$authenticated, values$combined_data)

      # Show first 100 rows initially
      key_cols <- c(
        "id_property",
        "property_type",
        "property_area",
        "commercial_type",
        "made_payment",
        "id_business",
        "business_category",
        "business_sub_category",
        "business_area",
        "ward_number"
      )
      available_cols <- key_cols[key_cols %in% names(values$combined_data)]
      preview_data(head(
        values$combined_data[, available_cols, drop = FALSE],
        100
      ))
    })

    # Output: Data preview table
    output$data_preview <- DT::renderDataTable({
      req(preview_data())
      DT::datatable(
        preview_data(),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "frtip"
        ),
        rownames = FALSE
      )
    })

    # ========================================================================
    # RETURN VALUES FOR OTHER MODULES
    # ========================================================================

    return(
      list(
        # Authentication status
        authenticated = reactive({
          values$authenticated
        }),
        city = reactive({
          values$city
        }),

        # Combined data (for calculations)
        combined_data = reactive({
          values$combined_data
        }),

        # Parameter tables (for default values in Module 2 and 3)
        param_additions = reactive({
          values$param_additions
        }),
        param_features = reactive({
          values$param_features
        }),
        param_prop_struct_type = reactive({
          values$param_prop_struct_type
        }),
        param_tax_min_rate = reactive({
          values$param_tax_min_rate
        }),
        param_license = reactive({
          values$param_license
        })
      )
    )
  })
}
