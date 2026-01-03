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
      param_license = NULL
    )

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
      data_file <- file.path("data", paste0(city, "_data.rds"))

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

            incProgress(0.3, detail = "Extracting combined data...")

            # Extract components from the bundle
            # The RDS file contains a list with named elements
            # We need to handle different possible naming conventions

            # Get combined data (try different possible names)
            combined_data_names <- c(
              "combined_data",
              "combined_data_sample_rows",
              paste0(city, "_data"),
              "data"
            )

            for (name in combined_data_names) {
              if (name %in% names(data_bundle)) {
                values$combined_data <- data_bundle[[name]]
                break
              }
            }

            if (is.null(values$combined_data)) {
              # If no matching name found, try the first element if it's a data frame
              first_elem <- data_bundle[[1]]
              if (is.data.frame(first_elem)) {
                values$combined_data <- first_elem
              } else {
                stop("Could not find combined data in the data bundle.")
              }
            }

            incProgress(0.2, detail = "Extracting parameters...")

            # Extract parameter tables
            values$param_additions <- data_bundle[["param_additions"]]
            values$param_features <- data_bundle[["param_features"]]
            values$param_prop_struct_type <- data_bundle[[
              "param_prop_struct_type"
            ]]
            values$param_tax_min_rate <- data_bundle[["param_tax_min_rate"]]
            values$param_license <- data_bundle[["param_license"]]

            incProgress(0.2, detail = "Finalizing...")

            # Mark as authenticated and store city
            values$authenticated <- TRUE
            values$city <- city

            # Success message
            output$auth_status <- renderUI({
              div(
                class = "alert alert-success",
                style = "margin-top: 15px;",
                HTML(
                  paste0(
                    "<i class='fa fa-check-circle'></i> Successfully loaded data for ",
                    tools::toTitleCase(city),
                    "."
                  )
                )
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

    # Output: Data loaded flag for conditional panel
    output$data_loaded <- reactive({
      values$authenticated && !is.null(values$combined_data)
    })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

    # Output: Total properties value box
    output$total_properties <- renderValueBox({
      req(values$combined_data)
      n_properties <- length(unique(values$combined_data$id_property))
      valueBox(
        value = format(n_properties, big.mark = ","),
        subtitle = "Unique Properties",
        icon = icon("home"),
        color = "blue"
      )
    })

    # Output: Total businesses value box
    output$total_businesses <- renderValueBox({
      req(values$combined_data)
      # Count non-NA business IDs
      n_businesses <- sum(!is.na(values$combined_data$id_business))
      valueBox(
        value = format(n_businesses, big.mark = ","),
        subtitle = "Business Records",
        icon = icon("building"),
        color = "green"
      )
    })

    # Output: Total rows value box
    output$total_rows <- renderValueBox({
      req(values$combined_data)
      n_rows <- nrow(values$combined_data)
      valueBox(
        value = format(n_rows, big.mark = ","),
        subtitle = "Total Data Rows",
        icon = icon("database"),
        color = "purple"
      )
    })

    # Output: Property type summary table
    output$property_type_summary <- renderTable(
      {
        req(values$combined_data)

        values$combined_data %>%
          group_by(property_type) %>%
          summarise(
            `Unique Properties` = n_distinct(id_property),
            `Total Rows` = n(),
            .groups = "drop"
          ) %>%
          arrange(desc(`Unique Properties`)) %>%
          rename(`Property Type` = property_type)
      },
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE
    )

    # Output: Payment summary table
    output$payment_summary <- renderTable(
      {
        req(values$combined_data)

        # Get unique properties with their payment status
        values$combined_data %>%
          distinct(id_property, made_payment) %>%
          group_by(made_payment) %>%
          summarise(
            `Number of Properties` = n(),
            .groups = "drop"
          ) %>%
          mutate(
            `Payment Status` = ifelse(made_payment, "Paid", "Not Paid")
          ) %>%
          select(`Payment Status`, `Number of Properties`)
      },
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE
    )

    # Data preview - reactive to search
    preview_data <- reactiveVal(NULL)

    # Initialize preview with first 100 rows
    observe({
      req(values$combined_data)
      # Select key columns for preview
      key_cols <- c(
        "id_property",
        "property_type",
        "property_area",
        "commercial_type",
        "made_payment",
        "id_business",
        "business_category",
        "business_area",
        "ward_number"
      )
      available_cols <- key_cols[key_cols %in% names(values$combined_data)]
      preview_data(head(values$combined_data[, available_cols], 100))
    })

    # Handle search
    observeEvent(input$search_btn, {
      req(values$combined_data, input$search_property_id)

      search_id <- trimws(input$search_property_id)
      if (search_id == "") {
        # Reset to default preview
        key_cols <- c(
          "id_property",
          "property_type",
          "property_area",
          "commercial_type",
          "made_payment",
          "id_business",
          "business_category",
          "business_area",
          "ward_number"
        )
        available_cols <- key_cols[key_cols %in% names(values$combined_data)]
        preview_data(head(values$combined_data[, available_cols], 100))
      } else {
        # Filter by property ID
        filtered <- values$combined_data %>%
          filter(grepl(search_id, id_property, ignore.case = TRUE))

        if (nrow(filtered) == 0) {
          showNotification(
            paste("No properties found matching:", search_id),
            type = "warning"
          )
        }

        key_cols <- c(
          "id_property",
          "property_type",
          "property_area",
          "commercial_type",
          "made_payment",
          "id_business",
          "business_category",
          "business_area",
          "ward_number"
        )
        available_cols <- key_cols[key_cols %in% names(filtered)]
        preview_data(filtered[, available_cols])
      }
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

    # Return all data and parameters to be used by other modules
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
