# modules/module2_server.R

source("R/module2_config_functions.R")

module2_server <- function(id, processed_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values to store configurations
    values <- reactiveValues(
      existing_config = NULL,
      scenario_a_config = NULL,
      scenario_b_config = NULL,
      feature_columns = list(),
      structure_columns = list(),
      commercial_type_columns = list(),
      institutional_type_columns = list(),
      ward_columns = list(),
      preview_data = NULL,
      defaults = NULL,
      calculated_values_existing = NULL,
      calculated_values_scenario_a = NULL,
      calculated_values_scenario_b = NULL
    )

    # Initialize with defaults
    observe({
      if (is.null(values$defaults)) {
        values$defaults <- get_default_weights()

        # Initialize all configs with proper structure
        values$existing_config <- values$defaults
        values$scenario_a_config <- values$defaults
        values$scenario_b_config <- values$defaults
      }
    })

    # In the observe block that initializes when processed data is available:
    observe({
      req(processed_data())
      data <- processed_data()

      # Get feature columns from the processed data
      all_columns <- names(data)

      # Remove non-feature columns
      feature_cols <- all_columns[
        !all_columns %in%
          c(
            "id_property",
            "coordinate_lat",
            "coordinate_lng",
            "property_area",
            "made_payment",
            "business_name",
            "business_area",
            "business_category",
            "business_sub_category"
          )
      ]

      # Group features by category (excluding _na for UI)
      values$feature_columns <- group_feature_columns(feature_cols)
      values$structure_columns <- get_structure_type_columns(feature_cols)

      # Store all feature columns (including _na) for calculations
      values$all_feature_columns <- get_all_feature_columns(all_columns)
      values$all_structure_columns <- get_all_structure_columns(feature_cols)

      # Separate commercial and institutional types (excluding _na for UI)
      values$commercial_type_columns <- feature_cols[
        grepl("^commercial_type_", feature_cols) &
          !grepl("(_na|_NA)$", feature_cols)
      ]
      values$institutional_type_columns <- feature_cols[
        grepl("^institutional_type_", feature_cols) &
          !grepl("(_na|_NA)$", feature_cols)
      ]

      values$ward_columns <- feature_cols[
        grepl("^ward_number_[0-9]+", feature_cols) &
          !grepl("(_na|_NA)$", feature_cols)
      ]
    })

    output$download_config_existing <- downloadHandler(
      filename = function() {
        paste0(
          "module2_existing_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".json"
        )
      },
      content = function(file) {
        tryCatch(
          {
            # Collect current configuration
            config <- collect_module2_config(
              input = input,
              scenario_suffix = "existing",
              feature_columns = values$feature_columns,
              commercial_type_columns = values$commercial_type_columns,
              institutional_type_columns = values$institutional_type_columns,
              ward_columns = values$ward_columns
            )

            # Convert to JSON and write to file
            config_json <- jsonlite::toJSON(
              config,
              auto_unbox = TRUE,
              pretty = TRUE
            )
            writeLines(config_json, file)

            showNotification(
              "Configuration downloaded successfully!",
              type = "message"
            )
          },
          error = function(e) {
            showNotification(
              paste("Error downloading configuration:", e$message),
              type = "error"
            )
          }
        )
      }
    )

    # Download handler for Scenario A
    output$download_config_scenario_a <- downloadHandler(
      filename = function() {
        paste0(
          "module2_scenario_a_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".json"
        )
      },
      content = function(file) {
        tryCatch(
          {
            # Collect current configuration
            config <- collect_module2_config(
              input = input,
              scenario_suffix = "scenario_a",
              feature_columns = values$feature_columns,
              commercial_type_columns = values$commercial_type_columns,
              institutional_type_columns = values$institutional_type_columns,
              ward_columns = values$ward_columns
            )

            # Convert to JSON and write to file
            config_json <- jsonlite::toJSON(
              config,
              auto_unbox = TRUE,
              pretty = TRUE
            )
            writeLines(config_json, file)

            showNotification(
              "Configuration downloaded successfully!",
              type = "message"
            )
          },
          error = function(e) {
            showNotification(
              paste("Error downloading configuration:", e$message),
              type = "error"
            )
          }
        )
      }
    )

    # Download handler for Scenario B
    output$download_config_scenario_b <- downloadHandler(
      filename = function() {
        paste0(
          "module2_scenario_b_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".json"
        )
      },
      content = function(file) {
        tryCatch(
          {
            # Collect current configuration
            config <- collect_module2_config(
              input = input,
              scenario_suffix = "scenario_b",
              feature_columns = values$feature_columns,
              commercial_type_columns = values$commercial_type_columns,
              institutional_type_columns = values$institutional_type_columns,
              ward_columns = values$ward_columns
            )

            # Convert to JSON and write to file
            config_json <- jsonlite::toJSON(
              config,
              auto_unbox = TRUE,
              pretty = TRUE
            )
            writeLines(config_json, file)

            showNotification(
              "Configuration downloaded successfully!",
              type = "message"
            )
          },
          error = function(e) {
            showNotification(
              paste("Error downloading configuration:", e$message),
              type = "error"
            )
          }
        )
      }
    )

    # ==============================================================================
    # UPLOAD OBSERVERS - Add these to your module server function
    # ==============================================================================

    # Upload handler for Existing scenario
    observeEvent(input$upload_config_existing, {
      req(input$upload_config_existing)

      tryCatch(
        {
          # Load configuration from uploaded file
          config <- load_module2_config(input$upload_config_existing$datapath)

          # Update the config
          values$existing_config <- config

          # Apply ALL configuration values to inputs
          apply_module2_config(
            session = session,
            config = config,
            scenario_suffix = "existing",
            feature_columns = values$feature_columns,
            commercial_type_columns = values$commercial_type_columns,
            institutional_type_columns = values$institutional_type_columns,
            ward_columns = values$ward_columns
          )

          showNotification(
            "Configuration uploaded and applied successfully!",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error uploading configuration:", e$message),
            type = "error"
          )
        }
      )
    })

    # Upload handler for Scenario A
    observeEvent(input$upload_config_scenario_a, {
      req(input$upload_config_scenario_a)

      tryCatch(
        {
          # Load configuration from uploaded file
          config <- load_module2_config(
            input$upload_config_scenario_a$datapath
          )

          # Update the config
          values$scenario_a_config <- config

          # Apply ALL configuration values to inputs
          apply_module2_config(
            session = session,
            config = config,
            scenario_suffix = "scenario_a",
            feature_columns = values$feature_columns,
            commercial_type_columns = values$commercial_type_columns,
            institutional_type_columns = values$institutional_type_columns,
            ward_columns = values$ward_columns
          )

          showNotification(
            "Configuration uploaded and applied successfully!",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error uploading configuration:", e$message),
            type = "error"
          )
        }
      )
    })

    # Upload handler for Scenario B
    observeEvent(input$upload_config_scenario_b, {
      req(input$upload_config_scenario_b)

      tryCatch(
        {
          # Load configuration from uploaded file
          config <- load_module2_config(
            input$upload_config_scenario_b$datapath
          )

          # Update the config
          values$scenario_b_config <- config

          # Apply ALL configuration values to inputs
          apply_module2_config(
            session = session,
            config = config,
            scenario_suffix = "scenario_b",
            feature_columns = values$feature_columns,
            commercial_type_columns = values$commercial_type_columns,
            institutional_type_columns = values$institutional_type_columns,
            ward_columns = values$ward_columns
          )

          showNotification(
            "Configuration uploaded and applied successfully!",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error uploading configuration:", e$message),
            type = "error"
          )
        }
      )
    })

    # ==============================================================================
    # COPY FUNCTIONS
    # ==============================================================================

    # Copy Existing to Scenario A
    observeEvent(input$copy_existing_to_a, {
      config <- collect_module2_config(
        input = input,
        scenario_suffix = "existing",
        feature_columns = values$feature_columns,
        commercial_type_columns = values$commercial_type_columns,
        institutional_type_columns = values$institutional_type_columns,
        ward_columns = values$ward_columns
      )

      apply_module2_config(
        session = session,
        config = config,
        scenario_suffix = "scenario_a",
        feature_columns = values$feature_columns,
        commercial_type_columns = values$commercial_type_columns,
        institutional_type_columns = values$institutional_type_columns,
        ward_columns = values$ward_columns
      )

      showNotification("Copied Existing to Scenario A", type = "message")
    })

    # Copy Existing to Scenario B
    observeEvent(input$copy_existing_to_b, {
      config <- collect_module2_config(
        input = input,
        scenario_suffix = "existing",
        feature_columns = values$feature_columns,
        commercial_type_columns = values$commercial_type_columns,
        institutional_type_columns = values$institutional_type_columns,
        ward_columns = values$ward_columns
      )

      apply_module2_config(
        session = session,
        config = config,
        scenario_suffix = "scenario_b",
        feature_columns = values$feature_columns,
        commercial_type_columns = values$commercial_type_columns,
        institutional_type_columns = values$institutional_type_columns,
        ward_columns = values$ward_columns
      )

      showNotification("Copied Existing to Scenario B", type = "message")
    })

    # Copy Scenario A to Scenario B
    observeEvent(input$copy_a_to_b, {
      config <- collect_module2_config(
        input = input,
        scenario_suffix = "scenario_a",
        feature_columns = values$feature_columns,
        commercial_type_columns = values$commercial_type_columns,
        institutional_type_columns = values$institutional_type_columns,
        ward_columns = values$ward_columns
      )

      apply_module2_config(
        session = session,
        config = config,
        scenario_suffix = "scenario_b",
        feature_columns = values$feature_columns,
        commercial_type_columns = values$commercial_type_columns,
        institutional_type_columns = values$institutional_type_columns,
        ward_columns = values$ward_columns
      )

      showNotification("Copied Scenario A to Scenario B", type = "message")
    })

    # ==============================================================================
    # RESET FUNCTION
    # ==============================================================================

    observeEvent(input$reset_all_scenarios, {
      defaults <- get_default_weights()

      # Apply defaults to all three scenarios
      for (scenario in c("existing", "scenario_a", "scenario_b")) {
        apply_module2_config(
          session = session,
          config = defaults,
          scenario_suffix = scenario,
          feature_columns = values$feature_columns,
          commercial_type_columns = values$commercial_type_columns,
          institutional_type_columns = values$institutional_type_columns,
          ward_columns = values$ward_columns
        )
      }

      showNotification(
        "All scenarios reset to default weights",
        type = "message"
      )
    })

    # ==============================================================================
    # UI GENERATION
    # ==============================================================================

    # Function to generate feature weight UI for a scenario
    generate_feature_ui <- function(scenario_suffix) {
      tagList(
        # Base parameters card
        div(
          class = "card",
          div(
            class = "card-header",
            h4("Base Parameters")
          ),
          div(
            class = "card-body",
            fluidRow(
              column(
                4,
                numericInput(
                  ns(paste0("base_value_", scenario_suffix)),
                  "Base Value (Le)",
                  value = 1000,
                  min = 0
                )
              ),
              column(
                4,
                numericInput(
                  ns(paste0("inflation_", scenario_suffix)),
                  "Inflation (%)",
                  value = 0,
                  min = 0,
                  max = 100,
                  step = 0.1
                )
              ),
              column(
                4,
                numericInput(
                  ns(paste0("area_weight_", scenario_suffix)),
                  "Area Weight (exponent)",
                  value = 1,
                  min = 0,
                  max = 2,
                  step = 0.1
                )
              )
            ),
            # Display inflation-adjusted base value
            div(
              class = "alert alert-info mt-3",
              strong("Inflation-Adjusted Base Value: "),
              textOutput(
                ns(paste0("adjusted_base_", scenario_suffix)),
                inline = TRUE
              )
            )
          )
        ),
        # Structure Features card
        div(
          class = "card mt-3",
          div(
            class = "card-header",
            h4("Structure Features")
          ),
          div(
            class = "card-body",
            lapply(
              values$feature_columns$structure_features,
              function(feat) {
                feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
                numericInput(
                  ns(paste0("weight_", feat_safe, "_", scenario_suffix)),
                  gsub("_", " ", feat),
                  value = 1,
                  min = 0,
                  max = 2,
                  step = 0.05
                )
              }
            )
          )
        ),
        # Utility Features card
        div(
          class = "card mt-3",
          div(
            class = "card-header",
            h4("Utility Features")
          ),
          div(
            class = "card-body",
            lapply(
              values$feature_columns$utility_features,
              function(feat) {
                feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
                numericInput(
                  ns(paste0("weight_", feat_safe, "_", scenario_suffix)),
                  gsub("_", " ", feat),
                  value = 1,
                  min = 0,
                  max = 2,
                  step = 0.05
                )
              }
            )
          )
        ),
        # Location Features card
        div(
          class = "card mt-3",
          div(
            class = "card-header",
            h4("Location Features")
          ),
          div(
            class = "card-body",
            lapply(
              values$feature_columns$location_features,
              function(feat) {
                feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
                numericInput(
                  ns(paste0("weight_", feat_safe, "_", scenario_suffix)),
                  gsub("_", " ", feat),
                  value = 1,
                  min = 0,
                  max = 2,
                  step = 0.05
                )
              }
            )
          )
        ),
        # Location Zones card
        div(
          class = "card mt-3",
          div(
            class = "card-header",
            h4("Location Zones")
          ),
          div(
            class = "card-body",
            lapply(
              values$feature_columns$location_zones,
              function(feat) {
                feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
                numericInput(
                  ns(paste0("weight_", feat_safe, "_", scenario_suffix)),
                  gsub("_", " ", feat),
                  value = 1,
                  min = 0,
                  max = 2,
                  step = 0.05
                )
              }
            )
          )
        ),
        # Property Characteristics card
        div(
          class = "card mt-3",
          div(
            class = "card-header",
            h4("Property Characteristics")
          ),
          div(
            class = "card-body",
            lapply(
              values$feature_columns$property_characteristics,
              function(feat) {
                feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
                numericInput(
                  ns(paste0("weight_", feat_safe, "_", scenario_suffix)),
                  gsub("_", " ", feat),
                  value = 1,
                  min = 0,
                  max = 2,
                  step = 0.05
                )
              }
            )
          )
        ),
        # Ward Adjustments card
        if (length(values$ward_columns) > 0) {
          div(
            class = "card mt-3",
            div(
              class = "card-header",
              h4("Ward Adjustments")
            ),
            div(
              class = "card-body",
              lapply(
                values$ward_columns,
                function(feat) {
                  feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
                  numericInput(
                    ns(paste0("weight_", feat_safe, "_", scenario_suffix)),
                    gsub("ward_number_", "Ward ", feat),
                    value = 1,
                    min = 0,
                    max = 2,
                    step = 0.05
                  )
                }
              )
            )
          )
        }
      )
    }

    # Function to generate structure type weight UI
    generate_structure_ui <- function(scenario_suffix) {
      tagList(
        # Commercial Types card
        if (length(values$commercial_type_columns) > 0) {
          div(
            class = "card",
            div(
              class = "card-header",
              h4("Commercial Property Types")
            ),
            div(
              class = "card-body",
              lapply(
                values$commercial_type_columns,
                function(struct) {
                  struct_safe <- gsub("[^A-Za-z0-9_]", "_", struct)
                  numericInput(
                    ns(paste0("weight_", struct_safe, "_", scenario_suffix)),
                    gsub("commercial_type_", "", struct),
                    value = 1,
                    min = 0,
                    max = 3,
                    step = 0.1
                  )
                }
              )
            )
          )
        },
        # Institutional Types card
        if (length(values$institutional_type_columns) > 0) {
          div(
            class = "card mt-3",
            div(
              class = "card-header",
              h4("Institutional Property Types")
            ),
            div(
              class = "card-body",
              lapply(
                values$institutional_type_columns,
                function(struct) {
                  struct_safe <- gsub("[^A-Za-z0-9_]", "_", struct)
                  numericInput(
                    ns(paste0("weight_", struct_safe, "_", scenario_suffix)),
                    gsub("institutional_type_", "", struct),
                    value = 1,
                    min = 0,
                    max = 3,
                    step = 0.1
                  )
                }
              )
            )
          )
        }
      )
    }

    # Render UI for each scenario
    output$existing_feature_weights <- renderUI({
      req(values$feature_columns)
      generate_feature_ui("existing")
    })

    output$existing_structure_weights <- renderUI({
      req(
        values$commercial_type_columns,
        values$institutional_type_columns
      )
      generate_structure_ui("existing")
    })

    output$scenario_a_feature_weights <- renderUI({
      req(values$feature_columns)
      generate_feature_ui("scenario_a")
    })

    output$scenario_a_structure_weights <- renderUI({
      req(
        values$commercial_type_columns,
        values$institutional_type_columns
      )
      generate_structure_ui("scenario_a")
    })

    output$scenario_b_feature_weights <- renderUI({
      req(values$feature_columns)
      generate_feature_ui("scenario_b")
    })

    output$scenario_b_structure_weights <- renderUI({
      req(
        values$commercial_type_columns,
        values$institutional_type_columns
      )
      generate_structure_ui("scenario_b")
    })

    # Calculate inflation-adjusted base values
    output$adjusted_base_existing <- renderText({
      base <- input$base_value_existing
      inflation <- input$inflation_existing
      adjusted <- base * (1 + inflation / 100)
      format(adjusted, digits = 2, big.mark = ",")
    })

    output$adjusted_base_scenario_a <- renderText({
      base <- input$base_value_scenario_a
      inflation <- input$inflation_scenario_a
      adjusted <- base * (1 + inflation / 100)
      format(adjusted, digits = 2, big.mark = ",")
    })

    output$adjusted_base_scenario_b <- renderText({
      base <- input$base_value_scenario_b
      inflation <- input$inflation_scenario_b
      adjusted <- base * (1 + inflation / 100)
      format(adjusted, digits = 2, big.mark = ",")
    })

    # ==============================================================================
    # CALCULATE PREVIEW - UPDATED TO CALCULATE ALL ROWS
    # ==============================================================================

    observeEvent(input$calculate_preview, {
      req(processed_data(), input$preview_scenario)

      withProgress(message = "Calculating property values...", value = 0, {
        # Get scenario selection
        scenario <- switch(
          input$preview_scenario,
          "Existing" = "existing",
          "Scenario A" = "scenario_a",
          "Scenario B" = "scenario_b"
        )

        # Get full dataset (ALL rows, not just preview)
        data <- processed_data()
        n_total_rows <- nrow(data)

        cat("\n=== Starting property value calculation ===\n")
        cat("Total rows to process:", n_total_rows, "\n")
        cat("Scenario:", scenario, "\n")

        incProgress(0.1, detail = "Loading configuration...")

        # Get base parameters
        base_value <- input[[paste0("base_value_", scenario)]]
        inflation <- input[[paste0("inflation_", scenario)]]
        area_weight <- input[[paste0("area_weight_", scenario)]]

        cat("Base value:", base_value, "\n")
        cat("Inflation:", inflation, "%\n")
        cat("Area weight:", area_weight, "\n")

        # Calculate inflation-adjusted base
        inflation_adjusted_base <- base_value * (1 + inflation / 100)

        incProgress(0.1, detail = "Collecting feature weights...")

        # Collect all feature weights (including _na set to 0)
        feature_weights <- list()

        # Get all features (including _na)
        all_features <- c(
          values$feature_columns$structure_features,
          values$feature_columns$utility_features,
          values$feature_columns$location_features,
          values$feature_columns$location_zones,
          values$feature_columns$property_characteristics,
          values$ward_columns
        )

        # Add _na variables to the complete list
        if (!is.null(values$all_feature_columns)) {
          na_features <- values$all_feature_columns[grepl(
            "(_na|_NA)$",
            values$all_feature_columns
          )]
          all_features <- c(all_features, na_features)
          all_features <- unique(all_features)
        }

        # Collect weights for all features
        for (feat in all_features) {
          if (grepl("(_na|_NA)$", feat)) {
            # _na variables get weight 0
            feature_weights[[feat]] <- 0
          } else {
            feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
            input_id <- paste0("weight_", feat_safe, "_", scenario)
            weight <- input[[input_id]]
            feature_weights[[feat]] <- if (is.null(weight)) 1 else weight
          }
        }

        cat("Total feature weights collected:", length(feature_weights), "\n")

        incProgress(0.1, detail = "Collecting structure weights...")

        # Collect all structure weights (including _na set to 0)
        structure_weights <- list()

        # Get all structure types (including _na)
        all_structures <- c(
          values$commercial_type_columns,
          values$institutional_type_columns
        )

        # Add _na structure variables
        if (!is.null(values$all_structure_columns)) {
          na_structures <- values$all_structure_columns[grepl(
            "(_na|_NA)$",
            values$all_structure_columns
          )]
          all_structures <- c(all_structures, na_structures)
          all_structures <- unique(all_structures)
        }

        # Collect weights for all structure types
        for (struct in all_structures) {
          if (grepl("(_na|_NA)$", struct)) {
            # _na variables get weight 0
            structure_weights[[struct]] <- 0
          } else {
            struct_safe <- gsub("[^A-Za-z0-9_]", "_", struct)
            input_id <- paste0("weight_", struct_safe, "_", scenario)
            weight <- input[[input_id]]
            structure_weights[[struct]] <- if (is.null(weight)) 1 else weight
          }
        }

        cat(
          "Total structure weights collected:",
          length(structure_weights),
          "\n"
        )

        incProgress(
          0.2,
          detail = "Calculating property values for ALL properties..."
        )

        # Calculate product of feature weights for ALL rows
        product_weights <- rep(1, n_total_rows)

        for (feat in names(feature_weights)) {
          if (feat %in% names(data)) {
            weight <- feature_weights[[feat]]
            feat_values <- data[[feat]]

            # Apply weight: weight^feature_value
            product_weights <- product_weights * (weight^feat_values)
          }
        }

        # Calculate structure type multipliers for ALL rows
        structure_multipliers <- rep(1, n_total_rows)

        for (struct in names(structure_weights)) {
          if (struct %in% names(data)) {
            weight <- structure_weights[[struct]]
            struct_values <- data[[struct]]

            # Multiply by weight where structure type is present (value = 1)
            structure_multipliers <- structure_multipliers *
              ifelse(struct_values == 1, weight, 1)
          }
        }

        # Get property area for ALL rows
        property_area <- if ("property_area" %in% names(data)) {
          data$property_area
        } else {
          rep(NA, n_total_rows)
        }

        # Calculate property value for ALL rows
        property_value_all <- ifelse(
          !is.na(property_area) & property_area > 0,
          inflation_adjusted_base *
            (property_area^area_weight) *
            product_weights *
            structure_multipliers,
          NA
        )

        # Get business area for ALL rows
        business_area_all <- if ("business_area" %in% names(data)) {
          data$business_area
        } else {
          rep(NA, n_total_rows)
        }

        # Calculate business value for ALL rows
        business_value_all <- ifelse(
          !is.na(business_area_all) & business_area_all > 0,
          inflation_adjusted_base *
            (business_area_all^area_weight) *
            product_weights *
            structure_multipliers,
          NA
        )

        cat("\n=== Calculation Summary (ALL rows) ===\n")
        cat(
          "Property values calculated:",
          sum(!is.na(property_value_all)),
          "of",
          n_total_rows,
          "\n"
        )
        cat(
          "Property value range:",
          ifelse(
            sum(!is.na(property_value_all)) > 0,
            paste(
              min(property_value_all, na.rm = TRUE),
              "-",
              max(property_value_all, na.rm = TRUE)
            ),
            "All NA"
          ),
          "\n"
        )
        cat(
          "Business values calculated:",
          sum(!is.na(business_value_all)),
          "of",
          n_total_rows,
          "\n"
        )

        incProgress(0.1, detail = "Storing full calculated values...")

        # Create dataframe with calculated values
        calculated_df <- data.frame(
          id_property = data$id_property,
          property_type = data$property_type,
          property_value = property_value_all,
          business_value = business_value_all,
          stringsAsFactors = FALSE
        )

        # Store in the appropriate scenario slot
        if (scenario == "existing") {
          values$calculated_values_existing <- calculated_df
        } else if (scenario == "scenario_a") {
          values$calculated_values_scenario_a <- calculated_df
        } else if (scenario == "scenario_b") {
          values$calculated_values_scenario_b <- calculated_df
        }

        cat(
          "Stored calculated values for",
          scenario,
          ":",
          nrow(calculated_df),
          "properties\n"
        )

        incProgress(0.1, detail = "Creating preview table...")

        # Create preview table (showing first N rows for display)
        n_preview <- min(1000, n_total_rows)
        preview_indices <- 1:n_preview

        values$preview_data <- data.frame(
          id_property = data$id_property[preview_indices],
          property_area = round(property_area[preview_indices], 2),
          inflation_adjusted_base_value = round(
            rep(inflation_adjusted_base, n_preview),
            2
          ),
          product_of_all_feature_weights = round(
            product_weights[preview_indices],
            4
          ),
          structure_type_multiplier = round(
            structure_multipliers[preview_indices],
            4
          ),
          business_area = if ("business_area" %in% names(data)) {
            round(data$business_area[preview_indices], 2)
          } else {
            rep(NA, n_preview)
          },
          property_value = round(property_value_all[preview_indices], 2),
          business_value = round(business_value_all[preview_indices], 2),
          stringsAsFactors = FALSE
        )

        cat(
          "Created preview table with",
          nrow(values$preview_data),
          "rows (for display)\n"
        )
        cat("=== Calculation complete ===\n\n")

        incProgress(0.1, detail = "Complete!")
      })
    })

    # Display preview table
    output$preview_table <- DT::renderDataTable({
      req(values$preview_data)

      DT::datatable(
        values$preview_data,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons'
      ) %>%
        DT::formatRound(
          columns = c(
            'property_area',
            'inflation_adjusted_base_value',
            'property_value',
            'business_value'
          ),
          digits = 2
        ) %>%
        DT::formatRound(
          columns = c(
            'product_of_all_feature_weights',
            'structure_type_multiplier'
          ),
          digits = 4
        ) %>%
        DT::formatCurrency(
          columns = c('property_value', 'business_value'),
          currency = "",
          interval = 3,
          mark = ","
        )
    })

    # Configuration summary
    output$config_summary <- renderPrint({
      cat("=== Configuration Summary ===\n\n")

      for (scenario in c("Existing", "Scenario A", "Scenario B")) {
        scenario_key <- tolower(gsub(" ", "_", scenario))

        cat(scenario, ":\n")
        cat("  Base Value:", input[[paste0("base_value_", scenario_key)]], "\n")
        cat("  Inflation:", input[[paste0("inflation_", scenario_key)]], "%\n")
        cat(
          "  Inflation-Adjusted Base:",
          format(
            input[[paste0("base_value_", scenario_key)]] *
              (1 + input[[paste0("inflation_", scenario_key)]] / 100),
            digits = 2,
            big.mark = ","
          ),
          "\n"
        )
        cat(
          "  Area Weight:",
          input[[paste0("area_weight_", scenario_key)]],
          "\n\n"
        )
      }
    })

    # Collect all configurations for return
    get_all_configs <- reactive({
      # Collect weights from all inputs for each scenario
      scenarios <- list()

      for (scenario in c("existing", "scenario_a", "scenario_b")) {
        config <- list(
          base_value = input[[paste0("base_value_", scenario)]],
          inflation = input[[paste0("inflation_", scenario)]] / 100,
          area_weight = input[[paste0("area_weight_", scenario)]],
          feature_weights = list(),
          structure_weights = list()
        )

        # Collect all feature weights (including _na variables set to 0)
        all_features <- c(
          values$feature_columns$structure_features,
          values$feature_columns$utility_features,
          values$feature_columns$location_features,
          values$feature_columns$location_zones,
          values$feature_columns$property_characteristics,
          values$ward_columns
        )

        # Add _na variables to the complete list
        if (!is.null(values$all_feature_columns)) {
          na_features <- values$all_feature_columns[grepl(
            "(_na|_NA)$",
            values$all_feature_columns
          )]
          all_features <- c(all_features, na_features)
          all_features <- unique(all_features)
        }

        for (feat in all_features) {
          if (grepl("(_na|_NA)$", feat)) {
            # Set _na variables to weight 0
            config$feature_weights[[feat]] <- 0
          } else {
            feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
            input_id <- paste0("weight_", feat_safe, "_", scenario)
            if (!is.null(input[[input_id]])) {
              config$feature_weights[[feat]] <- input[[input_id]]
            }
          }
        }

        # Collect structure type weights (including _na variables set to 0)
        all_structures <- c(
          values$commercial_type_columns,
          values$institutional_type_columns
        )

        # Add _na structure variables
        if (!is.null(values$all_structure_columns)) {
          na_structures <- values$all_structure_columns[grepl(
            "(_na|_NA)$",
            values$all_structure_columns
          )]
          all_structures <- c(all_structures, na_structures)
          all_structures <- unique(all_structures)
        }

        for (struct in all_structures) {
          if (grepl("(_na|_NA)$", struct)) {
            # Set _na variables to weight 0
            config$structure_weights[[struct]] <- 0
          } else {
            # CRITICAL FIX: Sanitize the struct name before creating input_id
            struct_safe <- gsub("[^A-Za-z0-9_]", "_", struct)
            input_id <- paste0("weight_", struct_safe, "_", scenario)
            if (!is.null(input[[input_id]])) {
              config$structure_weights[[struct]] <- input[[input_id]]
            }
          }
        }

        scenarios[[scenario]] <- config
      }

      scenarios
    })

    # Create reactive to expose calculated values for Module 3
    get_calculated_values <- reactive({
      # Return all three scenarios as a named list
      list(
        existing = values$calculated_values_existing,
        scenario_a = values$calculated_values_scenario_a,
        scenario_b = values$calculated_values_scenario_b
      )
    })

    # Return BOTH configurations and calculated values
    return(
      list(
        configs = get_all_configs,
        calculated_values = get_calculated_values
      )
    )
  })
}
