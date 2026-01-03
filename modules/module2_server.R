# modules/module2_server.R
# Module 2: Value Parameters - Updated to use parameter tables

module2_server <- function(
  id,
  processed_data,
  param_additions = reactive(NULL),
  param_features = reactive(NULL),
  param_prop_struct_type = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ==========================================================================
    # REACTIVE VALUES
    # ==========================================================================
    values <- reactiveValues(
      # Column information from data
      feature_columns = NULL,
      all_feature_columns = NULL,
      commercial_type_columns = NULL,
      institutional_type_columns = NULL,
      all_structure_columns = NULL,
      ward_columns = NULL,

      # Default weights from parameters
      default_feature_weights = list(),
      default_structure_weights = list(),
      default_base_value = 231.859128,
      default_area_weight = 0.5,

      # Feature metadata for UI
      feature_metadata = NULL,

      # Scenario configurations
      existing_config = NULL,
      scenario_a_config = NULL,
      scenario_b_config = NULL,

      # Calculated values
      calculated_existing = NULL,
      calculated_scenario_a = NULL,
      calculated_scenario_b = NULL,

      # Preview data
      preview_data = NULL
    )

    # ==========================================================================
    # INITIALIZATION - Extract columns and build defaults from parameters
    # ==========================================================================

    # Initialize when data becomes available
    observe({
      req(processed_data())
      data <- processed_data()
      col_names <- names(data)

      # Categorize feature columns
      values$feature_columns <- categorize_feature_columns(col_names)
      values$all_feature_columns <- get_all_feature_columns(col_names)

      # Get structure type columns
      struct_cols <- get_structure_type_columns(col_names)
      values$commercial_type_columns <- struct_cols$commercial_type_columns
      values$institutional_type_columns <- struct_cols$institutional_type_columns
      values$all_structure_columns <- c(
        struct_cols$commercial_type_columns,
        struct_cols$institutional_type_columns
      )

      # Ward columns
      values$ward_columns <- col_names[grepl("^ward_number_\\d+$", col_names)]

      cat("=== Module 2 Initialization ===\n")
      cat("Feature columns found:", length(values$all_feature_columns), "\n")
      cat(
        "Commercial type columns:",
        length(values$commercial_type_columns),
        "\n"
      )
      cat(
        "Institutional type columns:",
        length(values$institutional_type_columns),
        "\n"
      )
      cat("Ward columns:", length(values$ward_columns), "\n")
    })

    # Build default weights from parameter tables
    observe({
      # Build feature weights from param_features
      # Now builds full column names like "has_water_Yes", "wall_material_Masonry"
      if (!is.null(param_features())) {
        values$default_feature_weights <- build_feature_weights_from_params(
          param_features()
        )
        values$feature_metadata <- get_feature_metadata(param_features())
        cat(
          "Feature weights loaded from parameters:",
          length(values$default_feature_weights),
          "\n"
        )
        # Debug: show a few example keys
        if (length(values$default_feature_weights) > 0) {
          cat(
            "Sample feature weight keys:",
            paste(
              head(names(values$default_feature_weights), 5),
              collapse = ", "
            ),
            "\n"
          )
        }
      }

      # Build structure weights from param_prop_struct_type
      if (!is.null(param_prop_struct_type())) {
        values$default_structure_weights <- build_structure_weights_from_params(
          param_prop_struct_type()
        )
        cat(
          "Structure weights loaded from parameters:",
          length(values$default_structure_weights),
          "\n"
        )
      }

      # Get base parameters from param_additions
      if (!is.null(param_additions())) {
        base_params <- get_base_params_from_additions(param_additions())
        values$default_base_value <- base_params$base_value
        values$default_area_weight <- base_params$area_weight
        cat("Base value from parameters:", values$default_base_value, "\n")
        cat("Area weight from parameters:", values$default_area_weight, "\n")
      }
    })

    # ==========================================================================
    # HELPER FUNCTION - Get default weight for a feature
    # Now works with full column names from build_feature_weights_from_params()
    # ==========================================================================

    get_feature_default <- function(feature_name) {
      # Direct lookup in pre-built weights (now keyed by full column name)
      # e.g., "has_water_Yes" or "wall_material_Masonry"
      if (feature_name %in% names(values$default_feature_weights)) {
        return(values$default_feature_weights[[feature_name]])
      }

      # Fallback: Try to look up in param_features directly
      # This handles cases where the column name might not exactly match
      pf <- param_features()
      if (!is.null(pf)) {
        # Try to split: "wall_condition_Good" → base="wall_condition", option="Good"
        # Use regex to split on the last underscore
        parts <- strsplit(feature_name, "_(?=[^_]+$)", perl = TRUE)[[1]]

        if (length(parts) == 2) {
          base_feature <- parts[1]
          option <- parts[2]

          match_row <- pf[
            pf$feature == base_feature & pf$feature_options == option,
          ]

          if (nrow(match_row) > 0) {
            return(match_row$weight_feature_option[1])
          }
        }
      }

      # Default to 0 if not found
      return(0)
    }

    get_structure_default <- function(struct_col_name) {
      # struct_col_name is like "commercial_type_Bank"
      if (struct_col_name %in% names(values$default_structure_weights)) {
        return(values$default_structure_weights[[struct_col_name]])
      }
      return(0)
    }

    # ==========================================================================
    # ADJUSTED BASE VALUE OUTPUTS
    # ==========================================================================

    output$adjusted_base_existing <- renderText({
      req(input$base_value_existing, input$inflation_existing)
      adjusted <- input$base_value_existing *
        (1 + input$inflation_existing / 100)
      format(round(adjusted, 2), big.mark = ",")
    })

    output$adjusted_base_scenario_a <- renderText({
      req(input$base_value_scenario_a, input$inflation_scenario_a)
      adjusted <- input$base_value_scenario_a *
        (1 + input$inflation_scenario_a / 100)
      format(round(adjusted, 2), big.mark = ",")
    })

    output$adjusted_base_scenario_b <- renderText({
      req(input$base_value_scenario_b, input$inflation_scenario_b)
      adjusted <- input$base_value_scenario_b *
        (1 + input$inflation_scenario_b / 100)
      format(round(adjusted, 2), big.mark = ",")
    })

    # ==========================================================================
    # DYNAMIC UI - Base Value Inputs (with parameter-based defaults)
    # ==========================================================================

    output$base_params_existing <- renderUI({
      tagList(
        numericInput(
          ns("base_value_existing"),
          label = "Base Value",
          value = values$default_base_value,
          step = 0.01
        ),
        numericInput(
          ns("inflation_existing"),
          label = "Inflation Adjustment %",
          value = 0,
          step = 0.1
        ),
        helpText("0% = no adjustment, 50% = 50% inflation"),
        h5("Inflation-Adjusted Base Value:"),
        verbatimTextOutput(ns("adjusted_base_existing")),
        numericInput(
          ns("area_weight_existing"),
          label = "Area Weight",
          value = values$default_area_weight,
          step = 0.01
        ),
        helpText("Default from city parameters")
      )
    })

    output$base_params_scenario_a <- renderUI({
      tagList(
        numericInput(
          ns("base_value_scenario_a"),
          label = "Base Value",
          value = values$default_base_value,
          step = 0.01
        ),
        numericInput(
          ns("inflation_scenario_a"),
          label = "Inflation Adjustment %",
          value = 0,
          step = 0.1
        ),
        helpText("0% = no adjustment, 50% = 50% inflation"),
        h5("Inflation-Adjusted Base Value:"),
        verbatimTextOutput(ns("adjusted_base_scenario_a")),
        numericInput(
          ns("area_weight_scenario_a"),
          label = "Area Weight",
          value = values$default_area_weight,
          step = 0.01
        ),
        helpText("Default from city parameters")
      )
    })

    output$base_params_scenario_b <- renderUI({
      tagList(
        numericInput(
          ns("base_value_scenario_b"),
          label = "Base Value",
          value = values$default_base_value,
          step = 0.01
        ),
        numericInput(
          ns("inflation_scenario_b"),
          label = "Inflation Adjustment %",
          value = 0,
          step = 0.1
        ),
        helpText("0% = no adjustment, 50% = 50% inflation"),
        h5("Inflation-Adjusted Base Value:"),
        verbatimTextOutput(ns("adjusted_base_scenario_b")),
        numericInput(
          ns("area_weight_scenario_b"),
          label = "Area Weight",
          value = values$default_area_weight,
          step = 0.01
        ),
        helpText("Default from city parameters")
      )
    })

    # ==========================================================================
    # DYNAMIC UI - Feature Weight Generation
    # ==========================================================================

    generate_feature_ui <- function(scenario_suffix) {
      req(values$feature_columns)

      # Helper to create feature input boxes
      create_feature_inputs <- function(feature_list, title) {
        if (length(feature_list) == 0) {
          return(NULL)
        }

        box(
          title = title,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          status = "info",
          solidHeader = FALSE,
          lapply(feature_list, function(feat) {
            feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
            default_val <- get_feature_default(feat)

            # Try to get display label from metadata
            display_name <- feat
            if (!is.null(values$feature_metadata)) {
              # Match using full_col_name if available
              if ("full_col_name" %in% names(values$feature_metadata)) {
                meta_match <- values$feature_metadata[
                  values$feature_metadata$full_col_name == feat,
                ]
              } else {
                meta_match <- values$feature_metadata[
                  values$feature_metadata$feature == feat,
                ]
              }
              if (nrow(meta_match) > 0) {
                display_name <- meta_match$feature_label[1]
              }
            }

            fluidRow(
              column(
                8,
                p(display_name, style = "margin-top: 5px; font-size: 12px;")
              ),
              column(
                4,
                numericInput(
                  ns(paste0("weight_", feat_safe, "_", scenario_suffix)),
                  label = NULL,
                  value = default_val,
                  step = 1,
                  width = "100%"
                )
              )
            )
          })
        )
      }

      tagList(
        create_feature_inputs(
          values$feature_columns$structure_features,
          "Structure Features"
        ),
        create_feature_inputs(
          values$feature_columns$utility_features,
          "Utility Features"
        ),
        create_feature_inputs(
          values$feature_columns$location_features,
          "Location Features"
        ),
        create_feature_inputs(
          values$feature_columns$location_zones,
          "Location Zones"
        ),
        create_feature_inputs(
          values$ward_columns,
          "Ward Weights"
        )
      )
    }

    # Render feature UIs
    output$features_ui_existing <- renderUI({
      req(values$feature_columns)
      generate_feature_ui("existing")
    })

    output$features_ui_scenario_a <- renderUI({
      req(values$feature_columns)
      generate_feature_ui("scenario_a")
    })

    output$features_ui_scenario_b <- renderUI({
      req(values$feature_columns)
      generate_feature_ui("scenario_b")
    })

    # ==========================================================================
    # DYNAMIC UI - Structure Type Weight Generation
    # ==========================================================================

    generate_structure_ui <- function(scenario_suffix) {
      req(values$commercial_type_columns, values$institutional_type_columns)

      create_structure_inputs <- function(columns, title) {
        if (length(columns) == 0) {
          return(NULL)
        }

        box(
          title = title,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          status = "warning",
          solidHeader = FALSE,
          lapply(columns, function(col) {
            col_safe <- gsub("[^A-Za-z0-9_]", "_", col)
            # Extract display name from column
            display_name <- gsub("^(commercial|institutional)_type_", "", col)
            default_val <- get_structure_default(col)

            fluidRow(
              column(
                8,
                p(display_name, style = "margin-top: 5px; font-size: 12px;")
              ),
              column(
                4,
                numericInput(
                  ns(paste0("weight_", col_safe, "_", scenario_suffix)),
                  label = NULL,
                  value = default_val,
                  min = -100,
                  max = 5000,
                  step = 1,
                  width = "100%"
                )
              )
            )
          })
        )
      }

      tagList(
        create_structure_inputs(
          values$commercial_type_columns,
          "Commercial Structure Types"
        ),
        create_structure_inputs(
          values$institutional_type_columns,
          "Institutional Structure Types"
        )
      )
    }

    # Render structure UIs
    output$structures_ui_existing <- renderUI({
      req(values$commercial_type_columns)
      generate_structure_ui("existing")
    })

    output$structures_ui_scenario_a <- renderUI({
      req(values$commercial_type_columns)
      generate_structure_ui("scenario_a")
    })

    output$structures_ui_scenario_b <- renderUI({
      req(values$commercial_type_columns)
      generate_structure_ui("scenario_b")
    })

    # ==========================================================================
    # CALCULATE VALUES
    # ==========================================================================

    observeEvent(input$calculate_values, {
      req(processed_data())
      data <- processed_data()
      n_rows <- nrow(data)

      withProgress(message = "Calculating property values...", value = 0, {
        for (scenario in c("existing", "scenario_a", "scenario_b")) {
          incProgress(1 / 3, detail = paste("Processing", scenario))

          # Get base parameters
          base_value <- input[[paste0("base_value_", scenario)]] %||%
            values$default_base_value
          inflation <- input[[paste0("inflation_", scenario)]] %||% 0
          area_weight <- input[[paste0("area_weight_", scenario)]] %||%
            values$default_area_weight

          inflation_adjusted_base <- base_value * (1 + inflation / 100)

          # Collect feature weights
          feature_weights <- list()
          all_features <- c(
            values$feature_columns$structure_features,
            values$feature_columns$utility_features,
            values$feature_columns$location_features,
            values$feature_columns$location_zones,
            values$ward_columns
          )

          # Include _NA features with weight 0
          if (!is.null(values$all_feature_columns)) {
            na_features <- values$all_feature_columns[grepl(
              "(_na|_NA)$",
              values$all_feature_columns
            )]
            all_features <- unique(c(all_features, na_features))
          }

          for (feat in all_features) {
            if (grepl("(_na|_NA)$", feat)) {
              feature_weights[[feat]] <- 0
            } else {
              feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
              input_id <- paste0("weight_", feat_safe, "_", scenario)
              feature_weights[[feat]] <- input[[input_id]] %||% 0
            }
          }

          # Collect structure weights
          structure_weights <- list()
          all_structures <- c(
            values$commercial_type_columns,
            values$institutional_type_columns
          )

          if (!is.null(values$all_structure_columns)) {
            na_structures <- values$all_structure_columns[grepl(
              "(_na|_NA)$",
              values$all_structure_columns
            )]
            all_structures <- unique(c(all_structures, na_structures))
          }

          for (struct in all_structures) {
            if (grepl("(_na|_NA)$", struct)) {
              structure_weights[[struct]] <- 0
            } else {
              struct_safe <- gsub("[^A-Za-z0-9_]", "_", struct)
              input_id <- paste0("weight_", struct_safe, "_", scenario)
              structure_weights[[struct]] <- input[[input_id]] %||% 0
            }
          }

          # Calculate feature product weights
          product_weights <- rep(1, n_rows)
          for (feat in names(feature_weights)) {
            if (feat %in% names(data)) {
              weight <- feature_weights[[feat]]
              feat_values <- data[[feat]]
              feat_values[is.na(feat_values)] <- 0
              product_weights <- product_weights *
                (((weight / 100) + 1)^feat_values)
            }
          }

          # Calculate structure multipliers
          structure_multipliers <- rep(1, n_rows)
          for (struct in names(structure_weights)) {
            if (struct %in% names(data)) {
              weight <- structure_weights[[struct]]
              struct_values <- data[[struct]]
              struct_values[is.na(struct_values)] <- 0
              structure_multipliers <- structure_multipliers *
                ifelse(struct_values == 1, (weight / 100 + 1), 1)
            }
          }

          # Calculate property values
          property_area <- data$property_area %||% rep(NA, n_rows)
          business_area <- data$business_area %||% rep(NA, n_rows)

          property_values <- ifelse(
            !is.na(property_area) & property_area > 0,
            inflation_adjusted_base *
              (property_area^area_weight) *
              product_weights *
              structure_multipliers,
            NA
          )

          business_values <- ifelse(
            !is.na(business_area) & business_area > 0,
            inflation_adjusted_base *
              (business_area^area_weight) *
              product_weights *
              structure_multipliers,
            NA
          )

          # Store calculated values
          calc_result <- data.frame(
            id_property = data$id_property,
            property_value = property_values,
            business_value = business_values,
            stringsAsFactors = FALSE
          )

          if (scenario == "existing") {
            values$calculated_existing <- calc_result
          } else if (scenario == "scenario_a") {
            values$calculated_scenario_a <- calc_result
          } else {
            values$calculated_scenario_b <- calc_result
          }
        }
      })

      showNotification(
        "Property values calculated for all scenarios!",
        type = "message"
      )
    })

    # ==========================================================================
    # PREVIEW TABLE
    # ==========================================================================

    observeEvent(input$calculate_preview, {
      req(processed_data(), input$preview_scenario)
      data <- processed_data()
      scenario <- input$preview_scenario
      n_rows <- nrow(data)

      # Get parameters for preview scenario
      base_value <- input[[paste0("base_value_", scenario)]] %||%
        values$default_base_value
      inflation <- input[[paste0("inflation_", scenario)]] %||% 0
      area_weight <- input[[paste0("area_weight_", scenario)]] %||%
        values$default_area_weight

      inflation_adjusted_base <- base_value * (1 + inflation / 100)

      # Collect feature weights for preview
      feature_weights <- list()
      all_features <- c(
        values$feature_columns$structure_features,
        values$feature_columns$utility_features,
        values$feature_columns$location_features,
        values$feature_columns$location_zones,
        values$ward_columns
      )

      for (feat in all_features) {
        feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
        input_id <- paste0("weight_", feat_safe, "_", scenario)
        feature_weights[[feat]] <- input[[input_id]] %||% 0
      }

      # Calculate product weights
      product_weights <- rep(1, n_rows)
      for (feat in names(feature_weights)) {
        if (feat %in% names(data)) {
          weight <- feature_weights[[feat]]
          feat_values <- data[[feat]]
          feat_values[is.na(feat_values)] <- 0
          product_weights <- product_weights *
            (((weight / 100) + 1)^feat_values)
        }
      }

      # Collect structure weights
      structure_weights <- list()
      all_structures <- c(
        values$commercial_type_columns,
        values$institutional_type_columns
      )

      for (struct in all_structures) {
        struct_safe <- gsub("[^A-Za-z0-9_]", "_", struct)
        input_id <- paste0("weight_", struct_safe, "_", scenario)
        structure_weights[[struct]] <- input[[input_id]] %||% 0
      }

      # Calculate structure multipliers
      structure_multipliers <- rep(1, n_rows)
      for (struct in names(structure_weights)) {
        if (struct %in% names(data)) {
          weight <- structure_weights[[struct]]
          struct_values <- data[[struct]]
          struct_values[is.na(struct_values)] <- 0
          structure_multipliers <- structure_multipliers *
            ifelse(struct_values == 1, (weight / 100 + 1), 1)
        }
      }

      # Build preview data
      property_area <- data$property_area %||% rep(NA, n_rows)
      business_area <- data$business_area %||% rep(NA, n_rows)

      property_values <- ifelse(
        !is.na(property_area) & property_area > 0,
        inflation_adjusted_base *
          (property_area^area_weight) *
          product_weights *
          structure_multipliers,
        NA
      )

      business_values <- ifelse(
        !is.na(business_area) & business_area > 0,
        inflation_adjusted_base *
          (business_area^area_weight) *
          product_weights *
          structure_multipliers,
        NA
      )

      values$preview_data <- data.frame(
        id_property = data$id_property,
        property_area = round(property_area, 2),
        inflation_adjusted_base_value = round(inflation_adjusted_base, 2),
        product_of_all_feature_weights = round(product_weights, 4),
        structure_type_multiplier = round(structure_multipliers, 4),
        business_area = round(business_area, 2),
        property_value = round(property_values, 2),
        business_value = round(business_values, 2),
        stringsAsFactors = FALSE
      )
    })

    output$preview_table <- DT::renderDataTable({
      req(values$preview_data)
      DT::datatable(
        values$preview_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        ),
        filter = "top",
        rownames = FALSE
      )
    })

    # ==========================================================================
    # COLLECT CONFIGURATIONS FOR RETURN
    # ==========================================================================

    get_all_configs <- reactive({
      scenarios <- list()

      for (scenario in c("existing", "scenario_a", "scenario_b")) {
        config <- list(
          base_value = input[[paste0("base_value_", scenario)]] %||%
            values$default_base_value,
          inflation = (input[[paste0("inflation_", scenario)]] %||% 0) / 100,
          area_weight = input[[paste0("area_weight_", scenario)]] %||%
            values$default_area_weight,
          feature_weights = list(),
          structure_weights = list()
        )

        # Collect feature weights
        all_features <- c(
          values$feature_columns$structure_features,
          values$feature_columns$utility_features,
          values$feature_columns$location_features,
          values$feature_columns$location_zones,
          values$ward_columns
        )

        if (!is.null(values$all_feature_columns)) {
          na_features <- values$all_feature_columns[grepl(
            "(_na|_NA)$",
            values$all_feature_columns
          )]
          all_features <- unique(c(all_features, na_features))
        }

        for (feat in all_features) {
          if (grepl("(_na|_NA)$", feat)) {
            config$feature_weights[[feat]] <- 0
          } else {
            feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
            input_id <- paste0("weight_", feat_safe, "_", scenario)
            config$feature_weights[[feat]] <- input[[input_id]] %||% 0
          }
        }

        # Collect structure weights
        all_structures <- c(
          values$commercial_type_columns,
          values$institutional_type_columns
        )

        for (struct in all_structures) {
          struct_safe <- gsub("[^A-Za-z0-9_]", "_", struct)
          input_id <- paste0("weight_", struct_safe, "_", scenario)
          config$structure_weights[[struct]] <- input[[input_id]] %||% 0
        }

        scenarios[[scenario]] <- config
      }

      scenarios
    })

    get_calculated_values <- reactive({
      list(
        existing = values$calculated_existing,
        scenario_a = values$calculated_scenario_a,
        scenario_b = values$calculated_scenario_b
      )
    })

    # ==========================================================================
    # SAVE/LOAD CONFIGURATION HANDLERS
    # ==========================================================================

    # Download handlers - using collect_module2_config from module2_config_functions.R
    output$download_config_existing <- downloadHandler(
      filename = function() {
        paste0(
          "module2_existing_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".json"
        )
      },
      content = function(file) {
        config <- collect_module2_config(
          input,
          "existing",
          values$feature_columns,
          values$commercial_type_columns,
          values$institutional_type_columns,
          values$ward_columns
        )
        writeLines(
          jsonlite::toJSON(config, auto_unbox = TRUE, pretty = TRUE),
          file
        )
      }
    )

    output$download_config_scenario_a <- downloadHandler(
      filename = function() {
        paste0(
          "module2_scenario_a_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".json"
        )
      },
      content = function(file) {
        config <- collect_module2_config(
          input,
          "scenario_a",
          values$feature_columns,
          values$commercial_type_columns,
          values$institutional_type_columns,
          values$ward_columns
        )
        writeLines(
          jsonlite::toJSON(config, auto_unbox = TRUE, pretty = TRUE),
          file
        )
      }
    )

    output$download_config_scenario_b <- downloadHandler(
      filename = function() {
        paste0(
          "module2_scenario_b_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".json"
        )
      },
      content = function(file) {
        config <- collect_module2_config(
          input,
          "scenario_b",
          values$feature_columns,
          values$commercial_type_columns,
          values$institutional_type_columns,
          values$ward_columns
        )
        writeLines(
          jsonlite::toJSON(config, auto_unbox = TRUE, pretty = TRUE),
          file
        )
      }
    )

    # Upload handlers - using apply_module2_config from module2_config_functions.R
    observeEvent(input$upload_config_existing, {
      req(input$upload_config_existing)
      tryCatch(
        {
          config <- load_module2_config(input$upload_config_existing$datapath)
          apply_module2_config(
            session,
            config,
            "existing",
            values$feature_columns,
            values$commercial_type_columns,
            values$institutional_type_columns,
            values$ward_columns
          )
          showNotification(
            "Existing configuration uploaded!",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        }
      )
    })

    observeEvent(input$upload_config_scenario_a, {
      req(input$upload_config_scenario_a)
      tryCatch(
        {
          config <- load_module2_config(input$upload_config_scenario_a$datapath)
          apply_module2_config(
            session,
            config,
            "scenario_a",
            values$feature_columns,
            values$commercial_type_columns,
            values$institutional_type_columns,
            values$ward_columns
          )
          showNotification(
            "Scenario A configuration uploaded!",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        }
      )
    })

    observeEvent(input$upload_config_scenario_b, {
      req(input$upload_config_scenario_b)
      tryCatch(
        {
          config <- load_module2_config(input$upload_config_scenario_b$datapath)
          apply_module2_config(
            session,
            config,
            "scenario_b",
            values$feature_columns,
            values$commercial_type_columns,
            values$institutional_type_columns,
            values$ward_columns
          )
          showNotification(
            "Scenario B configuration uploaded!",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        }
      )
    })

    # ==========================================================================
    # RETURN VALUES
    # ==========================================================================

    return(list(
      configs = get_all_configs,
      calculated_values = get_calculated_values
    ))
  })
}
