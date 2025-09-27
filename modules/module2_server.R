# modules/module2_server.R

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
      defaults = NULL
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
    
    # Initialize when processed data is available
    observe({
      req(processed_data())
      data <- processed_data()
      
      # Get feature columns from the processed data
      all_columns <- names(data)
      
      # Remove non-feature columns
      feature_cols <- all_columns[!all_columns %in% c("id_property", "coordinate_lat", 
                                                      "coordinate_lng", "property_area",
                                                      "made_payment", "business_name",
                                                      "business_area", "business_category",
                                                      "business_sub_category")]
      
      # Group features by category
      values$feature_columns <- group_feature_columns(feature_cols)
      values$structure_columns <- get_structure_type_columns(feature_cols)
      
      # Separate commercial and institutional types
      values$commercial_type_columns <- feature_cols[grepl("^commercial_type_", feature_cols)]
      values$institutional_type_columns <- feature_cols[grepl("^institutional_type_", feature_cols)]
      
      values$ward_columns <- feature_cols[grepl("^ward_", feature_cols)]
    })
    
    # Calculate inflation-adjusted base values
    output$adjusted_base_existing <- renderText({
      base <- input$base_value_existing
      inflation <- input$inflation_existing
      adjusted <- base * (1 + inflation/100)
      format(adjusted, digits = 2, big.mark = ",")
    })
    
    output$adjusted_base_scenario_a <- renderText({
      base <- input$base_value_scenario_a
      inflation <- input$inflation_scenario_a
      adjusted <- base * (1 + inflation/100)
      format(adjusted, digits = 2, big.mark = ",")
    })
    
    output$adjusted_base_scenario_b <- renderText({
      base <- input$base_value_scenario_b
      inflation <- input$inflation_scenario_b
      adjusted <- base * (1 + inflation/100)
      format(adjusted, digits = 2, big.mark = ",")
    })
    
    # Copy functions
    observeEvent(input$copy_existing_to_a, {
      updateNumericInput(session, "base_value_scenario_a", value = input$base_value_existing)
      updateNumericInput(session, "inflation_scenario_a", value = input$inflation_existing)
      updateNumericInput(session, "area_weight_scenario_a", value = input$area_weight_existing)
      
      # Copy all weight values
      values$scenario_a_config <- values$existing_config
      
      showNotification("Copied Existing Scenario to Scenario A", type = "message")
    })
    
    observeEvent(input$copy_existing_to_b, {
      updateNumericInput(session, "base_value_scenario_b", value = input$base_value_existing)
      updateNumericInput(session, "inflation_scenario_b", value = input$inflation_existing)
      updateNumericInput(session, "area_weight_scenario_b", value = input$area_weight_existing)
      
      values$scenario_b_config <- values$existing_config
      
      showNotification("Copied Existing Scenario to Scenario B", type = "message")
    })
    
    observeEvent(input$copy_a_to_b, {
      updateNumericInput(session, "base_value_scenario_b", value = input$base_value_scenario_a)
      updateNumericInput(session, "inflation_scenario_b", value = input$inflation_scenario_a)
      updateNumericInput(session, "area_weight_scenario_b", value = input$area_weight_scenario_a)
      
      values$scenario_b_config <- values$scenario_a_config
      
      showNotification("Copied Scenario A to Scenario B", type = "message")
    })
    
    observeEvent(input$reset_all, {
      defaults <- get_default_weights()
      
      # Reset all scenarios
      for (scenario in c("existing", "scenario_a", "scenario_b")) {
        updateNumericInput(session, paste0("base_value_", scenario), value = defaults$base_value)
        updateNumericInput(session, paste0("inflation_", scenario), value = 0)
        updateNumericInput(session, paste0("area_weight_", scenario), value = 0.5)
      }
      
      # Reset the stored configurations
      values$existing_config <- defaults
      values$scenario_a_config <- defaults
      values$scenario_b_config <- defaults
      
      # Force UI refresh by toggling a reactive value
      values$refresh_ui <- runif(1)
      
      showNotification("Reset all scenarios to default values", type = "message")
    })
    
    # Update the generate_feature_ui function to use defaults:
    # Update the generate_feature_ui function to properly look up defaults:
    generate_feature_ui <- function(scenario_suffix) {
      renderUI({
        req(values$feature_columns)
        
        # Get the config for this scenario
        config <- switch(scenario_suffix,
                         "existing" = values$existing_config,
                         "scenario_a" = values$scenario_a_config,
                         "scenario_b" = values$scenario_b_config
        )
        
        tagList(
          # Structure Features
          if (length(values$feature_columns$structure_features) > 0) {
            box(
              title = "Structure Features",
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              status = "info",
              solidHeader = FALSE,
              lapply(values$feature_columns$structure_features, function(feat) {
                # Try to get value from config first, then from defaults
                default_val <- config$feature_weights[[feat]]
                if (is.null(default_val)) {
                  default_val <- values$defaults$feature_weights[[feat]]
                }
                if (is.null(default_val)) {
                  default_val <- 0
                }
                
                fluidRow(
                  column(8, p(feat, style = "margin-top: 5px; font-size: 12px;")),
                  column(4,
                         numericInput(ns(paste0("weight_", feat, "_", scenario_suffix)),
                                      label = NULL,
                                      value = default_val,
                                      step = 1,
                                      width = "100%")
                  )
                )
              })
            )
          },
          
          # Utility Features
          if (length(values$feature_columns$utility_features) > 0) {
            box(
              title = "Utility Features",
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              status = "info",
              solidHeader = FALSE,
              lapply(values$feature_columns$utility_features, function(feat) {
                # Try to get value from config first, then from defaults
                default_val <- config$feature_weights[[feat]]
                if (is.null(default_val)) {
                  default_val <- values$defaults$feature_weights[[feat]]
                }
                if (is.null(default_val)) {
                  default_val <- 0
                }
                
                fluidRow(
                  column(8, p(feat, style = "margin-top: 5px; font-size: 12px;")),
                  column(4,
                         numericInput(ns(paste0("weight_", feat, "_", scenario_suffix)),
                                      label = NULL,
                                      value = default_val,
                                      step = 1,
                                      width = "100%")
                  )
                )
              })
            )
          },
          
          # Location Features
          if (length(values$feature_columns$location_features) > 0) {
            box(
              title = "Location Features",
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              status = "info",
              solidHeader = FALSE,
              lapply(values$feature_columns$location_features, function(feat) {
                # Try to get value from config first, then from defaults
                default_val <- config$feature_weights[[feat]]
                if (is.null(default_val)) {
                  default_val <- values$defaults$feature_weights[[feat]]
                }
                if (is.null(default_val)) {
                  default_val <- 0
                }
                
                fluidRow(
                  column(8, p(feat, style = "margin-top: 5px; font-size: 12px;")),
                  column(4,
                         numericInput(ns(paste0("weight_", feat, "_", scenario_suffix)),
                                      label = NULL,
                                      value = default_val,
                                      step = 1,
                                      width = "100%")
                  )
                )
              })
            )
          },
          
          # Property Characteristics
          if (length(values$feature_columns$property_characteristics) > 0) {
            box(
              title = "Property Characteristics",
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              status = "info",
              solidHeader = FALSE,
              lapply(values$feature_columns$property_characteristics, function(feat) {
                # Try to get value from config first, then from defaults
                default_val <- config$feature_weights[[feat]]
                if (is.null(default_val)) {
                  default_val <- values$defaults$feature_weights[[feat]]
                }
                if (is.null(default_val)) {
                  default_val <- 0
                }
                
                fluidRow(
                  column(8, p(feat, style = "margin-top: 5px; font-size: 12px;")),
                  column(4,
                         numericInput(ns(paste0("weight_", feat, "_", scenario_suffix)),
                                      label = NULL,
                                      value = default_val,
                                      step = 1,
                                      width = "100%")
                  )
                )
              })
            )
          },
          
          # Location Zones Features
          if (length(values$feature_columns$location_zones) > 0) {
            box(
              title = "Location Zones",
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              status = "info",
              solidHeader = FALSE,
              lapply(values$feature_columns$location_zones, function(feat) {
                # Try to get value from config first, then from defaults
                default_val <- config$feature_weights[[feat]]
                if (is.null(default_val)) {
                  default_val <- values$defaults$feature_weights[[feat]]
                }
                if (is.null(default_val)) {
                  default_val <- 0
                }
                
                fluidRow(
                  column(8, p(feat, style = "margin-top: 5px; font-size: 12px;")),
                  column(4,
                         numericInput(ns(paste0("weight_", feat, "_", scenario_suffix)),
                                      label = NULL,
                                      value = default_val,
                                      step = 1,
                                      width = "100%")
                  )
                )
              })
            )
          },

          # Ward Features
          if (length(values$ward_columns) > 0) {
            box(
              title = "Ward Features",
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              status = "info",
              solidHeader = FALSE,
              lapply(values$ward_columns, function(feat) {
                # Try to get value from config first, then from defaults
                default_val <- config$feature_weights[[feat]]
                if (is.null(default_val)) {
                  default_val <- values$defaults$feature_weights[[feat]]
                }
                if (is.null(default_val)) {
                  default_val <- 0
                }
                
                fluidRow(
                  column(8, p(feat, style = "margin-top: 5px; font-size: 12px;")),
                  column(4,
                         numericInput(ns(paste0("weight_", feat, "_", scenario_suffix)),
                                      label = NULL,
                                      value = default_val,
                                      step = 1,
                                      width = "100%")
                  )
                )
              })
            )
          }
        )
      })
    }
    
    # Make sure the helper function is defined
    `%||%` <- function(x, y) {
      if (is.null(x)) y else x
    }    
    
    # Update the generate_structure_ui function similarly:
    generate_structure_ui <- function(scenario_suffix) {
      renderUI({
        # Get the config for this scenario
        config <- switch(scenario_suffix,
                         "existing" = values$existing_config,
                         "scenario_a" = values$scenario_a_config,
                         "scenario_b" = values$scenario_b_config
        )
        
        tagList(
          # Commercial Types
          if (length(values$commercial_type_columns) > 0) {
            tagList(
              h5("Commercial Types"),
              lapply(values$commercial_type_columns, function(struct) {
                # Get default value
                default_val <- config$structure_weights[[struct]] %||% 
                  values$defaults$structure_weights[[struct]] %||% 0
                
                # Clean up the display name
                display_name <- gsub("commercial_type_", "", struct)
                fluidRow(
                  column(8, p(display_name, style = "margin-top: 5px;")),
                  column(4,
                         numericInput(ns(paste0("weight_", struct, "_", scenario_suffix)),
                                      label = NULL,
                                      value = default_val,
                                      step = 10,
                                      width = "100%")
                  )
                )
              }),
              hr()
            )
          },
          
          # Institutional Types
          if (length(values$institutional_type_columns) > 0) {
            tagList(
              h5("Institutional Types"),
              lapply(values$institutional_type_columns, function(struct) {
                # Get default value
                default_val <- config$structure_weights[[struct]] %||% 
                  values$defaults$structure_weights[[struct]] %||% 0
                
                # Clean up the display name
                display_name <- gsub("institutional_type_", "", struct)
                fluidRow(
                  column(8, p(display_name, style = "margin-top: 5px;")),
                  column(4,
                         numericInput(ns(paste0("weight_", struct, "_", scenario_suffix)),
                                      label = NULL,
                                      value = default_val,
                                      step = 10,
                                      width = "100%")
                  )
                )
              })
            )
          }
        )
      })
    }
    
    # Create UI outputs for each scenario
    output$features_ui_existing <- generate_feature_ui("existing")
    output$features_ui_scenario_a <- generate_feature_ui("scenario_a")
    output$features_ui_scenario_b <- generate_feature_ui("scenario_b")
    
    output$structure_ui_existing <- generate_structure_ui("existing")
    output$structure_ui_scenario_a <- generate_structure_ui("scenario_a")
    output$structure_ui_scenario_b <- generate_structure_ui("scenario_b")
    
    # Update configurations when base parameters change
    observe({
      values$existing_config$base_value <- input$base_value_existing
      values$existing_config$inflation <- input$inflation_existing / 100
      values$existing_config$area_weight <- input$area_weight_existing
    })
    
    observe({
      values$scenario_a_config$base_value <- input$base_value_scenario_a
      values$scenario_a_config$inflation <- input$inflation_scenario_a / 100
      values$scenario_a_config$area_weight <- input$area_weight_scenario_a
    })
    
    observe({
      values$scenario_b_config$base_value <- input$base_value_scenario_b
      values$scenario_b_config$inflation <- input$inflation_scenario_b / 100
      values$scenario_b_config$area_weight <- input$area_weight_scenario_b
    })
    
    # Calculate preview data
    observeEvent(input$calculate_preview, {
      req(processed_data())
      
      withProgress(message = 'Calculating preview...', value = 0, {
        data <- processed_data()
        scenario <- input$preview_scenario
        n_rows <- min(input$preview_rows, nrow(data))
        
        # Sample data
        preview_data <- data[1:n_rows, ]
        
        # Get configuration for selected scenario
        base_value <- input[[paste0("base_value_", scenario)]]
        inflation <- input[[paste0("inflation_", scenario)]]
        area_weight <- input[[paste0("area_weight_", scenario)]]
        
        # Calculate inflation-adjusted base value
        inflation_adjusted_base <- base_value * (1 + inflation/100)
        
        # Debug output
        print(paste("Base value:", base_value))
        print(paste("Inflation:", inflation))
        print(paste("Area weight:", area_weight))
        print(paste("Inflation-adjusted base:", inflation_adjusted_base))
        
        incProgress(0.3, detail = "Calculating feature weights...")
        
        # Calculate product of all feature weights for each row
        all_features <- c(values$feature_columns$structure_features,
                          values$feature_columns$utility_features,
                          values$feature_columns$location_features,
                          values$feature_columns$location_zones,
                          values$feature_columns$property_characteristics,
                          values$ward_columns)
        
        product_weights <- rep(1, n_rows)
        
        for (feat in all_features) {
          input_id <- paste0("weight_", feat, "_", scenario)
          weight <- input[[input_id]]
          
          if (!is.null(weight) && feat %in% names(preview_data)) {
            # Apply weight where feature = 1
            feature_multiplier <- ifelse(preview_data[[feat]] == 1, 
                                         (weight/100 + 1), 
                                         1)
            product_weights <- product_weights * feature_multiplier
          }
        }
        
        # Debug output
        print(paste("Product weights range:", min(product_weights), "-", max(product_weights)))
        print(paste("Number of NA in product weights:", sum(is.na(product_weights))))
        
        incProgress(0.2, detail = "Calculating structure type weights...")
        
        # Calculate structure type weights (find the single applicable type per property)
        structure_weights <- rep(0, n_rows)
        
        all_structures <- c(values$commercial_type_columns,
                            values$institutional_type_columns)
        
        # Debug
        print(paste("Number of structure columns:", length(all_structures)))
        
        # ADD THE NEW DEBUGGING CODE HERE:
        print("=== DEBUGGING STRUCTURE COLUMNS ===")
        print(paste("Looking for structure columns in:", length(all_structures), "expected columns"))
        
        # Check what columns are actually in preview_data
        all_cols <- names(preview_data)
        print(paste("Total columns in preview_data:", length(all_cols)))
        
        # Look for any columns with 'commercial' or 'institutional' in the name
        commercial_cols_in_data <- all_cols[grepl("commercial", all_cols, ignore.case = TRUE)]
        institutional_cols_in_data <- all_cols[grepl("institutional", all_cols, ignore.case = TRUE)]
        
        print(paste("Commercial columns found:", length(commercial_cols_in_data)))
        if (length(commercial_cols_in_data) > 0) {
          print(paste("  ", commercial_cols_in_data[1:min(5, length(commercial_cols_in_data))]))
        }
        
        print(paste("Institutional columns found:", length(institutional_cols_in_data)))
        if (length(institutional_cols_in_data) > 0) {
          print(paste("  ", institutional_cols_in_data[1:min(5, length(institutional_cols_in_data))]))
        }
        
        # Check if the expected structure columns exist
        print("Checking expected vs actual columns:")
        for (i in 1:min(5, length(all_structures))) {
          struct <- all_structures[i]
          exists_in_data <- struct %in% names(preview_data)
          print(paste("  Expected:", struct, "- Found:", exists_in_data))
        }
        
        # Also check property_type column
        if ("property_type" %in% names(preview_data)) {
          print("Property types in data:")
          print(table(preview_data$property_type))
        }
        
        print("=== END STRUCTURE DEBUGGING ===")        
        
        # Add this right after the "=== END STRUCTURE DEBUGGING ===" line and before creating the structure_matrix:
        
        print("=== CHECKING STRUCTURE COLUMN VALUES ===")
        # Check a few structure columns to see what values they contain
        for (i in 1:min(3, length(all_structures))) {
          struct <- all_structures[i]
          if (struct %in% names(preview_data)) {
            col_values <- preview_data[[struct]]
            print(paste("Column:", struct))
            print(paste("  Unique values:", paste(unique(col_values), collapse=", ")))
            print(paste("  Number of 1s:", sum(col_values == 1, na.rm = TRUE)))
            print(paste("  Number of 0s:", sum(col_values == 0, na.rm = TRUE)))
            print(paste("  Number of NAs:", sum(is.na(col_values))))
            print(paste("  First 10 values:", paste(col_values[1:10], collapse=", ")))
          }
        }
        print("=== END STRUCTURE VALUE CHECK ===")
        
        # Debug: Check if we have any non-zero weights
        print(paste("Non-zero structure weights:", sum(structure_weights != 0)))
        print(paste("First 10 structure weights:", paste(structure_weights[1:10], collapse=", ")))
        
        
        # Create a matrix of structure type indicators
        structure_matrix <- matrix(0, nrow = n_rows, ncol = length(all_structures))
        weight_vector <- numeric(length(all_structures))
        
        for (j in seq_along(all_structures)) {
          struct <- all_structures[j]
          
          if (struct %in% names(preview_data)) {
            # Get the column values and handle NAs properly
            col_values <- preview_data[[struct]]
            # Convert to 1 where value is 1, 0 everywhere else (including NAs)
            structure_matrix[, j] <- ifelse(!is.na(col_values) & col_values == 1, 1, 0)
            
            # Get the weight
            input_id <- paste0("weight_", struct, "_", scenario)
            weight <- input[[input_id]]
            
            if (is.null(weight)) {
              weight <- values$defaults$structure_weights[[struct]]
              if (is.null(weight)) weight <- 0
            }
            
            weight_vector[j] <- weight
            
            # Debug print
            if (sum(structure_matrix[, j]) > 0) {
              print(paste("Structure", struct, "applies to", sum(structure_matrix[, j]), "properties with weight", weight))
            }
          }
        }
        
        # Each row should have at most one structure type
        # Multiply the structure matrix by the weight vector to get the weight for each row
        structure_weights <- structure_matrix %*% weight_vector
        
        # Convert to vector
        structure_weights <- as.vector(structure_weights)
        
        # NOW do the debug output AFTER calculation
        print(paste("Non-zero structure weights:", sum(structure_weights != 0)))
        print(paste("First 10 structure weights:", paste(structure_weights[1:10], collapse=", ")))
        
        # Apply structure type weights as multipliers
        structure_multipliers <- (structure_weights/100 + 1)
        
        # Debug output
        print(paste("Structure weights unique values:", paste(unique(structure_weights), collapse=", ")))
        print(paste("Number of properties with each structure type:"))
        print(table(structure_weights))
        print(paste("Structure multipliers range:", min(structure_multipliers), "-", max(structure_multipliers)))
        
        incProgress(0.3, detail = "Calculating property and business values...")
        
        # Calculate property value
        # Formula: base_value * (1 + inflation) * area^area_weight * product_of_features * structure_multiplier
        property_area <- if("property_area" %in% names(preview_data)) {
          preview_data$property_area
        } else {
          print("WARNING: property_area column not found!")
          rep(NA, n_rows)
        }
        
        # Debug property area
        print(paste("Property area range:", min(property_area, na.rm=TRUE), "-", max(property_area, na.rm=TRUE)))
        print(paste("Number of NA in property area:", sum(is.na(property_area))))
        
        # Calculate property value with NA handling
        property_value <- ifelse(is.na(property_area) | property_area <= 0,
                                 NA,
                                 inflation_adjusted_base * 
                                   (property_area ^ area_weight) * 
                                   product_weights * 
                                   structure_multipliers)
        
        # Debug property value calculation components
        if (sum(!is.na(property_value)) == 0) {
          print("All property values are NA. Checking components:")
          print(paste("Sample inflation_adjusted_base:", inflation_adjusted_base))
          print(paste("Sample property_area^area_weight:", (property_area[1:5] ^ area_weight)))
          print(paste("Sample product_weights:", product_weights[1:5]))
          print(paste("Sample structure_multipliers:", structure_multipliers[1:5]))
        }
        
        # Debug property value
        print(paste("Property value range:", 
                    ifelse(sum(!is.na(property_value)) > 0, 
                           paste(min(property_value, na.rm=TRUE), "-", max(property_value, na.rm=TRUE)),
                           "All NA")))
        print(paste("Number of NA in property value:", sum(is.na(property_value))))
        
        # Calculate business value (using business_area instead of property_area)
        business_area <- if("business_area" %in% names(preview_data)) {
          preview_data$business_area
        } else {
          print("WARNING: business_area column not found!")
          rep(NA, n_rows)
        }
        
        # Debug business area
        if (!all(is.na(business_area))) {
          print(paste("Business area range:", min(business_area, na.rm=TRUE), "-", max(business_area, na.rm=TRUE)))
          print(paste("Number of NA in business area:", sum(is.na(business_area))))
        }
        
        # Business value is only calculated where business_area exists and is not NA
        business_value <- ifelse(!is.na(business_area) & business_area > 0,
                                 inflation_adjusted_base * 
                                   (business_area ^ area_weight) * 
                                   product_weights * 
                                   structure_multipliers,
                                 NA)
        
        incProgress(0.2, detail = "Creating preview table...")
        
        # Create preview dataframe
        values$preview_data <- data.frame(
          id_property = preview_data$id_property,
          property_area = round(property_area, 2),
          inflation_adjusted_base_value = round(inflation_adjusted_base, 2),
          product_of_all_feature_weights = round(product_weights, 4),
          structure_type_multiplier = round(structure_multipliers, 4),  # Changed this line
          business_area = if("business_area" %in% names(preview_data)) 
            round(preview_data$business_area, 2) else NA,
          property_value = round(property_value, 2),
          business_value = round(business_value, 2)
        )
        
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
        DT::formatRound(columns = c('property_area', 'inflation_adjusted_base_value', 
                                    'property_value', 'business_value'), 
                        digits = 2) %>%
        DT::formatRound(columns = c('product_of_all_feature_weights', 'structure_type_multiplier'),  # Updated this line
                        digits = 4) %>%
        DT::formatCurrency(columns = c('property_value', 'business_value'), 
                           currency = "", 
                           interval = 3, 
                           mark = ",")
    })
    
    # Configuration summary
    output$config_summary <- renderPrint({
      cat("=== Configuration Summary ===\n\n")
      
      for (scenario in c("Existing", "Scenario A", "Scenario B")) {
        scenario_key <- tolower(gsub(" ", "_", scenario))
        
        cat(scenario, ":\n")
        cat("  Base Value:", input[[paste0("base_value_", scenario_key)]], "\n")
        cat("  Inflation:", input[[paste0("inflation_", scenario_key)]], "%\n")
        cat("  Inflation-Adjusted Base:", 
            format(input[[paste0("base_value_", scenario_key)]] * 
                     (1 + input[[paste0("inflation_", scenario_key)]]/100), 
                   digits = 2, big.mark = ","), "\n")
        cat("  Area Weight:", input[[paste0("area_weight_", scenario_key)]], "\n\n")
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
        
        # Collect product of all feature weights for each row
        all_features <- c(values$feature_columns$structure_features,
                          values$feature_columns$utility_features,
                          values$feature_columns$location_features,
                          values$feature_columns$location_zones,
                          values$feature_columns$property_characteristics,
                          values$ward_columns)
        
        for (feat in all_features) {
          input_id <- paste0("weight_", feat, "_", scenario)
          if (!is.null(input[[input_id]])) {
            config$feature_weights[[feat]] <- input[[input_id]]
          }
        }
        
        # Collect structure type weights
        all_structures <- c(values$commercial_type_columns,
                            values$institutional_type_columns)
        
        for (struct in all_structures) {
          input_id <- paste0("weight_", struct, "_", scenario)
          if (!is.null(input[[input_id]])) {
            config$structure_weights[[struct]] <- input[[input_id]]
          }
        }
        
        scenarios[[scenario]] <- config
      }
      
      scenarios
    })
    
    # Return configurations for use in other modules
    return(get_all_configs)
  })
}