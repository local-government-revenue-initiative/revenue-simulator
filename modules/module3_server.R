# modules/module3_server.R

module3_server <- function(id, processed_data, property_configs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      existing_config = get_default_tax_config(),
      scenario_a_config = get_default_tax_config(),
      scenario_b_config = get_default_tax_config(),
      business_subcategories = NULL,
      preview_data = NULL
    )
    
    # Get unique business subcategories when data is available
    observe({
      req(processed_data())
      data <- processed_data()
      
      if ("business_sub_category" %in% names(data)) {
        subcategories <- unique(data$business_sub_category[!is.na(data$business_sub_category)])
        values$business_subcategories <- sort(subcategories)
      }
    })
    
    # Generate business subcategory configuration UI
    generate_business_subcategories_ui <- function(scenario_suffix) {
      renderUI({
        req(values$business_subcategories)
        
        subcategory_configs <- lapply(values$business_subcategories, function(subcategory) {
          # Clean subcategory name for IDs
          subcat_id <- gsub("[^A-Za-z0-9]", "_", subcategory)
          
          wellPanel(
            h6(strong(subcategory)),
            
            # Tax type selection
            radioButtons(ns(paste0("bus_subcat_", subcat_id, "_type_", scenario_suffix)),
                         "Tax Type:",
                         choices = list(
                           "Minimum tax and rate" = "minimum_rate",
                           "Flat tax" = "flat_tax"
                         ),
                         selected = "minimum_rate",
                         inline = TRUE),
            
            # Logic slots toggle
            checkboxInput(ns(paste0("bus_subcat_", subcat_id, "_use_slots_", scenario_suffix)),
                          "Use logic slots",
                          value = FALSE),
            
            # Configuration based on selection
            conditionalPanel(
              condition = paste0("!input['", ns(paste0("bus_subcat_", subcat_id, "_use_slots_", scenario_suffix)), "']"),
              
              # Simple configuration
              conditionalPanel(
                condition = paste0("input['", ns(paste0("bus_subcat_", subcat_id, "_type_", scenario_suffix)), "'] == 'minimum_rate'"),
                fluidRow(
                  column(6,
                         numericInput(ns(paste0("bus_subcat_", subcat_id, "_min_", scenario_suffix)),
                                      "Minimum:",
                                      value = 50000,
                                      min = 0)),
                  column(6,
                         numericInput(ns(paste0("bus_subcat_", subcat_id, "_rate_", scenario_suffix)),
                                      "Rate (%):",
                                      value = 0.10,
                                      min = 0,
                                      step = 0.01))
                )
              ),
              
              conditionalPanel(
                condition = paste0("input['", ns(paste0("bus_subcat_", subcat_id, "_type_", scenario_suffix)), "'] == 'flat_tax'"),
                numericInput(ns(paste0("bus_subcat_", subcat_id, "_flat_", scenario_suffix)),
                             "Flat Tax Amount:",
                             value = 50000,
                             min = 0)
              )
            ),
            
            # Logic slots configuration
            conditionalPanel(
              condition = paste0("input['", ns(paste0("bus_subcat_", subcat_id, "_use_slots_", scenario_suffix)), "']"),
              
              # Slot basis selection
              radioButtons(ns(paste0("bus_subcat_", subcat_id, "_slot_basis_", scenario_suffix)),
                           "Logic slots based on:",
                           choices = list(
                             "Business value" = "value",
                             "Business area" = "area"
                           ),
                           selected = "value",
                           inline = TRUE),
              
              # Slot ranges
              h6("Logic Slot Ranges:"),
              fluidRow(
                column(4, p("Slot 1:", style = "font-weight: bold;")),
                column(4, numericInput(ns(paste0("bus_subcat_", subcat_id, "_slot1_min_", scenario_suffix)), 
                                       "Min:", value = 0, min = 0)),
                column(4, numericInput(ns(paste0("bus_subcat_", subcat_id, "_slot1_max_", scenario_suffix)), 
                                       "Max:", value = 1000000, min = 0))
              ),
              fluidRow(
                column(4, p("Slot 2:", style = "font-weight: bold;")),
                column(4, numericInput(ns(paste0("bus_subcat_", subcat_id, "_slot2_min_", scenario_suffix)), 
                                       "Min:", value = 1000000, min = 0)),
                column(4, numericInput(ns(paste0("bus_subcat_", subcat_id, "_slot2_max_", scenario_suffix)), 
                                       "Max:", value = 5000000, min = 0))
              ),
              fluidRow(
                column(4, p("Slot 3:", style = "font-weight: bold;")),
                column(4, numericInput(ns(paste0("bus_subcat_", subcat_id, "_slot3_min_", scenario_suffix)), 
                                       "Min:", value = 5000000, min = 0)),
                column(4, p("Max: No limit", style = "padding-top: 25px;"))
              ),
              
              hr(),
              h6("Tax Configuration per Slot:"),
              
              # Configuration based on tax type
              conditionalPanel(
                condition = paste0("input['", ns(paste0("bus_subcat_", subcat_id, "_type_", scenario_suffix)), "'] == 'minimum_rate'"),
                # Slot 1
                p("Logic Slot 1:", style = "font-weight: bold;"),
                fluidRow(
                  column(6, numericInput(ns(paste0("bus_subcat_", subcat_id, "_slot1_min_tax_", scenario_suffix)), 
                                         "Min Tax:", value = 25000, min = 0)),
                  column(6, numericInput(ns(paste0("bus_subcat_", subcat_id, "_slot1_rate_", scenario_suffix)), 
                                         "Rate (%):", value = 0.05, min = 0, step = 0.01))
                ),
                # Slot 2
                p("Logic Slot 2:", style = "font-weight: bold;"),
                fluidRow(
                  column(6, numericInput(ns(paste0("bus_subcat_", subcat_id, "_slot2_min_tax_", scenario_suffix)), 
                                         "Min Tax:", value = 50000, min = 0)),
                  column(6, numericInput(ns(paste0("bus_subcat_", subcat_id, "_slot2_rate_", scenario_suffix)), 
                                         "Rate (%):", value = 0.10, min = 0, step = 0.01))
                ),
                # Slot 3
                p("Logic Slot 3:", style = "font-weight: bold;"),
                fluidRow(
                  column(6, numericInput(ns(paste0("bus_subcat_", subcat_id, "_slot3_min_tax_", scenario_suffix)), 
                                         "Min Tax:", value = 100000, min = 0)),
                  column(6, numericInput(ns(paste0("bus_subcat_", subcat_id, "_slot3_rate_", scenario_suffix)), 
                                         "Rate (%):", value = 0.15, min = 0, step = 0.01))
                )
              ),
              
              conditionalPanel(
                condition = paste0("input['", ns(paste0("bus_subcat_", subcat_id, "_type_", scenario_suffix)), "'] == 'flat_tax'"),
                fluidRow(
                  column(4, 
                         p("Slot 1 Flat Tax:", style = "font-weight: bold;"),
                         numericInput(ns(paste0("bus_subcat_", subcat_id, "_slot1_flat_", scenario_suffix)), 
                                      NULL, value = 25000, min = 0)),
                  column(4, 
                         p("Slot 2 Flat Tax:", style = "font-weight: bold;"),
                         numericInput(ns(paste0("bus_subcat_", subcat_id, "_slot2_flat_", scenario_suffix)), 
                                      NULL, value = 50000, min = 0)),
                  column(4, 
                         p("Slot 3 Flat Tax:", style = "font-weight: bold;"),
                         numericInput(ns(paste0("bus_subcat_", subcat_id, "_slot3_flat_", scenario_suffix)), 
                                      NULL, value = 100000, min = 0))
                )
              )
            )
          )
        })
        
        do.call(tagList, subcategory_configs)
      })
    }
    
    # Generate all business UIs
    for (scenario in c("existing", "scenario_a", "scenario_b")) {
      local({
        sc <- scenario
        output[[paste0("business_subcategories_", sc)]] <- generate_business_subcategories_ui(sc)
      })
    }

    # Copy functions implementation
    observeEvent(input$copy_existing_to_a, {
      # Copy property tax settings
      for (prop_type in c("domestic", "commercial", "institutional")) {
        # Copy use_slots checkbox
        updateCheckboxInput(session, 
                            paste0("use_slots_", prop_type, "_scenario_a"), 
                            value = input[[paste0("use_slots_", prop_type, "_existing")]])
        
        # Copy simple configuration
        updateNumericInput(session, paste0(prop_type, "_min_scenario_a"), 
                           value = input[[paste0(prop_type, "_min_existing")]])
        updateNumericInput(session, paste0(prop_type, "_rate_scenario_a"), 
                           value = input[[paste0(prop_type, "_rate_existing")]])
        
        # Copy slot configurations
        for (slot in 1:3) {
          updateNumericInput(session, paste0(prop_type, "_slot", slot, "_min_scenario_a"), 
                             value = input[[paste0(prop_type, "_slot", slot, "_min_existing")]])
          updateNumericInput(session, paste0(prop_type, "_slot", slot, "_max_scenario_a"), 
                             value = input[[paste0(prop_type, "_slot", slot, "_max_existing")]])
          updateNumericInput(session, paste0(prop_type, "_slot", slot, "_min_tax_scenario_a"), 
                             value = input[[paste0(prop_type, "_slot", slot, "_min_tax_existing")]])
          updateNumericInput(session, paste0(prop_type, "_slot", slot, "_rate_scenario_a"), 
                             value = input[[paste0(prop_type, "_slot", slot, "_rate_existing")]])
        }
      }
      
      # Copy business license settings
      if (!is.null(values$business_subcategories)) {
        for (subcategory in values$business_subcategories) {
          subcat_id <- gsub("[^A-Za-z0-9]", "_", subcategory)
          
          # Copy tax type
          updateRadioButtons(session, 
                             paste0("bus_subcat_", subcat_id, "_type_scenario_a"),
                             selected = input[[paste0("bus_subcat_", subcat_id, "_type_existing")]])
          
          # Copy use slots
          updateCheckboxInput(session,
                              paste0("bus_subcat_", subcat_id, "_use_slots_scenario_a"),
                              value = input[[paste0("bus_subcat_", subcat_id, "_use_slots_existing")]])
          
          # Copy slot basis
          updateRadioButtons(session,
                             paste0("bus_subcat_", subcat_id, "_slot_basis_scenario_a"),
                             selected = input[[paste0("bus_subcat_", subcat_id, "_slot_basis_existing")]])
          
          # Copy simple configuration
          updateNumericInput(session, paste0("bus_subcat_", subcat_id, "_min_scenario_a"),
                             value = input[[paste0("bus_subcat_", subcat_id, "_min_existing")]])
          updateNumericInput(session, paste0("bus_subcat_", subcat_id, "_rate_scenario_a"),
                             value = input[[paste0("bus_subcat_", subcat_id, "_rate_existing")]])
          updateNumericInput(session, paste0("bus_subcat_", subcat_id, "_flat_scenario_a"),
                             value = input[[paste0("bus_subcat_", subcat_id, "_flat_existing")]])
          
          # Copy slot configurations
          for (slot in 1:3) {
            updateNumericInput(session, paste0("bus_subcat_", subcat_id, "_slot", slot, "_min_scenario_a"),
                               value = input[[paste0("bus_subcat_", subcat_id, "_slot", slot, "_min_existing")]])
            updateNumericInput(session, paste0("bus_subcat_", subcat_id, "_slot", slot, "_max_scenario_a"),
                               value = input[[paste0("bus_subcat_", subcat_id, "_slot", slot, "_max_existing")]])
            updateNumericInput(session, paste0("bus_subcat_", subcat_id, "_slot", slot, "_min_tax_scenario_a"),
                               value = input[[paste0("bus_subcat_", subcat_id, "_slot", slot, "_min_tax_existing")]])
            updateNumericInput(session, paste0("bus_subcat_", subcat_id, "_slot", slot, "_rate_scenario_a"),
                               value = input[[paste0("bus_subcat_", subcat_id, "_slot", slot, "_rate_existing")]])
            updateNumericInput(session, paste0("bus_subcat_", subcat_id, "_slot", slot, "_flat_scenario_a"),
                               value = input[[paste0("bus_subcat_", subcat_id, "_slot", slot, "_flat_existing")]])
          }
        }
      }
      
      showNotification("Copied Existing Scenario to Scenario A", type = "message")
    })
    
    observeEvent(input$copy_existing_to_b, {
      # Similar logic but copying to scenario_b
      # (repeat the above code but replace "_scenario_a" with "_scenario_b")
    })
    
    observeEvent(input$copy_a_to_b, {
      # Similar logic but copying from scenario_a to scenario_b
      # (repeat the above code but copy from "_scenario_a" to "_scenario_b")
    })
    
    observeEvent(input$reset_all, {
      # Reset all scenarios to defaults
      defaults <- get_default_tax_config()
      
      for (scenario in c("existing", "scenario_a", "scenario_b")) {
        # Reset property tax
        for (prop_type in c("domestic", "commercial", "institutional")) {
          updateCheckboxInput(session, paste0("use_slots_", prop_type, "_", scenario), value = FALSE)
          updateNumericInput(session, paste0(prop_type, "_min_", scenario), 
                             value = defaults$property_tax[[prop_type]]$minimum)
          updateNumericInput(session, paste0(prop_type, "_rate_", scenario), 
                             value = defaults$property_tax[[prop_type]]$rate)
        }
        
        # Reset business licenses
        if (!is.null(values$business_subcategories)) {
          for (subcategory in values$business_subcategories) {
            subcat_id <- gsub("[^A-Za-z0-9]", "_", subcategory)
            updateRadioButtons(session, paste0("bus_subcat_", subcat_id, "_type_", scenario), 
                               selected = "minimum_rate")
            updateCheckboxInput(session, paste0("bus_subcat_", subcat_id, "_use_slots_", scenario), 
                                value = FALSE)
            updateNumericInput(session, paste0("bus_subcat_", subcat_id, "_min_", scenario), 
                               value = 50000)
            updateNumericInput(session, paste0("bus_subcat_", subcat_id, "_rate_", scenario), 
                               value = 0.10)
            updateNumericInput(session, paste0("bus_subcat_", subcat_id, "_flat_", scenario), 
                               value = 50000)
          }
        }
      }
      
      showNotification("Reset all scenarios to default values", type = "message")
    })    
        
    # Helper function to collect property tax configuration
    collect_property_tax_config <- function(scenario) {
      config <- list()
      
      for (prop_type in c("domestic", "commercial", "institutional")) {
        use_slots <- input[[paste0("use_slots_", prop_type, "_", scenario)]]
        
        if (is.null(use_slots) || !use_slots) {
          # Simple configuration
          config[[prop_type]] <- list(
            use_slots = FALSE,
            minimum = input[[paste0(prop_type, "_min_", scenario)]],
            rate = input[[paste0(prop_type, "_rate_", scenario)]]
          )
        } else {
          # Slots configuration
          config[[prop_type]] <- list(
            use_slots = TRUE,
            slots = list(
              slot1 = list(
                min = input[[paste0(prop_type, "_slot1_min_", scenario)]],
                max = input[[paste0(prop_type, "_slot1_max_", scenario)]],
                minimum = input[[paste0(prop_type, "_slot1_min_tax_", scenario)]],
                rate = input[[paste0(prop_type, "_slot1_rate_", scenario)]]
              ),
              slot2 = list(
                min = input[[paste0(prop_type, "_slot2_min_", scenario)]],
                max = input[[paste0(prop_type, "_slot2_max_", scenario)]],
                minimum = input[[paste0(prop_type, "_slot2_min_tax_", scenario)]],
                rate = input[[paste0(prop_type, "_slot2_rate_", scenario)]]
              ),
              slot3 = list(
                min = input[[paste0(prop_type, "_slot3_min_", scenario)]],
                max = Inf,
                minimum = input[[paste0(prop_type, "_slot3_min_tax_", scenario)]],
                rate = input[[paste0(prop_type, "_slot3_rate_", scenario)]]
              )
            )
          )
        }
      }
      
      return(config)
    }
    
    # Helper function to collect business license configuration
    collect_business_license_config <- function(scenario) {
      config <- list()
      
      if (!is.null(values$business_subcategories)) {
        for (subcategory in values$business_subcategories) {
          subcat_id <- gsub("[^A-Za-z0-9]", "_", subcategory)
          
          tax_type <- input[[paste0("bus_subcat_", subcat_id, "_type_", scenario)]]
          use_slots <- input[[paste0("bus_subcat_", subcat_id, "_use_slots_", scenario)]]
          
          if (is.null(use_slots) || !use_slots) {
            # Simple configuration
            if (tax_type == "minimum_rate") {
              config[[subcategory]] <- list(
                tax_type = "minimum_rate",
                use_slots = FALSE,
                minimum = input[[paste0("bus_subcat_", subcat_id, "_min_", scenario)]],
                rate = input[[paste0("bus_subcat_", subcat_id, "_rate_", scenario)]]
              )
            } else {
              config[[subcategory]] <- list(
                tax_type = "flat_tax",
                use_slots = FALSE,
                flat_amount = input[[paste0("bus_subcat_", subcat_id, "_flat_", scenario)]]
              )
            }
          } else {
            # Slots configuration
            slot_basis <- input[[paste0("bus_subcat_", subcat_id, "_slot_basis_", scenario)]]
            
            if (tax_type == "minimum_rate") {
              config[[subcategory]] <- list(
                tax_type = "minimum_rate",
                use_slots = TRUE,
                slot_basis = slot_basis,
                slots = list(
                  slot1 = list(
                    min = input[[paste0("bus_subcat_", subcat_id, "_slot1_min_", scenario)]],
                    max = input[[paste0("bus_subcat_", subcat_id, "_slot1_max_", scenario)]],
                    minimum = input[[paste0("bus_subcat_", subcat_id, "_slot1_min_tax_", scenario)]],
                    rate = input[[paste0("bus_subcat_", subcat_id, "_slot1_rate_", scenario)]]
                  ),
                  slot2 = list(
                    min = input[[paste0("bus_subcat_", subcat_id, "_slot2_min_", scenario)]],
                    max = input[[paste0("bus_subcat_", subcat_id, "_slot2_max_", scenario)]],
                    minimum = input[[paste0("bus_subcat_", subcat_id, "_slot2_min_tax_", scenario)]],
                    rate = input[[paste0("bus_subcat_", subcat_id, "_slot2_rate_", scenario)]]
                  ),
                  slot3 = list(
                    min = input[[paste0("bus_subcat_", subcat_id, "_slot3_min_", scenario)]],
                    max = Inf,
                    minimum = input[[paste0("bus_subcat_", subcat_id, "_slot3_min_tax_", scenario)]],
                    rate = input[[paste0("bus_subcat_", subcat_id, "_slot3_rate_", scenario)]]
                  )
                )
              )
            } else {
              config[[subcategory]] <- list(
                tax_type = "flat_tax",
                use_slots = TRUE,
                slot_basis = slot_basis,
                slots = list(
                  slot1 = list(
                    min = input[[paste0("bus_subcat_", subcat_id, "_slot1_min_", scenario)]],
                    max = input[[paste0("bus_subcat_", subcat_id, "_slot1_max_", scenario)]],
                    flat_amount = input[[paste0("bus_subcat_", subcat_id, "_slot1_flat_", scenario)]]
                  ),
                  slot2 = list(
                    min = input[[paste0("bus_subcat_", subcat_id, "_slot2_min_", scenario)]],
                    max = input[[paste0("bus_subcat_", subcat_id, "_slot2_max_", scenario)]],
                    flat_amount = input[[paste0("bus_subcat_", subcat_id, "_slot2_flat_", scenario)]]
                  ),
                  slot3 = list(
                    min = input[[paste0("bus_subcat_", subcat_id, "_slot3_min_", scenario)]],
                    max = Inf,
                    flat_amount = input[[paste0("bus_subcat_", subcat_id, "_slot3_flat_", scenario)]]
                  )
                )
              )
            }
          }
        }
      }
      
      return(config)
    }
    
    # Preview calculation
    observeEvent(input$calculate_preview, {
      req(processed_data())  # Only require processed_data
      
      withProgress(message = 'Calculating tax preview...', value = 0, {
        data <- processed_data()
        scenario <- input$preview_scenario
        
        # NOTE: Property values will come from Module 2 when implemented
        # For now, using placeholder values
        
        # Get the property values from Module 2
        scenario_config <- property_configs()[[scenario]]
        
        # Sample first 100 rows
        n_rows <- min(100, nrow(data))
        preview_data <- data[1:n_rows, ]
        
        # Calculate property values using Module 2 configurations
        incProgress(0.3, detail = "Calculating property values...")        
        
        # Get configuration from Module 2
        if (!is.null(property_configs)) {
          module2_config <- property_configs()[[scenario]]
          
          # Calculate inflation-adjusted base value
          inflation_adjusted_base <- module2_config$base_value * (1 + module2_config$inflation)
          area_weight <- module2_config$area_weight
          
          # Get property areas
          property_areas <- if("property_area" %in% names(preview_data)) {
            preview_data$property_area
          } else {
            rep(NA, n_rows)
          }
          
          # Calculate feature weights product for each property
          all_features <- names(module2_config$feature_weights)
          product_weights <- rep(1, n_rows)
          
          for (feat in all_features) {
            if (feat %in% names(preview_data)) {
              weight <- module2_config$feature_weights[[feat]]
              if (!is.null(weight) && !is.na(weight)) {
                feature_multiplier <- ifelse(preview_data[[feat]] == 1, 
                                             (weight/100 + 1), 
                                             1)
                product_weights <- product_weights * feature_multiplier
              }
            }
          }
          
          # Calculate structure type weights (MATCHING MODULE 2 LOGIC)
          all_structures <- names(module2_config$structure_weights)
          
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
              weight <- module2_config$structure_weights[[struct]]
              if (is.null(weight)) weight <- 0
              
              weight_vector[j] <- weight
            }
          }
          
          # Each row should have at most one structure type
          # Multiply the structure matrix by the weight vector to get the weight for each row
          structure_weights <- structure_matrix %*% weight_vector
          
          # Convert to vector
          structure_weights <- as.vector(structure_weights)
          
          # Apply structure type weights as multipliers (MATCHING MODULE 2)
          structure_multipliers <- (structure_weights/100 + 1)
          
          # Calculate property values using the same formula as Module 2
          property_values <- ifelse(is.na(property_areas) | property_areas <= 0,
                                    NA,
                                    inflation_adjusted_base * 
                                      (property_areas ^ area_weight) * 
                                      product_weights * 
                                      structure_multipliers)
          
          # Calculate business values similarly
          business_areas <- if("business_area" %in% names(preview_data)) {
            preview_data$business_area
          } else {
            rep(NA, n_rows)
          }
          
          business_values <- ifelse(!is.na(business_areas) & business_areas > 0,
                                    inflation_adjusted_base * 
                                      (business_areas ^ area_weight) * 
                                      product_weights * 
                                      structure_multipliers,
                                    NA)
          
        } else {
          # Fallback if no config available
          property_values <- rep(NA, n_rows)
          business_values <- rep(NA, n_rows)
        }
        
        incProgress(0.3, detail = "Calculating taxes...")
        
        # Get property types
        property_types <- if("property_type_Domestic" %in% names(preview_data)) {
          ifelse(preview_data$property_type_Domestic == 1, "domestic",
                 ifelse(preview_data$property_type_Commercial == 1, "commercial",
                        ifelse(preview_data$property_type_Institutional == 1, "institutional", "domestic")))
        } else {
          rep("domestic", n_rows)
        }
        
        # Get tax configuration
        tax_config <- collect_property_tax_config(scenario)
        
        # Calculate property taxes
        property_taxes <- numeric(n_rows)
        tax_rates <- numeric(n_rows)
        tax_slots <- numeric(n_rows)
        
        for (i in 1:n_rows) {
          prop_type <- property_types[i]
          prop_value <- property_values[i]
          type_config <- tax_config[[prop_type]]
          
          if (!type_config$use_slots) {
            # Simple calculation
            property_taxes[i] <- max(prop_value * (type_config$rate / 100), type_config$minimum)
            tax_rates[i] <- type_config$rate
            tax_slots[i] <- NA
          } else {
            # Find which slot applies
            slot_num <- 3  # Default to highest slot
            for (s in 1:3) {
              slot <- type_config$slots[[paste0("slot", s)]]
              if (prop_value >= slot$min && prop_value < slot$max) {
                slot_num <- s
                break
              }
            }
            
            slot_config <- type_config$slots[[paste0("slot", slot_num)]]
            property_taxes[i] <- max(prop_value * (slot_config$rate / 100), slot_config$minimum)
            tax_rates[i] <- slot_config$rate
            tax_slots[i] <- slot_num
          }
        }
        
        # Calculate business licenses
        business_config <- collect_business_license_config(scenario)
        business_values <- property_values  # Placeholder - should come from Module 2
        business_areas <- if("business_area" %in% names(preview_data)) {
          preview_data$business_area
        } else {
          rep(NA, n_rows)
        }
        business_subcategories <- if("business_sub_category" %in% names(preview_data)) {
          preview_data$business_sub_category
        } else {
          rep(NA, n_rows)
        }
        
        business_licenses <- rep(0, n_rows)
        
        for (i in 1:n_rows) {
          if (!is.na(business_subcategories[i]) && business_subcategories[i] %in% names(business_config)) {
            subcat_config <- business_config[[business_subcategories[i]]]
            
            if (!subcat_config$use_slots) {
              # Simple calculation
              if (subcat_config$tax_type == "minimum_rate") {
                business_licenses[i] <- max(business_values[i] * (subcat_config$rate / 100), 
                                            subcat_config$minimum)
              } else {
                business_licenses[i] <- subcat_config$flat_amount
              }
            } else {
              # Slots calculation
              value_to_check <- if(subcat_config$slot_basis == "value") business_values[i] else business_areas[i]
              
              if (!is.na(value_to_check)) {
                slot_num <- 3
                for (s in 1:3) {
                  slot <- subcat_config$slots[[paste0("slot", s)]]
                  if (value_to_check >= slot$min && value_to_check < slot$max) {
                    slot_num <- s
                    break
                  }
                }
                
                slot_config <- subcat_config$slots[[paste0("slot", slot_num)]]
                if (subcat_config$tax_type == "minimum_rate") {
                  business_licenses[i] <- max(business_values[i] * (slot_config$rate / 100), 
                                              slot_config$minimum)
                } else {
                  business_licenses[i] <- slot_config$flat_amount
                }
              }
            }
          }
        }
        
        incProgress(0.4, detail = "Creating preview table...")
        
        # Create preview dataframe
        values$preview_data <- data.frame(
          id_property = preview_data$id_property,
          property_type = property_types,
          property_value = round(property_values, 2),
          property_tax = round(property_taxes, 2),
          tax_rate_used = tax_rates,
          tax_slot = tax_slots,
          business_subcategory = business_subcategories,
          business_value = round(business_values, 2),
          business_area = round(business_areas, 2),
          business_license = round(business_licenses, 2),
          total_tax = round(property_taxes + business_licenses, 2)
        )
      })
    })
    
    # Display preview table
    output$tax_preview_table <- DT::renderDataTable({
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
        DT::formatCurrency(columns = c('property_value', 'property_tax', 
                                       'business_value', 'business_license', 
                                       'total_tax'), 
                           currency = "", 
                           interval = 3, 
                           mark = ",")
    })
    
    # Return tax configurations for use in other modules
    return(reactive({
      list(
        existing = list(
          property_tax = collect_property_tax_config("existing"),
          business_license = collect_business_license_config("existing")
        ),
        scenario_a = list(
          property_tax = collect_property_tax_config("scenario_a"),
          business_license = collect_business_license_config("scenario_a")
        ),
        scenario_b = list(
          property_tax = collect_property_tax_config("scenario_b"),
          business_license = collect_business_license_config("scenario_b")
        )
      )
    }))
  })
}