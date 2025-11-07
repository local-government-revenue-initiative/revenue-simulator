# modules/module3_server.R

source("R/module3_config_functions.R")

module3_server <- function(
  id,
  processed_data,
  property_configs,
  calculated_property_values
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    values <- reactiveValues(
      existing_config = get_default_tax_config(),
      scenario_a_config = get_default_tax_config(),
      scenario_b_config = get_default_tax_config(),
      business_subcategories = NULL,
      preview_data = NULL,
      property_preview_data = NULL, # Add this
      business_preview_data = NULL # Add this
    )

    # Helper function to get pre-calculated property and business values
    # TODO: this should perhaps be in module2_functions.R
    get_precalculated_values <- function(scenario, preview_data, n_rows) {
      if (!is.null(calculated_property_values)) {
        tryCatch(
          {
            scenario_values <- calculated_property_values()[[scenario]]

            if (!is.null(scenario_values) && nrow(scenario_values) > 0) {
              preview_ids <- preview_data$id_property
              matched_indices <- match(preview_ids, scenario_values$id_property)

              property_values <- scenario_values$property_value[matched_indices]
              business_values <- scenario_values$business_value[matched_indices]

              property_values[is.na(property_values)] <- 0
              business_values[is.na(business_values)] <- 0

              cat("\n=== USING PRE-CALCULATED VALUES ===\n")
              cat("Scenario:", scenario, "\n")
              cat("Sample property values:", head(property_values, 3), "\n")
              cat("====================================\n\n")

              return(list(
                property_values = property_values,
                business_values = business_values,
                success = TRUE
              ))
            } else {
              return(list(
                property_values = rep(100000, n_rows),
                business_values = rep(50000, n_rows),
                success = FALSE
              ))
            }
          },
          error = function(e) {
            return(list(
              property_values = rep(100000, n_rows),
              business_values = rep(50000, n_rows),
              success = FALSE
            ))
          }
        )
      } else {
        return(list(
          property_values = rep(100000, n_rows),
          business_values = rep(50000, n_rows),
          success = FALSE
        ))
      }
    }

    # Download handler for Existing scenario
    output$download_tax_config_existing <- downloadHandler(
      filename = function() {
        paste0(
          "module3_existing_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".json"
        )
      },
      content = function(file) {
        tryCatch(
          {
            # Collect current configuration
            config <- collect_module3_config(
              input = input,
              scenario_suffix = "existing",
              business_subcategories = values$business_subcategories
            )

            # Convert to JSON and write to file
            config_json <- jsonlite::toJSON(
              config,
              auto_unbox = TRUE,
              pretty = TRUE
            )
            writeLines(config_json, file)

            showNotification(
              "Tax configuration downloaded successfully!",
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
    output$download_tax_config_scenario_a <- downloadHandler(
      filename = function() {
        paste0(
          "module3_scenario_a_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".json"
        )
      },
      content = function(file) {
        tryCatch(
          {
            # Collect current configuration
            config <- collect_module3_config(
              input = input,
              scenario_suffix = "scenario_a",
              business_subcategories = values$business_subcategories
            )

            # Convert to JSON and write to file
            config_json <- jsonlite::toJSON(
              config,
              auto_unbox = TRUE,
              pretty = TRUE
            )
            writeLines(config_json, file)

            showNotification(
              "Tax configuration downloaded successfully!",
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
    output$download_tax_config_scenario_b <- downloadHandler(
      filename = function() {
        paste0(
          "module3_scenario_b_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".json"
        )
      },
      content = function(file) {
        tryCatch(
          {
            # Collect current configuration
            config <- collect_module3_config(
              input = input,
              scenario_suffix = "scenario_b",
              business_subcategories = values$business_subcategories
            )

            # Convert to JSON and write to file
            config_json <- jsonlite::toJSON(
              config,
              auto_unbox = TRUE,
              pretty = TRUE
            )
            writeLines(config_json, file)

            showNotification(
              "Tax configuration downloaded successfully!",
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
    observeEvent(input$upload_tax_config_existing, {
      req(input$upload_tax_config_existing)

      tryCatch(
        {
          # Load configuration from uploaded file
          config <- load_module3_config(
            input$upload_tax_config_existing$datapath
          )

          # Apply configuration to UI inputs
          apply_module3_config(
            session = session,
            config = config,
            scenario_suffix = "existing",
            business_subcategories = values$business_subcategories
          )

          showNotification(
            paste0(
              "Tax configuration loaded successfully! (Saved: ",
              format(as.POSIXct(config$timestamp), "%Y-%m-%d %H:%M:%S"),
              ")"
            ),
            type = "message",
            duration = 5
          )
        },
        error = function(e) {
          showNotification(
            paste("Error loading configuration:", e$message),
            type = "error",
            duration = 10
          )
        }
      )
    })

    # Upload handler for Scenario A
    observeEvent(input$upload_tax_config_scenario_a, {
      req(input$upload_tax_config_scenario_a)

      tryCatch(
        {
          # Load configuration from uploaded file
          config <- load_module3_config(
            input$upload_tax_config_scenario_a$datapath
          )

          # Apply configuration to UI inputs
          apply_module3_config(
            session = session,
            config = config,
            scenario_suffix = "scenario_a",
            business_subcategories = values$business_subcategories
          )

          showNotification(
            paste0(
              "Tax configuration loaded successfully! (Saved: ",
              format(as.POSIXct(config$timestamp), "%Y-%m-%d %H:%M:%S"),
              ")"
            ),
            type = "message",
            duration = 5
          )
        },
        error = function(e) {
          showNotification(
            paste("Error loading configuration:", e$message),
            type = "error",
            duration = 10
          )
        }
      )
    })

    # Upload handler for Scenario B
    observeEvent(input$upload_tax_config_scenario_b, {
      req(input$upload_tax_config_scenario_b)

      tryCatch(
        {
          # Load configuration from uploaded file
          config <- load_module3_config(
            input$upload_tax_config_scenario_b$datapath
          )

          # Apply configuration to UI inputs
          apply_module3_config(
            session = session,
            config = config,
            scenario_suffix = "scenario_b",
            business_subcategories = values$business_subcategories
          )

          showNotification(
            paste0(
              "Tax configuration loaded successfully! (Saved: ",
              format(as.POSIXct(config$timestamp), "%Y-%m-%d %H:%M:%S"),
              ")"
            ),
            type = "message",
            duration = 5
          )
        },
        error = function(e) {
          showNotification(
            paste("Error loading configuration:", e$message),
            type = "error",
            duration = 10
          )
        }
      )
    })

    observe({
      tryCatch(
        {
          req(processed_data())
          data <- processed_data()

          # Get business subcategories directly from the data
          values$business_subcategories <- get_business_subcategories_from_data(
            data
          )
          values$business_categories <- get_business_categories_from_data(data)

          cat(
            "Found",
            length(values$business_subcategories),
            "business subcategories in data\n"
          )
          cat(
            "Found",
            length(values$business_categories),
            "business categories in data\n"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error loading business data:", e$message),
            type = "error"
          )
          values$business_subcategories <- c()
          values$business_categories <- c()
        }
      )
    })

    generate_business_subcategories_ui <- function(scenario_suffix) {
      renderUI({
        tryCatch(
          {
            # Use both categories and subcategories from actual data
            categories <- values$business_categories
            subcategories <- values$business_subcategories

            if (is.null(subcategories) || length(subcategories) == 0) {
              return(div(
                p(
                  "No business subcategories found in the imported data.",
                  style = "color: #666; font-style: italic;"
                ),
                p(
                  "Make sure your business data includes a 'business_sub_category' column with values."
                )
              ))
            }

            # Group subcategories by category
            data <- processed_data()
            category_groups <- data |>
              filter(
                !is.na(business_sub_category),
                !is.na(business_category)
              ) |>
              select(business_category, business_sub_category) |>
              distinct() |>
              group_by(business_category) |>
              summarise(
                subcategories = list(unique(business_sub_category)),
                .groups = "drop"
              )

            # Create collapsible sections for each category
            category_sections <- map(1:nrow(category_groups), function(i) {
              category <- category_groups$business_category[i]
              subcats <- category_groups$subcategories[[i]]

              # Create UI for subcategories in this category
              subcat_configs <- map(subcats, function(subcategory) {
                create_business_subcategory_ui(ns, subcategory, scenario_suffix)
              })

              # Create collapsible section
              div(
                style = "margin-bottom: 15px; border: 1px solid #ddd; border-radius: 4px;",
                # Category header (clickable)
                div(
                  id = paste0(
                    "category_header_",
                    gsub("[^A-Za-z0-9_]", "_", category),
                    "_",
                    scenario_suffix
                  ),
                  class = "category-header",
                  style = "background-color: #f8f9fa; padding: 10px; cursor: pointer; border-bottom: 1px solid #ddd;",
                  onclick = paste0(
                    "toggleCategory('",
                    gsub("[^A-Za-z0-9_]", "_", category),
                    "_",
                    scenario_suffix,
                    "')"
                  ),
                  h5(category, style = "margin: 0; color: #2c3e50;"),
                  span(
                    paste0(" (", length(subcats), " subcategories)"),
                    style = "color: #6c757d; font-size: 0.9em;"
                  ),
                  icon("chevron-down", style = "float: right; margin-top: 2px;")
                ),
                # Category content (collapsible)
                div(
                  id = paste0(
                    "category_content_",
                    gsub("[^A-Za-z0-9_]", "_", category),
                    "_",
                    scenario_suffix
                  ),
                  style = "padding: 10px; display: none;", # Initially collapsed
                  do.call(tagList, subcat_configs)
                )
              )
            })

            tagList(
              # Add JavaScript for collapsible functionality
              tags$script(HTML(
                "
          function toggleCategory(categoryId) {
            var content = document.getElementById('category_content_' + categoryId);
            var header = document.getElementById('category_header_' + categoryId);
            var icon = header.querySelector('i');
            
            if (content.style.display === 'none') {
              content.style.display = 'block';
              icon.className = icon.className.replace('fa-chevron-down', 'fa-chevron-up');
            } else {
              content.style.display = 'none';
              icon.className = icon.className.replace('fa-chevron-up', 'fa-chevron-down');
            }
          }
        "
              )),

              p(
                paste(
                  "Configuring",
                  length(subcategories),
                  "business subcategories organized into",
                  nrow(category_groups),
                  "categories:"
                ),
                style = "font-weight: bold; margin-bottom: 15px;"
              ),

              # Add "Expand All" / "Collapse All" buttons
              div(
                style = "margin-bottom: 15px;",
                actionButton(
                  paste0("expand_all_", scenario_suffix),
                  "Expand All",
                  class = "btn-sm btn-outline-primary",
                  onclick = paste0(
                    "toggleAllCategories('",
                    scenario_suffix,
                    "', true)"
                  )
                ),
                actionButton(
                  paste0("collapse_all_", scenario_suffix),
                  "Collapse All",
                  class = "btn-sm btn-outline-secondary",
                  onclick = paste0(
                    "toggleAllCategories('",
                    scenario_suffix,
                    "', false)"
                  )
                )
              ),

              do.call(tagList, category_sections)
            )
          },
          error = function(e) {
            showNotification(
              paste("Error generating business UI:", e$message),
              type = "error"
            )
            return(p("Error loading business categories"))
          }
        )
      })
    }

    # Generate business subcategory UIs for each scenario
    output$business_subcategories_existing <- generate_business_subcategories_ui(
      "existing"
    )
    output$business_subcategories_scenario_a <- generate_business_subcategories_ui(
      "scenario_a"
    )
    output$business_subcategories_scenario_b <- generate_business_subcategories_ui(
      "scenario_b"
    )

    # CORRECTED: Copy functions with proper business license naming
    # Copy from existing to scenario A
    observeEvent(input$copy_existing_to_a, {
      tryCatch(
        {
          # Copy property tax settings
          for (prop_type in c("domestic", "commercial", "institutional")) {
            # Check if use_slots checkbox exists
            use_slots_id <- paste0("use_slots_", prop_type, "_existing")
            if (!is.null(input[[use_slots_id]])) {
              updateCheckboxInput(
                session,
                paste0("use_slots_", prop_type, "_scenario_a"),
                value = input[[use_slots_id]]
              )
            }

            # Copy basic property tax inputs
            min_id <- paste0(prop_type, "_min_existing")
            rate_id <- paste0(prop_type, "_rate_existing")

            if (!is.null(input[[min_id]])) {
              updateNumericInput(
                session,
                paste0(prop_type, "_min_scenario_a"),
                value = input[[min_id]]
              )
            }

            if (!is.null(input[[rate_id]])) {
              updateNumericInput(
                session,
                paste0(prop_type, "_rate_scenario_a"),
                value = input[[rate_id]]
              )
            }

            # Copy slots settings if they exist
            if (!is.null(input[[use_slots_id]]) && input[[use_slots_id]]) {
              for (slot in 1:3) {
                slot_inputs <- c("min", "max", "min_tax", "rate")
                for (slot_input in slot_inputs) {
                  if (slot == 3 && slot_input == "max") {
                    next
                  } # Skip max for slot 3

                  input_id <- paste0(
                    prop_type,
                    "_slot",
                    slot,
                    "_",
                    slot_input,
                    "_existing"
                  )
                  target_id <- paste0(
                    prop_type,
                    "_slot",
                    slot,
                    "_",
                    slot_input,
                    "_scenario_a"
                  )

                  if (!is.null(input[[input_id]])) {
                    updateNumericInput(
                      session,
                      target_id,
                      value = input[[input_id]]
                    )
                  }
                }
              }
            }
          }

          # CORRECTED: Copy business license settings using new structure
          subcategories <- values$business_subcategories
          # Copy business license settings - simplified approach
          if (!is.null(values$business_subcategories)) {
            for (subcategory in values$business_subcategories) {
              subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)

              # Copy method selection
              method_source <- paste0(
                "bus_subcat_",
                subcategory_safe,
                "_method_existing"
              )
              method_target <- paste0(
                "bus_subcat_",
                subcategory_safe,
                "_method_scenario_a"
              )

              if (!is.null(input[[method_source]])) {
                updateSelectInput(
                  session,
                  method_target,
                  selected = input[[method_source]]
                )
              }

              # Copy all input types (they will be hidden/shown based on method selection)
              input_suffixes <- c(
                "_min_",
                "_rate_",
                "_value_band1_max_",
                "_value_band1_tax_",
                "_value_band2_max_",
                "_value_band2_tax_",
                "_value_band3_tax_",
                "_area_band1_max_",
                "_area_band1_tax_",
                "_area_band2_max_",
                "_area_band2_tax_",
                "_area_band3_tax_"
              )

              for (suffix in input_suffixes) {
                source_id <- paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  suffix,
                  "existing"
                )
                target_id <- paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  suffix,
                  "scenario_a"
                )

                if (!is.null(input[[source_id]])) {
                  updateNumericInput(
                    session,
                    target_id,
                    value = input[[source_id]]
                  )
                }
              }
            }
          }

          showNotification(
            "Copied Existing Scenario to Scenario A",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error copying to Scenario A:", e$message),
            type = "error"
          )
        }
      )
    })

    # Copy from existing to scenario B (similar corrections)
    observeEvent(input$copy_existing_to_b, {
      tryCatch(
        {
          # Copy property tax settings
          for (prop_type in c("domestic", "commercial", "institutional")) {
            # Check if use_slots checkbox exists
            use_slots_id <- paste0("use_slots_", prop_type, "_existing")
            if (!is.null(input[[use_slots_id]])) {
              updateCheckboxInput(
                session,
                paste0("use_slots_", prop_type, "_scenario_b"),
                value = input[[use_slots_id]]
              )
            }

            # Copy basic property tax inputs
            min_id <- paste0(prop_type, "_min_existing")
            rate_id <- paste0(prop_type, "_rate_existing")

            if (!is.null(input[[min_id]])) {
              updateNumericInput(
                session,
                paste0(prop_type, "_min_scenario_b"),
                value = input[[min_id]]
              )
            }

            if (!is.null(input[[rate_id]])) {
              updateNumericInput(
                session,
                paste0(prop_type, "_rate_scenario_b"),
                value = input[[rate_id]]
              )
            }

            # Copy slots settings if they exist
            if (!is.null(input[[use_slots_id]]) && input[[use_slots_id]]) {
              for (slot in 1:3) {
                slot_inputs <- c("min", "max", "min_tax", "rate")
                for (slot_input in slot_inputs) {
                  if (slot == 3 && slot_input == "max") {
                    next
                  } # Skip max for slot 3

                  input_id <- paste0(
                    prop_type,
                    "_slot",
                    slot,
                    "_",
                    slot_input,
                    "_existing"
                  )
                  target_id <- paste0(
                    prop_type,
                    "_slot",
                    slot,
                    "_",
                    slot_input,
                    "_scenario_b"
                  )

                  if (!is.null(input[[input_id]])) {
                    updateNumericInput(
                      session,
                      target_id,
                      value = input[[input_id]]
                    )
                  }
                }
              }
            }
          }

          # Copy business license settings using new structure
          subcategories <- values$business_subcategories
          # Copy business license settings using new structure
          if (!is.null(values$business_subcategories)) {
            # Copy all the new business license method inputs
            for (subcategory in values$business_subcategories) {
              subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)

              # Copy method selection
              method_source <- paste0(
                "bus_subcat_",
                subcategory_safe,
                "_method_existing"
              )
              method_target <- paste0(
                "bus_subcat_",
                subcategory_safe,
                "_method_scenario_b"
              ) # Adjust for target scenario

              if (!is.null(input[[method_source]])) {
                updateSelectInput(
                  session,
                  method_target,
                  selected = input[[method_source]]
                )
              }

              # Copy all input types (they will be hidden/shown based on method selection)
              input_suffixes <- c(
                "_min_",
                "_rate_",
                "_value_band1_max_",
                "_value_band1_tax_",
                "_value_band2_max_",
                "_value_band2_tax_",
                "_value_band3_tax_",
                "_area_band1_max_",
                "_area_band1_tax_",
                "_area_band2_max_",
                "_area_band2_tax_",
                "_area_band3_tax_"
              )

              for (suffix in input_suffixes) {
                source_id <- paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  suffix,
                  "existing"
                )
                target_id <- paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  suffix,
                  "scenario_b"
                ) # Adjust for target scenario

                if (!is.null(input[[source_id]])) {
                  updateNumericInput(
                    session,
                    target_id,
                    value = input[[source_id]]
                  )
                }
              }
            }
          }

          showNotification(
            "Copied Existing Scenario to Scenario B",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error copying to Scenario B:", e$message),
            type = "error"
          )
        }
      )
    })

    # Copy from scenario A to scenario B (similar corrections)
    observeEvent(input$copy_a_to_b, {
      tryCatch(
        {
          # Copy property tax settings
          for (prop_type in c("domestic", "commercial", "institutional")) {
            # Check if use_slots checkbox exists
            use_slots_id <- paste0("use_slots_", prop_type, "_scenario_a")
            if (!is.null(input[[use_slots_id]])) {
              updateCheckboxInput(
                session,
                paste0("use_slots_", prop_type, "_scenario_b"),
                value = input[[use_slots_id]]
              )
            }

            # Copy basic property tax inputs
            min_id <- paste0(prop_type, "_min_scenario_a")
            rate_id <- paste0(prop_type, "_rate_scenario_a")

            if (!is.null(input[[min_id]])) {
              updateNumericInput(
                session,
                paste0(prop_type, "_min_scenario_b"),
                value = input[[min_id]]
              )
            }

            if (!is.null(input[[rate_id]])) {
              updateNumericInput(
                session,
                paste0(prop_type, "_rate_scenario_b"),
                value = input[[rate_id]]
              )
            }

            # Copy slots settings if they exist
            if (!is.null(input[[use_slots_id]]) && input[[use_slots_id]]) {
              for (slot in 1:3) {
                slot_inputs <- c("min", "max", "min_tax", "rate")
                for (slot_input in slot_inputs) {
                  if (slot == 3 && slot_input == "max") {
                    next
                  } # Skip max for slot 3

                  input_id <- paste0(
                    prop_type,
                    "_slot",
                    slot,
                    "_",
                    slot_input,
                    "_scenario_a"
                  )
                  target_id <- paste0(
                    prop_type,
                    "_slot",
                    slot,
                    "_",
                    slot_input,
                    "_scenario_b"
                  )

                  if (!is.null(input[[input_id]])) {
                    updateNumericInput(
                      session,
                      target_id,
                      value = input[[input_id]]
                    )
                  }
                }
              }
            }
          }

          # Copy business license settings using new structure
          subcategories <- values$business_subcategories
          # Copy business license settings using new structure
          if (!is.null(values$business_subcategories)) {
            # Copy all the new business license method inputs
            for (subcategory in values$business_subcategories) {
              subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)

              # Copy method selection
              method_source <- paste0(
                "bus_subcat_",
                subcategory_safe,
                "_method_scenario_a"
              )
              method_target <- paste0(
                "bus_subcat_",
                subcategory_safe,
                "_method_scenario_b"
              ) # Adjust for target scenario

              if (!is.null(input[[method_source]])) {
                updateSelectInput(
                  session,
                  method_target,
                  selected = input[[method_source]]
                )
              }

              # Copy all input types (they will be hidden/shown based on method selection)
              input_suffixes <- c(
                "_min_",
                "_rate_",
                "_value_band1_max_",
                "_value_band1_tax_",
                "_value_band2_max_",
                "_value_band2_tax_",
                "_value_band3_tax_",
                "_area_band1_max_",
                "_area_band1_tax_",
                "_area_band2_max_",
                "_area_band2_tax_",
                "_area_band3_tax_"
              )

              for (suffix in input_suffixes) {
                source_id <- paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  suffix,
                  "scenario_a"
                )
                target_id <- paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  suffix,
                  "scenario_b"
                ) # Adjust for target scenario

                if (!is.null(input[[source_id]])) {
                  updateNumericInput(
                    session,
                    target_id,
                    value = input[[source_id]]
                  )
                }
              }
            }
          }

          showNotification("Copied Scenario A to Scenario B", type = "message")
        },
        error = function(e) {
          showNotification(
            paste("Error copying Scenario A to Scenario B:", e$message),
            type = "error"
          )
        }
      )
    })

    # Reset function with comprehensive error handling
    observeEvent(input$reset_all, {
      tryCatch(
        {
          # Get default configuration
          defaults <- get_default_tax_config()

          if (is.null(defaults)) {
            showNotification(
              "Error: Could not load default configuration",
              type = "error"
            )
            return()
          }

          for (scenario in c("existing", "scenario_a", "scenario_b")) {
            # Reset property tax settings with error handling
            for (prop_type in c("domestic", "commercial", "institutional")) {
              tryCatch(
                {
                  # Reset use_slots checkbox
                  updateCheckboxInput(
                    session,
                    paste0("use_slots_", prop_type, "_", scenario),
                    value = FALSE
                  )

                  # Reset basic property tax inputs
                  prop_config <- defaults$property_tax[[prop_type]]
                  if (!is.null(prop_config)) {
                    if (!is.null(prop_config$minimum)) {
                      updateNumericInput(
                        session,
                        paste0(prop_type, "_min_", scenario),
                        value = prop_config$minimum
                      )
                    }

                    if (!is.null(prop_config$rate)) {
                      updateNumericInput(
                        session,
                        paste0(prop_type, "_rate_", scenario),
                        value = prop_config$rate * 100
                      ) # Convert to percentage
                    }
                  }
                },
                error = function(e) {
                  warning(paste(
                    "Error resetting",
                    prop_type,
                    "property tax for",
                    scenario,
                    ":",
                    e$message
                  ))
                }
              )
            }

            # In module3_server.R, update the reset function business license section:
            # Reset business license settings - simplified approach
            if (!is.null(values$business_subcategories)) {
              for (subcategory in values$business_subcategories) {
                subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)

                # Get subcategory-specific defaults
                defaults <- get_subcategory_defaults(subcategory)

                # Reset to subcategory-specific defaults
                updateSelectInput(
                  session,
                  paste0("bus_subcat_", subcategory_safe, "_method_", scenario),
                  selected = "min_rate"
                )
                updateNumericInput(
                  session,
                  paste0("bus_subcat_", subcategory_safe, "_min_", scenario),
                  value = defaults$minimum
                )
                updateNumericInput(
                  session,
                  paste0("bus_subcat_", subcategory_safe, "_rate_", scenario),
                  value = defaults$rate
                )
              }
            }
          }

          showNotification(
            "Reset all scenarios to default values",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error during reset:", e$message),
            type = "error"
          )
        }
      )
    })

    # Helper function to collect property tax configuration with enhanced error handling
    collect_property_tax_config <- function(scenario) {
      config <- list()

      tryCatch(
        {
          for (prop_type in c("domestic", "commercial", "institutional")) {
            tryCatch(
              {
                use_slots <- input[[paste0(
                  "use_slots_",
                  prop_type,
                  "_",
                  scenario
                )]]

                if (is.null(use_slots) || !use_slots) {
                  # Simple configuration
                  min_val <- input[[paste0(prop_type, "_min_", scenario)]]
                  rate_val <- input[[paste0(prop_type, "_rate_", scenario)]]

                  # Validate inputs
                  if (is.null(min_val)) {
                    min_val <- 0
                  }
                  if (is.null(rate_val)) {
                    rate_val <- 0
                  }

                  config[[prop_type]] <- list(
                    use_slots = FALSE,
                    minimum = min_val,
                    rate = rate_val / 100 # Convert percentage to decimal
                  )
                } else {
                  # Slots configuration
                  slots_config <- list()

                  for (slot in 1:3) {
                    slot_config <- list()

                    # Get slot values with defaults
                    slot_min <- input[[paste0(
                      prop_type,
                      "_slot",
                      slot,
                      "_min_",
                      scenario
                    )]]
                    slot_max <- if (slot == 3) {
                      Inf
                    } else {
                      input[[paste0(
                        prop_type,
                        "_slot",
                        slot,
                        "_max_",
                        scenario
                      )]]
                    }
                    slot_min_tax <- input[[paste0(
                      prop_type,
                      "_slot",
                      slot,
                      "_min_tax_",
                      scenario
                    )]]
                    slot_rate <- input[[paste0(
                      prop_type,
                      "_slot",
                      slot,
                      "_rate_",
                      scenario
                    )]]

                    # Apply defaults for missing values
                    if (is.null(slot_min)) {
                      slot_min <- 0
                    }
                    if (is.null(slot_max) && slot != 3) {
                      slot_max <- 1000000
                    }
                    if (is.null(slot_min_tax)) {
                      slot_min_tax <- 0
                    }
                    if (is.null(slot_rate)) {
                      slot_rate <- 0
                    }

                    slot_config <- list(
                      min = slot_min,
                      max = slot_max,
                      minimum = slot_min_tax,
                      rate = slot_rate / 100 # Convert percentage to decimal
                    )

                    slots_config[[paste0("slot", slot)]] <- slot_config
                  }

                  config[[prop_type]] <- list(
                    use_slots = TRUE,
                    slots = slots_config
                  )
                }
              },
              error = function(e) {
                warning(paste(
                  "Error collecting config for",
                  prop_type,
                  "in",
                  scenario,
                  ":",
                  e$message
                ))
                # Provide default config for this property type
                config[[prop_type]] <<- list(
                  use_slots = FALSE,
                  minimum = 0,
                  rate = 0
                )
              }
            )
          }

          return(config)
        },
        error = function(e) {
          showNotification(
            paste(
              "Error collecting property tax config for",
              scenario,
              ":",
              e$message
            ),
            type = "error"
          )
          # Return default config
          return(list(
            domestic = list(use_slots = FALSE, minimum = 0, rate = 0),
            commercial = list(use_slots = FALSE, minimum = 0, rate = 0),
            institutional = list(use_slots = FALSE, minimum = 0, rate = 0)
          ))
        }
      )
    }

    collect_business_license_config <- function(scenario) {
      config <- list()

      # Use actual subcategories from data
      subcategories <- values$business_subcategories

      if (is.null(subcategories) || length(subcategories) == 0) {
        return(config)
      }

      for (subcategory in subcategories) {
        subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)

        # Get method selection
        method <- input[[paste0(
          "bus_subcat_",
          subcategory_safe,
          "_method_",
          scenario
        )]]
        if (is.null(method)) {
          method <- "minimum_rate"
        }

        if (method == "minimum_rate") {
          # Method 1: Minimum + rate
          minimum <- input[[paste0(
            "bus_subcat_",
            subcategory_safe,
            "_min_",
            scenario
          )]]
          rate <- input[[paste0(
            "bus_subcat_",
            subcategory_safe,
            "_rate_",
            scenario
          )]]

          config[[subcategory]] <- list(
            calculation_method = "minimum_rate",
            minimum = if (is.null(minimum)) 350 else minimum,
            rate = if (is.null(rate)) 0.035 else rate / 100
          )
        } else if (method == "flat") {
          # Method 2: Flat amount (fixed)
          flat_amount <- input[[paste0(
            "bus_subcat_",
            subcategory_safe,
            "_flat_",
            scenario
          )]]

          config[[subcategory]] <- list(
            calculation_method = "flat",
            flat_amount = if (is.null(flat_amount)) 1000 else flat_amount
          )
        } else if (method == "flat_value_bands") {
          # Method 3: Flat amount based on business value bands
          config[[subcategory]] <- list(
            calculation_method = "flat_value_bands",
            value_bands = list(
              band1 = list(
                max = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_value_band1_max_",
                  scenario
                )]] %||%
                  100000,
                tax = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_value_band1_tax_",
                  scenario
                )]] %||%
                  500
              ),
              band2 = list(
                max = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_value_band2_max_",
                  scenario
                )]] %||%
                  500000,
                tax = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_value_band2_tax_",
                  scenario
                )]] %||%
                  1500
              ),
              band3 = list(
                max = Inf,
                tax = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_value_band3_tax_",
                  scenario
                )]] %||%
                  3000
              )
            )
          )
        } else if (method == "flat_area_bands") {
          # Method 4: Flat amount based on business area bands
          config[[subcategory]] <- list(
            calculation_method = "flat_area_bands",
            area_bands = list(
              band1 = list(
                max = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_area_band1_max_",
                  scenario
                )]] %||%
                  100,
                tax = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_area_band1_tax_",
                  scenario
                )]] %||%
                  300
              ),
              band2 = list(
                max = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_area_band2_max_",
                  scenario
                )]] %||%
                  500,
                tax = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_area_band2_tax_",
                  scenario
                )]] %||%
                  800
              ),
              band3 = list(
                max = Inf,
                tax = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_area_band3_tax_",
                  scenario
                )]] %||%
                  1200
              )
            )
          )
        }
      }

      return(config)
    }

    # CORRECTED: Business license calculation in preview
    observeEvent(input$calculate_preview, {
      req(processed_data())

      data <- processed_data()
      scenario <- input$preview_scenario

      # Calculate n_rows first
      n_rows <- min(input$preview_rows %||% 25, nrow(data))

      withProgress(
        message = paste('Calculating tax preview for', n_rows, 'properties...'),
        value = 0,
        {
          tryCatch(
            {
              preview_data <- data[1:n_rows, ]

              incProgress(0.2, detail = "Getting configurations...")

              values_result <- get_precalculated_values(
                scenario,
                preview_data,
                n_rows
              )
              property_values <- values_result$property_values
              business_values <- values_result$business_values

              incProgress(0.2, detail = "Calculating property taxes...")

              # Get property types with error handling
              property_types <- tryCatch(
                {
                  if ("property_type_Domestic" %in% names(preview_data)) {
                    ifelse(
                      preview_data$property_type_Domestic == 1,
                      "domestic",
                      ifelse(
                        preview_data$property_type_Commercial == 1,
                        "commercial",
                        ifelse(
                          preview_data$property_type_Institutional == 1,
                          "institutional",
                          "domestic"
                        )
                      )
                    )
                  } else {
                    rep("domestic", n_rows)
                  }
                },
                error = function(e) {
                  rep("domestic", n_rows) # Default to domestic
                }
              )

              # Add debugging here:
              cat(
                "Debug: property_values length:",
                length(property_values),
                "\n"
              )
              cat("Debug: property_types length:", length(property_types), "\n")
              cat(
                "Debug: first few property_values:",
                head(property_values, 3),
                "\n"
              )
              cat(
                "Debug: first few property_types:",
                head(property_types, 3),
                "\n"
              )

              # Calculate property taxes with error handling
              tax_config <- collect_property_tax_config(scenario)

              cat("Debug: tax_config structure:\n")
              str(tax_config)
              property_taxes <- numeric(n_rows)
              tax_rates <- numeric(n_rows)
              tax_slots <- numeric(n_rows)

              for (i in 1:n_rows) {
                tryCatch(
                  {
                    prop_type <- property_types[i]
                    prop_value <- property_values[i]

                    if (is.na(prop_value) || prop_value <= 0) {
                      property_taxes[i] <- 0
                      tax_rates[i] <- 0
                      tax_slots[i] <- NA
                      next
                    }

                    type_config <- tax_config[[prop_type]]

                    if (!type_config$use_slots) {
                      # Simple calculation
                      property_taxes[i] <- max(
                        prop_value * type_config$rate,
                        type_config$minimum
                      )
                      tax_rates[i] <- type_config$rate * 100 # Convert to percentage for display
                      tax_slots[i] <- NA
                    } else {
                      # Find which slot applies
                      slot_num <- 3 # Default to highest slot
                      for (s in 1:3) {
                        slot <- type_config$slots[[paste0("slot", s)]]
                        if (prop_value >= slot$min && prop_value < slot$max) {
                          slot_num <- s
                          break
                        }
                      }

                      slot_config <- type_config$slots[[paste0(
                        "slot",
                        slot_num
                      )]]
                      property_taxes[i] <- max(
                        prop_value * slot_config$rate,
                        slot_config$minimum
                      )
                      tax_rates[i] <- slot_config$rate * 100 # Convert to percentage for display
                      tax_slots[i] <- slot_num
                    }
                  },
                  error = function(e) {
                    warning(paste(
                      "Error calculating property tax for row",
                      i,
                      ":",
                      e$message
                    ))
                    property_taxes[i] <- 0
                    tax_rates[i] <- 0
                    tax_slots[i] <- NA
                  }
                )
              }

              incProgress(0.2, detail = "Calculating business licenses...")

              # Calculate business licenses with error handling
              business_config <- collect_business_license_config(scenario)
              business_subcategories <- tryCatch(
                {
                  if ("business_sub_category" %in% names(preview_data)) {
                    preview_data$business_sub_category
                  } else {
                    rep(NA, n_rows)
                  }
                },
                error = function(e) {
                  rep(NA, n_rows)
                }
              )

              business_licenses <- rep(0, n_rows)

              # Updated: Business license calculation with new methods
              for (i in 1:n_rows) {
                subcat <- business_subcategories[i]
                if (!is.na(subcat) && subcat %in% names(business_config)) {
                  subcat_config <- business_config[[subcat]]

                  if (subcat_config$calculation_method == "minimum_rate") {
                    # Method 1: Traditional minimum + rate calculation
                    business_licenses[i] <- max(
                      business_values[i] * subcat_config$rate,
                      subcat_config$minimum
                    )
                  } else if (
                    subcat_config$calculation_method == "flat_value_bands"
                  ) {
                    # Method 2: Flat amount based on business value bands
                    business_licenses[i] <- subcat_config$value_bands$band3$tax # Default to highest band

                    if (
                      business_values[i] <= subcat_config$value_bands$band1$max
                    ) {
                      business_licenses[
                        i
                      ] <- subcat_config$value_bands$band1$tax
                    } else if (
                      business_values[i] <= subcat_config$value_bands$band2$max
                    ) {
                      business_licenses[
                        i
                      ] <- subcat_config$value_bands$band2$tax
                    }
                  } else if (
                    subcat_config$calculation_method == "flat_area_bands"
                  ) {
                    # Method 3: Flat amount based on business area bands
                    area_value <- if (
                      "business_area" %in% names(preview_data)
                    ) {
                      preview_data$business_area[i]
                    } else {
                      property_areas[i] # Use property area as fallback
                    }

                    business_licenses[i] <- subcat_config$area_bands$band3$tax # Default to highest band

                    if (area_value <= subcat_config$area_bands$band1$max) {
                      business_licenses[i] <- subcat_config$area_bands$band1$tax
                    } else if (
                      area_value <= subcat_config$area_bands$band2$max
                    ) {
                      business_licenses[i] <- subcat_config$area_bands$band2$tax
                    }
                  }
                }
              }

              incProgress(0.1, detail = "Creating preview table...")

              # Create preview dataframe with error handling
              values$preview_data <- tryCatch(
                {
                  data.frame(
                    id_property = preview_data$id_property,
                    property_type = property_types,
                    property_value = round(property_values, 2),
                    property_tax = round(property_taxes, 2),
                    tax_rate_used = round(tax_rates, 2),
                    tax_slot = tax_slots,
                    business_subcategory = business_subcategories,
                    business_value = round(business_values, 2),
                    business_area = round(
                      if ("business_area" %in% names(preview_data)) {
                        preview_data$business_area
                      } else {
                        property_areas
                      },
                      2
                    ),
                    business_license = round(business_licenses, 2),
                    total_tax = round(property_taxes + business_licenses, 2),
                    stringsAsFactors = FALSE
                  )
                },
                error = function(e) {
                  showNotification(
                    paste("Error creating preview table:", e$message),
                    type = "error"
                  )
                  # Return minimal table
                  data.frame(
                    id_property = 1:n_rows,
                    property_type = rep("domestic", n_rows),
                    property_value = rep(0, n_rows),
                    property_tax = rep(0, n_rows),
                    tax_rate_used = rep(0, n_rows),
                    tax_slot = rep(NA, n_rows),
                    business_subcategory = rep(NA, n_rows),
                    business_value = rep(0, n_rows),
                    business_area = rep(0, n_rows),
                    business_license = rep(0, n_rows),
                    total_tax = rep(0, n_rows)
                  )
                }
              )

              showNotification(
                "Tax preview calculated successfully",
                type = "message"
              )
            },
            error = function(e) {
              showNotification(
                paste("Error in preview calculation:", e$message),
                type = "error"
              )
              values$preview_data <- NULL
            }
          )
        }
      )
    })

    # Property Tax Preview calculation
    observeEvent(input$calculate_property_preview, {
      req(processed_data())

      data <- processed_data()
      scenario <- input$property_preview_scenario
      n_rows <- min(input$property_preview_rows %||% 25, nrow(data))

      withProgress(
        message = paste(
          'Calculating property tax preview for',
          n_rows,
          'properties...'
        ),
        value = 0,
        {
          tryCatch(
            {
              preview_data <- data[1:n_rows, ]

              incProgress(0.2, detail = "Getting configurations...")

              values_result <- get_precalculated_values(
                scenario,
                preview_data,
                n_rows
              )
              property_values <- values_result$property_values
              business_values <- values_result$business_values

              incProgress(0.2, detail = "Calculating property taxes...")

              # Get property types with error handling
              property_types <- tryCatch(
                {
                  if ("property_type_Domestic" %in% names(preview_data)) {
                    ifelse(
                      preview_data$property_type_Domestic == 1,
                      "domestic",
                      ifelse(
                        preview_data$property_type_Commercial == 1,
                        "commercial",
                        ifelse(
                          preview_data$property_type_Institutional == 1,
                          "institutional",
                          "domestic"
                        )
                      )
                    )
                  } else {
                    rep("domestic", n_rows)
                  }
                },
                error = function(e) {
                  rep("domestic", n_rows) # Default to domestic
                }
              )

              # Calculate property taxes with deduplication - same approach as Module 4
              tax_config <- list(
                property_tax = collect_property_tax_config(scenario)
              )

              # Add this debugging before calling calculate_property_taxes_with_deduplication
              cat(
                "Debug: property_values length:",
                length(property_values),
                "\n"
              )
              cat("Debug: property_types length:", length(property_types), "\n")
              cat("Debug: tax_config structure:\n")
              str(tax_config$property_tax)
              cat(
                "Debug: first few property_values:",
                head(property_values, 3),
                "\n"
              )
              cat(
                "Debug: first few property_types:",
                head(property_types, 3),
                "\n"
              )

              # Use the deduplication function from module3_functions.R
              property_taxes <- calculate_property_taxes_with_deduplication(
                preview_data,
                property_values,
                property_types,
                tax_config$property_tax
              )

              # Initialize display arrays
              tax_rates <- numeric(n_rows)
              tax_slots <- numeric(n_rows)

              # Calculate display values for each row (rates and slots for display purposes)
              for (i in 1:n_rows) {
                prop_type <- property_types[i]
                prop_value <- property_values[i]

                if (is.na(prop_value) || prop_value <= 0) {
                  tax_rates[i] <- 0
                  tax_slots[i] <- NA
                  next
                }

                type_config <- tax_config$property_tax[[prop_type]]

                if (!type_config$use_slots) {
                  tax_rates[i] <- type_config$rate * 100
                  tax_slots[i] <- NA
                } else {
                  # Find which slot applies for display
                  slot_num <- 3
                  for (s in 1:3) {
                    slot <- type_config$slots[[paste0("slot", s)]]
                    if (prop_value >= slot$min && prop_value < slot$max) {
                      slot_num <- s
                      break
                    }
                  }

                  slot_config <- type_config$slots[[paste0("slot", slot_num)]]
                  tax_rates[i] <- slot_config$rate * 100
                  tax_slots[i] <- slot_num
                }
              }

              incProgress(0.1, detail = "Creating preview table...")

              # Create property-only preview dataframe
              preview_df <- data.frame(
                id_property = preview_data$id_property,
                property_type = property_types,
                property_value = round(property_values, 2),
                property_tax = round(property_taxes, 2),
                tax_rate_used = round(tax_rates, 2),
                tax_slot = tax_slots,
                stringsAsFactors = FALSE
              )

              # DEDUPLICATE: Keep only unique combinations of id_property + property_type
              # This removes duplicate rows that may exist in the input data from Module 1
              values$property_preview_data <- preview_df %>%
                group_by(id_property, property_type) %>%
                slice(1) %>% # Keep only the first occurrence of each combination
                ungroup()

              showNotification(
                "Property tax preview calculated successfully",
                type = "message"
              )
            },
            error = function(e) {
              showNotification(
                paste("Error calculating property tax preview:", e$message),
                type = "error"
              )
              values$property_preview_data <- NULL
            }
          )
        }
      )
    })

    # Business License Preview calculation
    observeEvent(input$calculate_business_preview, {
      req(processed_data())

      data <- processed_data()
      scenario <- input$business_preview_scenario
      n_preview <- input$business_preview_rows %||% 25 # Store desired preview count
      n_rows <- nrow(data) # Calculate for ALL rows instead of just first n

      withProgress(
        message = paste(
          'Calculating business license preview for',
          n_rows,
          'properties...'
        ),
        value = 0,
        {
          tryCatch(
            {
              preview_data <- data[1:n_rows, ]

              incProgress(0.3, detail = "Getting configurations...")

              values_result <- get_precalculated_values(
                scenario,
                preview_data,
                n_rows
              )
              property_values <- values_result$property_values
              business_values <- values_result$business_values

              incProgress(0.4, detail = "Calculating business licenses...")

              # Get business subcategories
              business_subcategories <- tryCatch(
                {
                  if ("business_sub_category" %in% names(preview_data)) {
                    preview_data$business_sub_category
                  } else {
                    rep(NA, n_rows)
                  }
                },
                error = function(e) {
                  rep(NA, n_rows)
                }
              )

              # Calculate business licenses using the existing logic
              business_config <- collect_business_license_config(scenario)
              business_licenses <- rep(0, n_rows)

              for (i in 1:n_rows) {
                subcat <- business_subcategories[i]
                # Inside the loop calculating business licenses:
                if (!is.na(subcat) && subcat %in% names(business_config)) {
                  subcat_config <- business_config[[subcat]]

                  if (subcat_config$calculation_method == "minimum_rate") {
                    # Method 1: Traditional minimum + rate calculation
                    business_licenses[i] <- max(
                      business_values[i] * subcat_config$rate,
                      subcat_config$minimum
                    )
                  } else if (subcat_config$calculation_method == "flat") {
                    # Method 2: Flat amount (THIS WAS MISSING!)
                    business_licenses[i] <- subcat_config$flat_amount
                  } else if (
                    subcat_config$calculation_method == "flat_value_bands"
                  ) {
                    # Method 3: Flat amount based on business value bands
                    business_licenses[i] <- subcat_config$value_bands$band3$tax # Default to highest band

                    if (
                      business_values[i] <= subcat_config$value_bands$band1$max
                    ) {
                      business_licenses[
                        i
                      ] <- subcat_config$value_bands$band1$tax
                    } else if (
                      business_values[i] <= subcat_config$value_bands$band2$max
                    ) {
                      business_licenses[
                        i
                      ] <- subcat_config$value_bands$band2$tax
                    }
                  } else if (
                    subcat_config$calculation_method == "flat_area_bands"
                  ) {
                    # Method 4: Flat amount based on business area bands
                    area_value <- business_areas[i]

                    business_licenses[i] <- subcat_config$area_bands$band3$tax # Default to highest band

                    if (area_value <= subcat_config$area_bands$band1$max) {
                      business_licenses[i] <- subcat_config$area_bands$band1$tax
                    } else if (
                      area_value <= subcat_config$area_bands$band2$max
                    ) {
                      business_licenses[i] <- subcat_config$area_bands$band2$tax
                    }
                  }
                }
              }

              incProgress(0.2, detail = "Creating preview table...")

              # Create business-only preview dataframe and filter out rows without businesses
              values$business_preview_data <- data.frame(
                id_property = preview_data$id_property,
                business_subcategory = business_subcategories,
                business_value = round(business_values, 2),
                business_area = round(business_areas, 2),
                business_license = round(business_licenses, 2),
                stringsAsFactors = FALSE
              ) |>
                filter(
                  !is.na(business_value) &
                    business_value > 0 &
                    !is.na(business_subcategory)
                ) |>
                head(n_preview)

              showNotification(
                "Business license preview calculated successfully",
                type = "message"
              )
            },
            error = function(e) {
              showNotification(
                paste("Error calculating business license preview:", e$message),
                type = "error"
              )
              values$business_preview_data <- NULL
            }
          )
        }
      )
    })

    # Property Tax Preview outputs
    output$property_preview_summary <- renderText({
      req(values$property_preview_data)

      data <- values$property_preview_data
      scenario_name <- switch(
        input$property_preview_scenario,
        "existing" = "Existing Scenario",
        "scenario_a" = "Alternative Scenario A",
        "scenario_b" = "Alternative Scenario B"
      )

      total_properties <- nrow(data)
      total_property_tax <- sum(data$property_tax, na.rm = TRUE)
      avg_property_value <- mean(data$property_value, na.rm = TRUE)
      avg_property_tax <- mean(data$property_tax, na.rm = TRUE)

      paste0(
        "<strong>",
        scenario_name,
        " - Property Taxes</strong><br/>",
        "<strong>Total Properties:</strong> ",
        format(total_properties, big.mark = ","),
        "<br/>",
        "<strong>Total Property Tax Revenue:</strong> ",
        format(round(total_property_tax), big.mark = ","),
        "<br/>",
        "<strong>Avg Property Value:</strong> ",
        format(round(avg_property_value), big.mark = ","),
        "<br/>",
        "<strong>Avg Property Tax:</strong> ",
        format(round(avg_property_tax), big.mark = ",")
      )
    })

    output$property_tax_preview_table <- DT::renderDataTable({
      req(values$property_preview_data)

      DT::datatable(
        values$property_preview_data,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons'
      ) %>%
        DT::formatCurrency(
          columns = c('property_value', 'property_tax'),
          currency = "",
          interval = 3,
          mark = ","
        ) %>%
        DT::formatRound(columns = 'tax_rate_used', digits = 2)
    })

    output$download_property_preview <- downloadHandler(
      filename = function() {
        paste0(
          "property_tax_preview_",
          input$property_preview_scenario,
          "_",
          Sys.Date(),
          ".csv"
        )
      },
      content = function(file) {
        req(values$property_preview_data)
        write.csv(values$property_preview_data, file, row.names = FALSE)
      }
    )

    # Business License Preview outputs
    output$business_preview_summary <- renderText({
      req(values$business_preview_data)

      data <- values$business_preview_data
      scenario_name <- switch(
        input$business_preview_scenario,
        "existing" = "Existing Scenario",
        "scenario_a" = "Alternative Scenario A",
        "scenario_b" = "Alternative Scenario B"
      )

      total_businesses <- nrow(data) # Now only businesses
      total_business_license <- sum(data$business_license, na.rm = TRUE)
      avg_business_value <- mean(data$business_value, na.rm = TRUE)
      avg_business_license <- mean(data$business_license, na.rm = TRUE)

      paste0(
        "<strong>",
        scenario_name,
        " - Business Licenses</strong><br/>",
        "<strong>Properties with Business:</strong> ",
        format(total_businesses, big.mark = ","),
        "<br/>",
        "<strong>Total Business License Revenue:</strong> ",
        format(round(total_business_license), big.mark = ","),
        "<br/>",
        "<strong>Avg Business Value:</strong> ",
        format(round(avg_business_value), big.mark = ","),
        "<br/>",
        "<strong>Avg Business License:</strong> ",
        format(round(avg_business_license), big.mark = ",")
      )
    })

    output$business_license_preview_table <- DT::renderDataTable({
      req(values$business_preview_data)

      DT::datatable(
        values$business_preview_data,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons'
      ) %>%
        DT::formatCurrency(
          columns = c('business_value', 'business_license'),
          currency = "",
          interval = 3,
          mark = ","
        ) %>%
        DT::formatRound(columns = 'business_area', digits = 2)
    })

    output$download_business_preview <- downloadHandler(
      filename = function() {
        paste0(
          "business_license_preview_",
          input$business_preview_scenario,
          "_",
          Sys.Date(),
          ".csv"
        )
      },
      content = function(file) {
        req(values$business_preview_data)
        write.csv(values$business_preview_data, file, row.names = FALSE)
      }
    )

    # Preview summary output
    output$preview_summary <- renderText({
      req(values$preview_data)

      data <- values$preview_data
      scenario_name <- switch(
        input$preview_scenario,
        "existing" = "Existing Scenario",
        "scenario_a" = "Alternative Scenario A",
        "scenario_b" = "Alternative Scenario B"
      )

      total_properties <- nrow(data)
      total_property_tax <- sum(data$property_tax, na.rm = TRUE)
      total_business_license <- sum(data$business_license, na.rm = TRUE)
      total_revenue <- total_property_tax + total_business_license
      avg_property_value <- mean(data$property_value, na.rm = TRUE)
      avg_total_tax <- mean(data$total_tax, na.rm = TRUE)

      # Count property types
      prop_type_counts <- table(data$property_type)

      # Count business subcategories (excluding NAs)
      business_counts <- sum(!is.na(data$business_subcategory))

      paste0(
        "<strong>",
        scenario_name,
        "</strong><br/>",
        "<strong>Properties:</strong> ",
        format(total_properties, big.mark = ","),
        "<br/>",
        "<strong>Property Types:</strong> ",
        paste(
          names(prop_type_counts),
          " (",
          prop_type_counts,
          ")",
          sep = "",
          collapse = ", "
        ),
        "<br/>",
        "<strong>Properties with Business:</strong> ",
        format(business_counts, big.mark = ","),
        "<br/>",
        "<strong>Total Property Tax:</strong> ",
        format(round(total_property_tax), big.mark = ","),
        "<br/>",
        "<strong>Total Business License:</strong> ",
        format(round(total_business_license), big.mark = ","),
        "<br/>",
        "<strong>Total Revenue:</strong> ",
        format(round(total_revenue), big.mark = ","),
        "<br/>",
        "<strong>Avg Property Value:</strong> ",
        format(round(avg_property_value), big.mark = ","),
        "<br/>",
        "<strong>Avg Total Tax:</strong> ",
        format(round(avg_total_tax), big.mark = ",")
      )
    })

    # Download handler for preview data
    output$download_preview <- downloadHandler(
      filename = function() {
        scenario_name <- switch(
          input$preview_scenario,
          "existing" = "existing",
          "scenario_a" = "scenario_a",
          "scenario_b" = "scenario_b"
        )
        paste0("tax_preview_", scenario_name, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(values$preview_data)
        write.csv(values$preview_data, file, row.names = FALSE)
      }
    )

    # Display preview table with enhanced error handling
    output$tax_preview_table <- DT::renderDataTable({
      tryCatch(
        {
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
            DT::formatCurrency(
              columns = c(
                'property_value',
                'property_tax',
                'business_value',
                'business_license',
                'total_tax'
              ),
              currency = "",
              interval = 3,
              mark = ","
            )
        },
        error = function(e) {
          showNotification(
            paste("Error displaying preview table:", e$message),
            type = "error"
          )
          DT::datatable(data.frame(Error = "Unable to display preview table"))
        }
      )
    })

    # Return tax configurations for use in other modules
    return(reactive({
      tryCatch(
        {
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
        },
        error = function(e) {
          showNotification(
            paste("Error collecting tax configurations:", e$message),
            type = "error"
          )
          # Return default empty configuration
          list(
            existing = list(property_tax = list(), business_license = list()),
            scenario_a = list(property_tax = list(), business_license = list()),
            scenario_b = list(property_tax = list(), business_license = list())
          )
        }
      )
    }))
  }) # Close the moduleServer function
} # Close the module3_server function
