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
    
    # Get business categories and subcategories from data and config
    observe({
      tryCatch({
        req(processed_data())
        data <- processed_data()
        
        # Get the default configuration to show all categories and subcategories
        default_config <- get_default_tax_config()
        if (is.null(default_config) || is.null(default_config$business_license) || 
            is.null(default_config$business_license$categories)) {
          values$config_categories <- NULL
          return()
        }
        
        # Get categories from the default config
        config_categories <- names(default_config$business_license$categories)
        values$config_categories <- config_categories
        
        # Also get subcategories from data for backwards compatibility
        if ("business_sub_category" %in% names(data)) {
          data_subcategories <- unique(data$business_sub_category[!is.na(data$business_sub_category)])
          values$business_subcategories <- sort(data_subcategories)
        } else {
          values$business_subcategories <- NULL
        }
      }, error = function(e) {
        showNotification(paste("Error loading business categories:", e$message), type = "error")
        values$config_categories <- NULL
        values$business_subcategories <- NULL
      })
    })
    
    # CORRECTED: Single UI generation approach
    generate_business_subcategories_ui <- function(scenario_suffix) {
      renderUI({
        tryCatch({
          subcategories <- get_business_subcategories()
          
          if (length(subcategories) == 0) {
            return(p("No business subcategories found."))
          }
          
          category_configs <- lapply(subcategories, function(subcategory) {
            create_business_subcategory_ui(ns, subcategory, scenario_suffix)
          })
          
          do.call(tagList, category_configs)
        }, error = function(e) {
          showNotification(paste("Error generating business UI:", e$message), type = "error")
          return(p("Error loading business categories"))
        })
      })
    }
    
    # Generate business subcategory UIs for each scenario
    output$business_subcategories_existing <- generate_business_subcategories_ui("existing")
    output$business_subcategories_scenario_a <- generate_business_subcategories_ui("scenario_a") 
    output$business_subcategories_scenario_b <- generate_business_subcategories_ui("scenario_b")
    
    # CORRECTED: Copy functions with proper business license naming
    # Copy from existing to scenario A
    observeEvent(input$copy_existing_to_a, {
      tryCatch({
        # Copy property tax settings
        for (prop_type in c("domestic", "commercial", "institutional")) {
          # Check if use_slots checkbox exists
          use_slots_id <- paste0("use_slots_", prop_type, "_existing")
          if (!is.null(input[[use_slots_id]])) {
            updateCheckboxInput(session, paste0("use_slots_", prop_type, "_scenario_a"), 
                               value = input[[use_slots_id]])
          }
          
          # Copy basic property tax inputs
          min_id <- paste0(prop_type, "_min_existing")
          rate_id <- paste0(prop_type, "_rate_existing")
          
          if (!is.null(input[[min_id]])) {
            updateNumericInput(session, paste0(prop_type, "_min_scenario_a"), 
                              value = input[[min_id]])
          }
          
          if (!is.null(input[[rate_id]])) {
            updateNumericInput(session, paste0(prop_type, "_rate_scenario_a"), 
                              value = input[[rate_id]])
          }
          
          # Copy slots settings if they exist
          if (!is.null(input[[use_slots_id]]) && input[[use_slots_id]]) {
            for (slot in 1:3) {
              slot_inputs <- c("min", "max", "min_tax", "rate")
              for (slot_input in slot_inputs) {
                if (slot == 3 && slot_input == "max") next  # Skip max for slot 3
                
                input_id <- paste0(prop_type, "_slot", slot, "_", slot_input, "_existing")
                target_id <- paste0(prop_type, "_slot", slot, "_", slot_input, "_scenario_a")
                
                if (!is.null(input[[input_id]])) {
                  updateNumericInput(session, target_id, value = input[[input_id]])
                }
              }
            }
          }
        }
        
        # CORRECTED: Copy business license settings using new structure
        subcategories <- get_business_subcategories()
        for (subcategory in subcategories) {
          subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)
          
          # Check if this is a Portfolio subcategory
          default_config <- get_default_tax_config()
          is_portfolio_subcat <- subcategory %in% names(default_config$business_license$categories$Portfolio$subcategories)
          
          if (is_portfolio_subcat) {
            # Copy flat amount only
            flat_source <- paste0("bus_subcat_", subcategory_safe, "_flat_existing")
            flat_target <- paste0("bus_subcat_", subcategory_safe, "_flat_scenario_a")
            
            if (!is.null(input[[flat_source]])) {
              updateNumericInput(session, flat_target, value = input[[flat_source]])
            }
          } else {
            # Copy method selection
            method_source <- paste0("bus_subcat_", subcategory_safe, "_method_existing")
            method_target <- paste0("bus_subcat_", subcategory_safe, "_method_scenario_a")
            
            if (!is.null(input[[method_source]])) {
              updateSelectInput(session, method_target, selected = input[[method_source]])
            }
            
            # Copy use_slots checkbox
            use_slots_source <- paste0("bus_subcat_", subcategory_safe, "_use_slots_existing")
            use_slots_target <- paste0("bus_subcat_", subcategory_safe, "_use_slots_scenario_a")
            
            if (!is.null(input[[use_slots_source]])) {
              updateCheckboxInput(session, use_slots_target, value = input[[use_slots_source]])
            }
            
            # Copy minimum and rate
            min_source <- paste0("bus_subcat_", subcategory_safe, "_min_existing")
            rate_source <- paste0("bus_subcat_", subcategory_safe, "_rate_existing")
            
            if (!is.null(input[[min_source]])) {
              updateNumericInput(session, paste0("bus_subcat_", subcategory_safe, "_min_scenario_a"), 
                                value = input[[min_source]])
            }
            if (!is.null(input[[rate_source]])) {
              updateNumericInput(session, paste0("bus_subcat_", subcategory_safe, "_rate_scenario_a"), 
                                value = input[[rate_source]])
            }
            
            # Copy flat amount
            flat_source <- paste0("bus_subcat_", subcategory_safe, "_flat_existing")
            flat_target <- paste0("bus_subcat_", subcategory_safe, "_flat_scenario_a")
            
            if (!is.null(input[[flat_source]])) {
              updateNumericInput(session, flat_target, value = input[[flat_source]])
            }
            
            # Copy slots configuration if slots are being used
            if (!is.null(input[[use_slots_source]]) && input[[use_slots_source]]) {
              # Copy slot basis
              basis_source <- paste0("bus_subcat_", subcategory_safe, "_slot_basis_existing")
              basis_target <- paste0("bus_subcat_", subcategory_safe, "_slot_basis_scenario_a")
              
              if (!is.null(input[[basis_source]])) {
                updateRadioButtons(session, basis_target, selected = input[[basis_source]])
              }
              
              # Copy all slot configurations
              for (slot in 1:3) {
                slot_inputs <- c("min", "max", "min_tax", "rate")
                for (slot_input in slot_inputs) {
                  if (slot == 3 && slot_input == "max") next  # Skip max for slot 3
                  
                  input_source <- paste0("bus_subcat_", subcategory_safe, "_slot", slot, "_", slot_input, "_existing")
                  input_target <- paste0("bus_subcat_", subcategory_safe, "_slot", slot, "_", slot_input, "_scenario_a")
                  
                  if (!is.null(input[[input_source]])) {
                    updateNumericInput(session, input_target, value = input[[input_source]])
                  }
                }
              }
            }
          }
        }
        
        showNotification("Copied Existing Scenario to Scenario A", type = "message")
      }, error = function(e) {
        showNotification(paste("Error copying to Scenario A:", e$message), type = "error")
      })
    })

    # Copy from existing to scenario B (similar corrections)
    observeEvent(input$copy_existing_to_b, {

    })
      tryCatch({
        # Copy property tax settings
        for (prop_type in c("domestic", "commercial", "institutional")) {
          # Check if use_slots checkbox exists
          use_slots_id <- paste0("use_slots_", prop_type, "_existing")
          if (!is.null(input[[use_slots_id]])) {
            updateCheckboxInput(session, paste0("use_slots_", prop_type, "_scenario_b"), 
                               value = input[[use_slots_id]])
          }
          
          # Copy basic property tax inputs
          min_id <- paste0(prop_type, "_min_existing")
          rate_id <- paste0(prop_type, "_rate_existing")
          
          if (!is.null(input[[min_id]])) {
            updateNumericInput(session, paste0(prop_type, "_min_scenario_b"), 
                              value = input[[min_id]])
          }
          
          if (!is.null(input[[rate_id]])) {
            updateNumericInput(session, paste0(prop_type, "_rate_scenario_b"), 
                              value = input[[rate_id]])
          }
          
          # Copy slots settings if they exist
          if (!is.null(input[[use_slots_id]]) && input[[use_slots_id]]) {
            for (slot in 1:3) {
              slot_inputs <- c("min", "max", "min_tax", "rate")
              for (slot_input in slot_inputs) {
                if (slot == 3 && slot_input == "max") next  # Skip max for slot 3
                
                input_id <- paste0(prop_type, "_slot", slot, "_", slot_input, "_existing")
                target_id <- paste0(prop_type, "_slot", slot, "_", slot_input, "_scenario_b")
                
                if (!is.null(input[[input_id]])) {
                  updateNumericInput(session, target_id, value = input[[input_id]])
                }
              }
            }
          }
        }
        
        # Copy business license settings using new structure
        subcategories <- get_business_subcategories()
        for (subcategory in subcategories) {
          subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)
          
          # Check if this is a Portfolio subcategory
          default_config <- get_default_tax_config()
          is_portfolio_subcat <- subcategory %in% names(default_config$business_license$categories$Portfolio$subcategories)
          
          if (is_portfolio_subcat) {
            # Copy flat amount only
            flat_source <- paste0("bus_subcat_", subcategory_safe, "_flat_existing")
            flat_target <- paste0("bus_subcat_", subcategory_safe, "_flat_scenario_b")
            
            if (!is.null(input[[flat_source]])) {
              updateNumericInput(session, flat_target, value = input[[flat_source]])
            }
          } else {
            # Copy method selection
            method_source <- paste0("bus_subcat_", subcategory_safe, "_method_existing")
            method_target <- paste0("bus_subcat_", subcategory_safe, "_method_scenario_b")
            
            if (!is.null(input[[method_source]])) {
              updateSelectInput(session, method_target, selected = input[[method_source]])
            }
            
            # Copy use_slots checkbox
            use_slots_source <- paste0("bus_subcat_", subcategory_safe, "_use_slots_existing")
            use_slots_target <- paste0("bus_subcat_", subcategory_safe, "_use_slots_scenario_b")
            
            if (!is.null(input[[use_slots_source]])) {
              updateCheckboxInput(session, use_slots_target, value = input[[use_slots_source]])
            }
            
            # Copy minimum and rate
            min_source <- paste0("bus_subcat_", subcategory_safe, "_min_existing")
            rate_source <- paste0("bus_subcat_", subcategory_safe, "_rate_existing")
            
            if (!is.null(input[[min_source]])) {
              updateNumericInput(session, paste0("bus_subcat_", subcategory_safe, "_min_scenario_b"), 
                                value = input[[min_source]])
            }
            if (!is.null(input[[rate_source]])) {
              updateNumericInput(session, paste0("bus_subcat_", subcategory_safe, "_rate_scenario_b"), 
                                value = input[[rate_source]])
            }
            
            # Copy flat amount
            flat_source <- paste0("bus_subcat_", subcategory_safe, "_flat_existing")
            flat_target <- paste0("bus_subcat_", subcategory_safe, "_flat_scenario_b")
            
            if (!is.null(input[[flat_source]])) {
              updateNumericInput(session, flat_target, value = input[[flat_source]])
            }
            
            # Copy slots configuration if slots are being used
            if (!is.null(input[[use_slots_source]]) && input[[use_slots_source]]) {
              # Copy slot basis
              basis_source <- paste0("bus_subcat_", subcategory_safe, "_slot_basis_existing")
              basis_target <- paste0("bus_subcat_", subcategory_safe, "_slot_basis_scenario_b")
              
              if (!is.null(input[[basis_source]])) {
                updateRadioButtons(session, basis_target, selected = input[[basis_source]])
              }
              
              # Copy all slot configurations
              for (slot in 1:3) {
                slot_inputs <- c("min", "max", "min_tax", "rate")
                for (slot_input in slot_inputs) {
                  if (slot == 3 && slot_input == "max") next  # Skip max for slot 3
                  
                  input_source <- paste0("bus_subcat_", subcategory_safe, "_slot", slot, "_", slot_input, "_existing")
                  input_target <- paste0("bus_subcat_", subcategory_safe, "_slot", slot, "_", slot_input, "_scenario_b")
                  
                  if (!is.null(input[[input_source]])) {
                    updateNumericInput(session, input_target, value = input[[input_source]])
                  }
                }
              }
            }
          }
        }
        
        showNotification("Copied Existing Scenario to Scenario B", type = "message")
      }, error = function(e) {
        showNotification(paste("Error copying to Scenario B:", e$message), type = "error")
      })


    # Copy from scenario A to scenario B (similar corrections)
    observeEvent(input$copy_a_to_b, {

    })
tryCatch({
        # Copy property tax settings
        for (prop_type in c("domestic", "commercial", "institutional")) {
          # Check if use_slots checkbox exists
          use_slots_id <- paste0("use_slots_", prop_type, "_scenario_a")
          if (!is.null(input[[use_slots_id]])) {
            updateCheckboxInput(session, paste0("use_slots_", prop_type, "_scenario_b"), 
                               value = input[[use_slots_id]])
          }
          
          # Copy basic property tax inputs
          min_id <- paste0(prop_type, "_min_scenario_a")
          rate_id <- paste0(prop_type, "_rate_scenario_a")
          
          if (!is.null(input[[min_id]])) {
            updateNumericInput(session, paste0(prop_type, "_min_scenario_b"), 
                              value = input[[min_id]])
          }
          
          if (!is.null(input[[rate_id]])) {
            updateNumericInput(session, paste0(prop_type, "_rate_scenario_b"), 
                              value = input[[rate_id]])
          }
          
          # Copy slots settings if they exist
          if (!is.null(input[[use_slots_id]]) && input[[use_slots_id]]) {
            for (slot in 1:3) {
              slot_inputs <- c("min", "max", "min_tax", "rate")
              for (slot_input in slot_inputs) {
                if (slot == 3 && slot_input == "max") next  # Skip max for slot 3
                
                input_id <- paste0(prop_type, "_slot", slot, "_", slot_input, "_scenario_a")
                target_id <- paste0(prop_type, "_slot", slot, "_", slot_input, "_scenario_b")
                
                if (!is.null(input[[input_id]])) {
                  updateNumericInput(session, target_id, value = input[[input_id]])
                }
              }
            }
          }
        }
        
        # Copy business license settings using new structure
        subcategories <- get_business_subcategories()
        for (subcategory in subcategories) {
          subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)
          
          # Check if this is a Portfolio subcategory
          default_config <- get_default_tax_config()
          is_portfolio_subcat <- subcategory %in% names(default_config$business_license$categories$Portfolio$subcategories)
          
          if (is_portfolio_subcat) {
            # Copy flat amount only
            flat_source <- paste0("bus_subcat_", subcategory_safe, "_flat_scenario_a")
            flat_target <- paste0("bus_subcat_", subcategory_safe, "_flat_scenario_b")
            
            if (!is.null(input[[flat_source]])) {
              updateNumericInput(session, flat_target, value = input[[flat_source]])
            }
          } else {
            # Copy method selection
            method_source <- paste0("bus_subcat_", subcategory_safe, "_method_scenario_a")
            method_target <- paste0("bus_subcat_", subcategory_safe, "_method_scenario_b")
            
            if (!is.null(input[[method_source]])) {
              updateSelectInput(session, method_target, selected = input[[method_source]])
            }
            
            # Copy use_slots checkbox
            use_slots_source <- paste0("bus_subcat_", subcategory_safe, "_use_slots_scenario_a")
            use_slots_target <- paste0("bus_subcat_", subcategory_safe, "_use_slots_scenario_b")
            
            if (!is.null(input[[use_slots_source]])) {
              updateCheckboxInput(session, use_slots_target, value = input[[use_slots_source]])
            }
            
            # Copy minimum and rate
            min_source <- paste0("bus_subcat_", subcategory_safe, "_min_scenario_a")
            rate_source <- paste0("bus_subcat_", subcategory_safe, "_rate_scenario_a")
            
            if (!is.null(input[[min_source]])) {
              updateNumericInput(session, paste0("bus_subcat_", subcategory_safe, "_min_scenario_b"), 
                                value = input[[min_source]])
            }
            if (!is.null(input[[rate_source]])) {
              updateNumericInput(session, paste0("bus_subcat_", subcategory_safe, "_rate_scenario_b"), 
                                value = input[[rate_source]])
            }
            
            # Copy flat amount
            flat_source <- paste0("bus_subcat_", subcategory_safe, "_flat_scenario_a")
            flat_target <- paste0("bus_subcat_", subcategory_safe, "_flat_scenario_b")
            
            if (!is.null(input[[flat_source]])) {
              updateNumericInput(session, flat_target, value = input[[flat_source]])
            }
            
            # Copy slots configuration if slots are being used
            if (!is.null(input[[use_slots_source]]) && input[[use_slots_source]]) {
              # Copy slot basis
              basis_source <- paste0("bus_subcat_", subcategory_safe, "_slot_basis_scenario_a")
              basis_target <- paste0("bus_subcat_", subcategory_safe, "_slot_basis_scenario_b")
              
              if (!is.null(input[[basis_source]])) {
                updateRadioButtons(session, basis_target, selected = input[[basis_source]])
              }
              
              # Copy all slot configurations
              for (slot in 1:3) {
                slot_inputs <- c("min", "max", "min_tax", "rate")
                for (slot_input in slot_inputs) {
                  if (slot == 3 && slot_input == "max") next  # Skip max for slot 3
                  
                  input_source <- paste0("bus_subcat_", subcategory_safe, "_slot", slot, "_", slot_input, "_scenario_a")
                  input_target <- paste0("bus_subcat_", subcategory_safe, "_slot", slot, "_", slot_input, "_scenario_b")
                  
                  if (!is.null(input[[input_source]])) {
                    updateNumericInput(session, input_target, value = input[[input_source]])
                  }
                }
              }
            }
          }
        }
        
        showNotification("Copied Scenario A to Scenario B", type = "message")
      }, error = function(e) {
        showNotification(paste("Error copying Scenario A to Scenario B:", e$message), type = "error")
      })

    
# Reset function with comprehensive error handling
observeEvent(input$reset_all, {
  tryCatch({
    # Get default configuration
    defaults <- get_default_tax_config()
    
    if (is.null(defaults)) {
      showNotification("Error: Could not load default configuration", type = "error")
      return()
    }
    
    for (scenario in c("existing", "scenario_a", "scenario_b")) {
      # Reset property tax settings with error handling
      for (prop_type in c("domestic", "commercial", "institutional")) {
        tryCatch({
          # Reset use_slots checkbox
          updateCheckboxInput(session, paste0("use_slots_", prop_type, "_", scenario), 
                             value = FALSE)
          
          # Reset basic property tax inputs
          prop_config <- defaults$property_tax[[prop_type]]
          if (!is.null(prop_config)) {
            if (!is.null(prop_config$minimum)) {
              updateNumericInput(session, paste0(prop_type, "_min_", scenario), 
                               value = prop_config$minimum)
            }
            
            if (!is.null(prop_config$rate)) {
              updateNumericInput(session, paste0(prop_type, "_rate_", scenario), 
                               value = prop_config$rate * 100)  # Convert to percentage
            }
          }
        }, error = function(e) {
          warning(paste("Error resetting", prop_type, "property tax for", scenario, ":", e$message))
        })
      }
      
      # Reset business licenses using new structure
if (!is.null(values$config_categories)) {
  for (category in values$config_categories) {
    cat_id <- gsub("[^A-Za-z0-9]", "_", category)
    
    # Reset calculation method to default
    updateRadioButtons(session, paste0("bus_cat_", cat_id, "_method_", scenario), 
                      selected = "minimum_rate")
    
    # Reset minimum + rate inputs
    updateNumericInput(session, paste0("bus_cat_", cat_id, "_min_", scenario), value = 350)
    updateNumericInput(session, paste0("bus_cat_", cat_id, "_rate_", scenario), value = 3.5)
    
    # Reset value-based inputs
    updateCheckboxInput(session, paste0("bus_cat_", cat_id, "_use_value_bands_", scenario), value = FALSE)
    updateNumericInput(session, paste0("bus_cat_", cat_id, "_flat_value_", scenario), value = 1000)
    
    # Reset area-based inputs  
    updateCheckboxInput(session, paste0("bus_cat_", cat_id, "_use_area_bands_", scenario), value = FALSE)
    updateNumericInput(session, paste0("bus_cat_", cat_id, "_flat_area_", scenario), value = 10)
    
    # Reset band configurations (both value and area)
    for (band in 1:3) {
      if (band <= 2) {
        updateNumericInput(session, paste0("bus_cat_", cat_id, "_vband", band, "_max_", scenario), value = c(50000, 100000)[band])
        updateNumericInput(session, paste0("bus_cat_", cat_id, "_aband", band, "_max_", scenario), value = c(100, 500)[band])
      }
      updateNumericInput(session, paste0("bus_cat_", cat_id, "_vband", band, "_amount_", scenario), value = c(500, 1000, 2000)[band])
      updateNumericInput(session, paste0("bus_cat_", cat_id, "_aband", band, "_amount_", scenario), value = c(500, 1500, 3000)[band])
    }
  }
}
    }
    
    showNotification("Reset all scenarios to default values", type = "message")
  }, error = function(e) {
    showNotification(paste("Error during reset:", e$message), type = "error")
  })
})
    
    # Helper function to collect property tax configuration with enhanced error handling
    collect_property_tax_config <- function(scenario) {
      config <- list()
      
      tryCatch({
        for (prop_type in c("domestic", "commercial", "institutional")) {
          tryCatch({
            use_slots <- input[[paste0("use_slots_", prop_type, "_", scenario)]]
            
            if (is.null(use_slots) || !use_slots) {
              # Simple configuration
              min_val <- input[[paste0(prop_type, "_min_", scenario)]]
              rate_val <- input[[paste0(prop_type, "_rate_", scenario)]]
              
              # Validate inputs
              if (is.null(min_val)) min_val <- 0
              if (is.null(rate_val)) rate_val <- 0
              
              config[[prop_type]] <- list(
                use_slots = FALSE,
                minimum = min_val,
                rate = rate_val / 100  # Convert percentage to decimal
              )
            } else {
              # Slots configuration
              slots_config <- list()
              
              for (slot in 1:3) {
                slot_config <- list()
                
                # Get slot values with defaults
                slot_min <- input[[paste0(prop_type, "_slot", slot, "_min_", scenario)]]
                slot_max <- if (slot == 3) Inf else input[[paste0(prop_type, "_slot", slot, "_max_", scenario)]]
                slot_min_tax <- input[[paste0(prop_type, "_slot", slot, "_min_tax_", scenario)]]
                slot_rate <- input[[paste0(prop_type, "_slot", slot, "_rate_", scenario)]]
                
                # Apply defaults for missing values
                if (is.null(slot_min)) slot_min <- 0
                if (is.null(slot_max) && slot != 3) slot_max <- 1000000
                if (is.null(slot_min_tax)) slot_min_tax <- 0
                if (is.null(slot_rate)) slot_rate <- 0
                
                slot_config <- list(
                  min = slot_min,
                  max = slot_max,
                  minimum = slot_min_tax,
                  rate = slot_rate / 100  # Convert percentage to decimal
                )
                
                slots_config[[paste0("slot", slot)]] <- slot_config
              }
              
              config[[prop_type]] <- list(
                use_slots = TRUE,
                slots = slots_config
              )
            }
          }, error = function(e) {
            warning(paste("Error collecting config for", prop_type, "in", scenario, ":", e$message))
            # Provide default config for this property type
            config[[prop_type]] <<- list(
              use_slots = FALSE,
              minimum = 0,
              rate = 0
            )
          })
        }
        
        return(config)
      }, error = function(e) {
        showNotification(paste("Error collecting property tax config for", scenario, ":", e$message), type = "error")
        # Return default config
        return(list(
          domestic = list(use_slots = FALSE, minimum = 0, rate = 0),
          commercial = list(use_slots = FALSE, minimum = 0, rate = 0),
          institutional = list(use_slots = FALSE, minimum = 0, rate = 0)
        ))
      })
    }
    
collect_business_license_config <- function(scenario) {
  config <- list()
  
  subcategories <- get_business_subcategories()
  
  for (subcategory in subcategories) {
    subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)
    
    # Check if this is a Portfolio subcategory
    default_config <- get_default_tax_config()
    is_portfolio_subcat <- subcategory %in% names(default_config$business_license$categories$Portfolio$subcategories)
    
    if (is_portfolio_subcat) {
      # Portfolio subcategories only have flat amount
      flat_amount <- input[[paste0("bus_subcat_", subcategory_safe, "_flat_", scenario)]]
      
      config[[subcategory]] <- list(
        calculation_method = "flat",
        flat_amount = if(is.null(flat_amount)) 1000 else flat_amount
      )
    } else {
      # Regular categories - check method selection
      method <- input[[paste0("bus_subcat_", subcategory_safe, "_method_", scenario)]]
      if (is.null(method)) method <- "min_rate"
      
      if (method == "min_rate") {
        # Check if using slots
        use_slots <- input[[paste0("bus_subcat_", subcategory_safe, "_use_slots_", scenario)]]
        
        if (!is.null(use_slots) && use_slots) {
          # Collect slots configuration
          slot_basis <- input[[paste0("bus_subcat_", subcategory_safe, "_slot_basis_", scenario)]]
          if (is.null(slot_basis)) slot_basis <- "value"
          
          slots_config <- list()
          for (slot in 1:3) {
            slot_min <- input[[paste0("bus_subcat_", subcategory_safe, "_slot", slot, "_min_", scenario)]]
            slot_max <- if (slot == 3) Inf else input[[paste0("bus_subcat_", subcategory_safe, "_slot", slot, "_max_", scenario)]]
            slot_min_tax <- input[[paste0("bus_subcat_", subcategory_safe, "_slot", slot, "_min_tax_", scenario)]]
            slot_rate <- input[[paste0("bus_subcat_", subcategory_safe, "_slot", slot, "_rate_", scenario)]]
            
            slots_config[[paste0("slot", slot)]] <- list(
              min = if(is.null(slot_min)) 0 else slot_min,
              max = if(is.null(slot_max) && slot != 3) 100000 else slot_max,
              minimum = if(is.null(slot_min_tax)) 200 else slot_min_tax,
              rate = if(is.null(slot_rate)) 0.005 else slot_rate / 100
            )
          }
          
          config[[subcategory]] <- list(
            calculation_method = "slots",
            slot_basis = slot_basis,
            slots = slots_config
          )
        } else {
          # Simple minimum + rate
          minimum <- input[[paste0("bus_subcat_", subcategory_safe, "_min_", scenario)]]
          rate <- input[[paste0("bus_subcat_", subcategory_safe, "_rate_", scenario)]]
          
          config[[subcategory]] <- list(
            calculation_method = "minimum_rate",
            minimum = if(is.null(minimum)) 350 else minimum,
            rate = if(is.null(rate)) 0.035 else rate / 100
          )
        }
      } else if (method == "flat") {
        # Flat tax amount
        flat_amount <- input[[paste0("bus_subcat_", subcategory_safe, "_flat_", scenario)]]
        
        config[[subcategory]] <- list(
          calculation_method = "flat",
          flat_amount = if(is.null(flat_amount)) 1000 else flat_amount
        )
      }
    }
  }
  
  return(config)
}
    
    # CORRECTED: Business license calculation in preview
    observeEvent(input$calculate_preview, {
      req(processed_data())
      
      withProgress(message = 'Calculating tax preview...', value = 0, {
        tryCatch({
      data <- processed_data()
      scenario <- input$preview_scenario
      
      # Sample first 100 rows for performance
      n_rows <- min(100, nrow(data))
      preview_data <- data[1:n_rows, ]
      
      incProgress(0.2, detail = "Getting configurations...")
      
      # Get property values from Module 2 with error handling
      if (!is.null(property_configs)) {
        tryCatch({
          module2_config <- property_configs()[[scenario]]
          
          # Calculate inflation-adjusted base value
          inflation_adjusted_base <- module2_config$base_value * (1 + module2_config$inflation)
          area_weight <- module2_config$area_weight
          
          # Get property areas
          property_areas <- if("property_area" %in% names(preview_data)) {
            preview_data$property_area
          } else {
            rep(1000, n_rows)  # Default area
          }
          
          incProgress(0.2, detail = "Calculating feature weights...")
          
          # Calculate feature weights product for each property
          all_features <- names(module2_config$feature_weights)
          product_weights <- rep(1, n_rows)
          
          for (feat in all_features) {
            tryCatch({
              if (feat %in% names(preview_data)) {
                weight <- module2_config$feature_weights[[feat]]
                if (!is.null(weight) && !is.na(weight)) {
                  feature_multiplier <- ifelse(preview_data[[feat]] == 1, 
                                               (weight/100 + 1), 
                                               1)
                  product_weights <- product_weights * feature_multiplier
                }
              }
            }, error = function(e) {
              warning(paste("Error processing feature", feat, ":", e$message))
            })
          }
          
          incProgress(0.1, detail = "Calculating structure weights...")
          
          # Calculate structure type weights
          all_structures <- names(module2_config$structure_weights)
          structure_matrix <- matrix(0, nrow = n_rows, ncol = length(all_structures))
          weight_vector <- numeric(length(all_structures))
          
          for (j in seq_along(all_structures)) {
            tryCatch({
              struct <- all_structures[j]
              
              if (struct %in% names(preview_data)) {
                col_values <- preview_data[[struct]]
                structure_matrix[, j] <- ifelse(!is.na(col_values) & col_values == 1, 1, 0)
                
                weight <- module2_config$structure_weights[[struct]]
                if (is.null(weight)) weight <- 0
                weight_vector[j] <- weight
              }
            }, error = function(e) {
              warning(paste("Error processing structure", struct, ":", e$message))
            })
          }
          
          structure_weights <- structure_matrix %*% weight_vector
          structure_weights <- as.vector(structure_weights)
          structure_multipliers <- (structure_weights/100 + 1)
          
          # Calculate property values
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
            property_areas  # Use property areas as fallback
          }
          
          business_values <- ifelse(!is.na(business_areas) & business_areas > 0,
                                    inflation_adjusted_base * 
                                      (business_areas ^ area_weight) * 
                                      product_weights * 
                                      structure_multipliers,
                                    NA)
          
        }, error = function(e) {
          showNotification(paste("Error calculating property values:", e$message), type = "warning")
          # Fallback values
          property_values <- rep(100000, n_rows)  # Default property value
          business_values <- rep(50000, n_rows)   # Default business value
        })
      } else {
        # No property configs available - use defaults
        property_values <- rep(100000, n_rows)
        business_values <- rep(50000, n_rows)
      }
      
      incProgress(0.2, detail = "Calculating property taxes...")
      
      # Get property types with error handling
      property_types <- tryCatch({
        if("property_type_Domestic" %in% names(preview_data)) {
          ifelse(preview_data$property_type_Domestic == 1, "domestic",
                 ifelse(preview_data$property_type_Commercial == 1, "commercial",
                        ifelse(preview_data$property_type_Institutional == 1, "institutional", "domestic")))
        } else {
          rep("domestic", n_rows)
        }
      }, error = function(e) {
        rep("domestic", n_rows)  # Default to domestic
      })
      
      # Calculate property taxes with error handling
      tax_config <- collect_property_tax_config(scenario)
      property_taxes <- numeric(n_rows)
      tax_rates <- numeric(n_rows)
      tax_slots <- numeric(n_rows)
      
      for (i in 1:n_rows) {
        tryCatch({
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
            property_taxes[i] <- max(prop_value * type_config$rate, type_config$minimum)
            tax_rates[i] <- type_config$rate * 100  # Convert to percentage for display
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
            property_taxes[i] <- max(prop_value * slot_config$rate, slot_config$minimum)
            tax_rates[i] <- slot_config$rate * 100  # Convert to percentage for display
            tax_slots[i] <- slot_num
          }
        }, error = function(e) {
          warning(paste("Error calculating property tax for row", i, ":", e$message))
          property_taxes[i] <- 0
          tax_rates[i] <- 0
          tax_slots[i] <- NA
        })
      }
      
      incProgress(0.2, detail = "Calculating business licenses...")
      
      # Calculate business licenses with error handling
      business_config <- collect_business_license_config(scenario)
      business_subcategories <- tryCatch({
        if("business_sub_category" %in% names(preview_data)) {
          preview_data$business_sub_category
        } else {
          rep(NA, n_rows)
        }
      }, error = function(e) {
        rep(NA, n_rows)
      })
      
      business_licenses <- rep(0, n_rows)
      
# CORRECTED: Business license calculation in preview
for (i in 1:n_rows) {
  subcat <- business_subcategories[i]
  if (!is.na(subcat) && subcat %in% names(business_config)) {
    subcat_config <- business_config[[subcat]]
    
    # CORRECTED: Use calculation_method instead of tax_type
    if (subcat_config$calculation_method == "minimum_rate") {
      business_licenses[i] <- max(business_values[i] * subcat_config$rate, 
                                  subcat_config$minimum)
                                  
    } else if (subcat_config$calculation_method == "flat") {
      # Simple flat amount
      business_licenses[i] <- subcat_config$flat_amount
      
    } else if (subcat_config$calculation_method == "slots") {
      # Handle slots calculation
      slot_basis_value <- if (subcat_config$slot_basis == "area") {
        if("business_area" %in% names(preview_data)) {
          preview_data$business_area[i]
        } else {
          property_areas[i]  # Use property area as fallback
        }
      } else {
        business_values[i]  # Value-based
      }
      
      # Find which slot applies
      slot_num <- 3  # Default to highest slot
      for (s in 1:3) {
        slot <- subcat_config$slots[[paste0("slot", s)]]
        if (slot_basis_value >= slot$min && slot_basis_value < slot$max) {
          slot_num <- s
          break
        }
      }
      
      slot_config <- subcat_config$slots[[paste0("slot", slot_num)]]
      business_licenses[i] <- max(slot_basis_value * slot_config$rate, slot_config$minimum)
    }
  }
}
      
      incProgress(0.1, detail = "Creating preview table...")
      
      # Create preview dataframe with error handling
      values$preview_data <- tryCatch({
        data.frame(
          id_property = preview_data$id_property,
          property_type = property_types,
          property_value = round(property_values, 2),
          property_tax = round(property_taxes, 2),
          tax_rate_used = round(tax_rates, 2),
          tax_slot = tax_slots,
          business_subcategory = business_subcategories,
          business_value = round(business_values, 2),
          business_area = round(if("business_area" %in% names(preview_data)) preview_data$business_area else property_areas, 2),
          business_license = round(business_licenses, 2),
          total_tax = round(property_taxes + business_licenses, 2),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        showNotification(paste("Error creating preview table:", e$message), type = "error")
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
      })
      
      showNotification("Tax preview calculated successfully", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in preview calculation:", e$message), type = "error")
      values$preview_data <- NULL
    })
  })
})

# Display preview table with enhanced error handling
output$tax_preview_table <- DT::renderDataTable({
  tryCatch({
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
  }, error = function(e) {
    showNotification(paste("Error displaying preview table:", e$message), type = "error")
    DT::datatable(data.frame(Error = "Unable to display preview table"))
  })
})

# Return tax configurations for use in other modules
return(reactive({
  tryCatch({
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
  }, error = function(e) {
    showNotification(paste("Error collecting tax configurations:", e$message), type = "error")
    # Return default empty configuration
    list(
      existing = list(property_tax = list(), business_license = list()),
      scenario_a = list(property_tax = list(), business_license = list()),
      scenario_b = list(property_tax = list(), business_license = list())
    )
  })
}))

  })  # Close the moduleServer function
}     # Close the module3_server function