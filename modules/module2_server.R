# modules/module2_server.R

module2_server <- function(id, processed_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store configurations
    values <- reactiveValues(
      existing_config = NULL,
      alt_a_config = NULL,
      alt_b_config = NULL,
      current_scenario = "existing",
      property_types = NULL,
      property_features = NULL,
      business_categories = NULL
    )
    
    # Initialize when data is available
    observeEvent(processed_data(), {
      req(processed_data())
      data <- processed_data()
      
      # Extract categories
      values$property_types <- get_property_types(data)
      values$property_features <- get_property_features(data)
      values$business_categories <- get_business_categories(data)
      
      # Create default configurations if not exists
      if (is.null(values$existing_config)) {
        values$existing_config <- create_default_config(data)
      }
      if (is.null(values$alt_a_config)) {
        values$alt_a_config <- create_default_config(data)
      }
      if (is.null(values$alt_b_config)) {
        values$alt_b_config <- create_default_config(data)
      }
    })
    
    # Update current scenario
    observeEvent(input$scenario_select, {
      values$current_scenario <- input$scenario_select
    })
    
    # Get current configuration
    current_config <- reactive({
      switch(values$current_scenario,
             "existing" = values$existing_config,
             "alt_a" = values$alt_a_config,
             "alt_b" = values$alt_b_config
      )
    })
    
    # Copy from existing scenario
    observeEvent(input$copy_existing, {
      if (values$current_scenario != "existing" && !is.null(values$existing_config)) {
        if (values$current_scenario == "alt_a") {
          values$alt_a_config <- values$existing_config
        } else if (values$current_scenario == "alt_b") {
          values$alt_b_config <- values$existing_config
        }
        showNotification("Configuration copied from Existing scenario", 
                         type = "message")
      }
    })
    
    # Generate property types UI
    output$property_types_ui <- renderUI({
      req(values$property_types)
      config <- current_config()
      req(config)
      
      ui_list <- lapply(values$property_types, function(type) {
        clean_type <- gsub("_", " ", type)
        clean_type <- tools::toTitleCase(clean_type)
        
        fluidRow(
          column(4,
                 h5(clean_type)
          ),
          column(4,
                 numericInput(
                   ns(paste0("prop_rate_", type)),
                   "Tax Rate (%):",
                   value = config$property_types[[type]]$tax_rate * 100,
                   min = 0,
                   max = 100,
                   step = 0.01
                 )
          ),
          column(4,
                 numericInput(
                   ns(paste0("prop_min_", type)),
                   "Minimum Tax ($):",
                   value = config$property_types[[type]]$minimum_tax,
                   min = 0,
                   step = 10
                 )
          )
        )
      })
      
      do.call(tagList, ui_list)
    })
    
    # Generate property features UI
    output$property_features_ui <- renderUI({
      req(values$property_features)
      config <- current_config()
      req(config)
      
      # Group features by type
      area_features <- grep("property_area|coordinate", values$property_features, value = TRUE)
      condition_features <- grep("condition", values$property_features, value = TRUE)
      material_features <- grep("material", values$property_features, value = TRUE)
      amenity_features <- grep("^has_", values$property_features, value = TRUE)
      access_features <- grep("access|quality|drainage", values$property_features, value = TRUE)
      location_features <- grep("tourist|hazard|visibility|settlement|corridor", 
                                values$property_features, value = TRUE)
      
      other_features <- setdiff(values$property_features, 
                                c(area_features, condition_features, material_features,
                                  amenity_features, access_features, location_features))
      
      tagList(
        if (length(area_features) > 0) {
          tagList(
            h4("Area and Location"),
            create_feature_inputs(area_features, config, ns)
          )
        },
        
        if (length(condition_features) > 0) {
          tagList(
            hr(),
            h4("Condition Features"),
            create_feature_inputs(condition_features, config, ns)
          )
        },
        
        if (length(material_features) > 0) {
          tagList(
            hr(),
            h4("Material Features"),
            create_feature_inputs(material_features, config, ns)
          )
        },
        
        if (length(amenity_features) > 0) {
          tagList(
            hr(),
            h4("Amenities"),
            create_feature_inputs(amenity_features, config, ns)
          )
        },
        
        if (length(access_features) > 0) {
          tagList(
            hr(),
            h4("Access and Infrastructure"),
            create_feature_inputs(access_features, config, ns)
          )
        },
        
        if (length(location_features) > 0) {
          tagList(
            hr(),
            h4("Location Characteristics"),
            create_feature_inputs(location_features, config, ns)
          )
        },
        
        if (length(other_features) > 0) {
          tagList(
            hr(),
            h4("Other Features"),
            create_feature_inputs(other_features, config, ns)
          )
        }
      )
    })
    
    # Helper function to create feature inputs
    create_feature_inputs <- function(features, config, ns) {
      ui_list <- lapply(features, function(feature) {
        clean_feature <- gsub("_", " ", feature)
        clean_feature <- gsub("^has ", "Has ", clean_feature)
        clean_feature <- tools::toTitleCase(clean_feature)
        
        weight_value <- ifelse(
          !is.null(config$property_features[[feature]]),
          config$property_features[[feature]],
          0.5
        )
        
        fluidRow(
          column(6,
                 p(clean_feature, style = "margin-top: 7px;")
          ),
          column(6,
                 sliderInput(
                   ns(paste0("weight_", feature)),
                   label = NULL,
                   min = 0,
                   max = 2,
                   value = weight_value,
                   step = 0.05,
                   width = "100%"
                 )
          )
        )
      })
      
      do.call(tagList, ui_list)
    }
    
    # Generate business categories UI
    output$business_categories_ui <- renderUI({
      req(values$business_categories)
      config <- current_config()
      req(config)
      
      if (length(values$business_categories) == 0) {
        return(p("No business categories found in the data."))
      }
      
      ui_list <- lapply(values$business_categories, function(category) {
        clean_category <- tools::toTitleCase(category)
        
        fluidRow(
          column(4,
                 h5(clean_category)
          ),
          column(4,
                 numericInput(
                   ns(paste0("bus_rate_", make.names(category))),
                   "License Rate (%):",
                   value = config$business_categories[[category]]$tax_rate * 100,
                   min = 0,
                   max = 100,
                   step = 0.01
                 )
          ),
          column(4,
                 numericInput(
                   ns(paste0("bus_min_", make.names(category))),
                   "Minimum License ($):",
                   value = config$business_categories[[category]]$minimum_tax,
                   min = 0,
                   step = 10
                 )
          )
        )
      })
      
      do.call(tagList, ui_list)
    })
    
    # Reset weights to defaults
    observeEvent(input$reset_weights, {
      showNotification("Weights reset to defaults", type = "message")
      # This will trigger UI regeneration with default values
      config <- current_config()
      if (!is.null(config)) {
        default_config <- create_default_config(processed_data())
        config$property_features <- default_config$property_features
        
        # Update the appropriate scenario
        if (values$current_scenario == "existing") {
          values$existing_config <- config
        } else if (values$current_scenario == "alt_a") {
          values$alt_a_config <- config
        } else if (values$current_scenario == "alt_b") {
          values$alt_b_config <- config
        }
      }
    })
    
    # Normalize weights
    observeEvent(input$normalize_weights, {
      # Collect current weights
      current_weights <- list()
      for (feature in values$property_features) {
        weight_input <- input[[paste0("weight_", feature)]]
        if (!is.null(weight_input)) {
          current_weights[[feature]] <- weight_input
        }
      }
      
      # Normalize to sum to number of features
      if (length(current_weights) > 0) {
        total <- sum(unlist(current_weights))
        if (total > 0) {
          factor <- length(current_weights) / total
          for (feature in names(current_weights)) {
            updateSliderInput(session, 
                              paste0("weight_", feature),
                              value = current_weights[[feature]] * factor)
          }
          showNotification("Weights normalized", type = "message")
        }
      }
    })
    
    # Save configuration
    observeEvent(input$save_config, {
      # Collect all current values
      config <- list(
        inflation_adjustment = input$inflation_rate,
        property_types = list(),
        property_features = list(),
        business_categories = list(),
        area_bands = list(),
        value_bands = list()
      )
      
      # Collect property type settings
      for (type in values$property_types) {
        rate_input <- input[[paste0("prop_rate_", type)]]
        min_input <- input[[paste0("prop_min_", type)]]
        
        if (!is.null(rate_input) && !is.null(min_input)) {
          config$property_types[[type]] <- list(
            tax_rate = rate_input / 100,
            minimum_tax = min_input
          )
        }
      }
      
      # Collect feature weights
      for (feature in values$property_features) {
        weight_input <- input[[paste0("weight_", feature)]]
        if (!is.null(weight_input)) {
          config$property_features[[feature]] <- weight_input
        }
      }
      
      # Collect business category settings
      for (category in values$business_categories) {
        safe_category <- make.names(category)
        rate_input <- input[[paste0("bus_rate_", safe_category)]]
        min_input <- input[[paste0("bus_min_", safe_category)]]
        
        if (!is.null(rate_input) && !is.null(min_input)) {
          config$business_categories[[category]] <- list(
            tax_rate = rate_input / 100,
            minimum_tax = min_input
          )
        }
      }
      
      # Collect area bands
      if (!is.null(input$area_band1_max)) {
        config$area_bands <- list(
          list(max = input$area_band1_max, rate = input$area_band1_rate / 100),
          list(max = input$area_band2_max, rate = input$area_band2_rate / 100),
          list(max = input$area_band3_max, rate = input$area_band3_rate / 100),
          list(max = Inf, rate = input$area_band4_rate / 100)
        )
      }
      
      # Collect value bands
      if (!is.null(input$value_band1_max)) {
        config$value_bands <- list(
          list(max = input$value_band1_max, rate = input$value_band1_rate / 100),
          list(max = input$value_band2_max, rate = input$value_band2_rate / 100),
          list(max = input$value_band3_max, rate = input$value_band3_rate / 100),
          list(max = Inf, rate = input$value_band4_rate / 100)
        )
      }
      
      # Save to appropriate scenario
      if (values$current_scenario == "existing") {
        values$existing_config <- config
      } else if (values$current_scenario == "alt_a") {
        values$alt_a_config <- config
      } else if (values$current_scenario == "alt_b") {
        values$alt_b_config <- config
      }
      
      # Save to file
      filename <- save_configuration(config, values$current_scenario)
      
      output$save_status <- renderText({
        paste("✓ Configuration saved successfully!\n",
              "Scenario:", values$current_scenario, "\n",
              "File:", filename)
      })
      
      showNotification("Configuration saved!", type = "message")
    })
    
    # Configuration summary
    output$config_summary <- renderPrint({
      config <- current_config()
      req(config)
      
      cat("=== Configuration Summary ===\n\n")
      cat("Scenario:", values$current_scenario, "\n")
      cat("Inflation Adjustment:", config$inflation_adjustment, "%\n\n")
      
      cat("Property Types:\n")
      for (type in names(config$property_types)) {
        cat("  ", type, ": Rate =", 
            config$property_types[[type]]$tax_rate * 100, 
            "%, Minimum = $",
            config$property_types[[type]]$minimum_tax, "\n")
      }
      
      cat("\nBusiness Categories:\n")
      for (cat in names(config$business_categories)) {
        cat("  ", cat, ": Rate =", 
            config$business_categories[[cat]]$tax_rate * 100, 
            "%, Minimum = $",
            config$business_categories[[cat]]$minimum_tax, "\n")
      }
      
      cat("\nProperty Feature Weights:\n")
      cat("  Total features:", length(config$property_features), "\n")
      cat("  Average weight:", 
          round(mean(unlist(config$property_features)), 3), "\n")
    })
    
    # Scenario comparison table
    output$scenario_comparison <- DT::renderDataTable({
      req(values$existing_config, values$alt_a_config, values$alt_b_config)
      
      # Create comparison data
      comparison_data <- data.frame(
        Parameter = character(),
        Existing = character(),
        `Alternative A` = character(),
        `Alternative B` = character(),
        stringsAsFactors = FALSE
      )
      
      # Add inflation
      comparison_data <- rbind(comparison_data, data.frame(
        Parameter = "Inflation Adjustment",
        Existing = paste0(values$existing_config$inflation_adjustment, "%"),
        `Alternative A` = paste0(values$alt_a_config$inflation_adjustment, "%"),
        `Alternative B` = paste0(values$alt_b_config$inflation_adjustment, "%"),
        stringsAsFactors = FALSE
      ))
      
      # Add property types
      all_prop_types <- unique(c(
        names(values$existing_config$property_types),
        names(values$alt_a_config$property_types),
        names(values$alt_b_config$property_types)
      ))
      
      for (type in all_prop_types) {
        existing_rate <- values$existing_config$property_types[[type]]$tax_rate * 100
        alt_a_rate <- values$alt_a_config$property_types[[type]]$tax_rate * 100
        alt_b_rate <- values$alt_b_config$property_types[[type]]$tax_rate * 100
        
        comparison_data <- rbind(comparison_data, data.frame(
          Parameter = paste("Property:", type),
          Existing = paste0(existing_rate, "%"),
          `Alternative A` = paste0(alt_a_rate, "%"),
          `Alternative B` = paste0(alt_b_rate, "%"),
          stringsAsFactors = FALSE
        ))
      }
      
      names(comparison_data) <- c("Parameter", "Existing", "Alternative A", "Alternative B")
      
      DT::datatable(
        comparison_data,
        options = list(
          pageLength = 20,
          dom = 'Bfrtip'
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          columns = c("Alternative A", "Alternative B"),
          backgroundColor = DT::styleInterval(0, c("white", "#e6f3ff"))
        )
    })
    
    # Download configuration
    output$download_config <- downloadHandler(
      filename = function() {
        paste0("config_", values$current_scenario, "_", Sys.Date(), ".rds")
      },
      content = function(file) {
        config <- current_config()
        saveRDS(config, file)
      }
    )
    
    # Return configurations for use in other modules
    return(reactive({
      list(
        existing = values$existing_config,
        alt_a = values$alt_a_config,
        alt_b = values$alt_b_config,
        property_features = values$property_features,
        property_types = values$property_types,
        business_categories = values$business_categories
      )
    }))
  })
}