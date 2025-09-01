# modules/module4_server.R

module4_server <- function(id, processed_data, property_configs, tax_configs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store calculated revenue data
    values <- reactiveValues(
      revenue_data = NULL,
      calculations_done = FALSE
    )
    
    # Main revenue calculation
    observeEvent(input$calculate_revenue, {
      req(processed_data(), property_configs(), tax_configs())
      
      tryCatch({
        withProgress(message = 'Calculating revenue...', value = 0, {
          data <- processed_data()
          
          # Initialize results storage
          all_results <- list()
          
          # Calculate for each scenario
          scenarios <- c("existing", "scenario_a", "scenario_b")
          
          for (scenario in scenarios) {
            incProgress(0.25, detail = paste("Processing", scenario, "scenario..."))
            
            # Get configurations
            prop_config <- property_configs()[[scenario]]
            tax_config <- tax_configs()[[scenario]]
            
            # Check if configurations exist
            if (is.null(prop_config)) {
              showNotification(paste("Missing property configuration for", scenario), type = "error")
              next
            }
            
            if (is.null(tax_config)) {
              showNotification(paste("Missing tax configuration for", scenario), type = "error")
              next
            }
            
            # Calculate property and business values
            calc_result <- calculate_values_and_taxes(
              data, 
              prop_config, 
              tax_config,
              scenario
            )
            
            all_results[[scenario]] <- calc_result
          }
          
          incProgress(0.2, detail = "Combining results...")
          
          # Check if we have results
          if (length(all_results) == 0) {
            showNotification("No results calculated. Please check configurations.", type = "error")
            return()
          }
          
          # Combine all scenarios into one dataset for comparison
          values$revenue_data <- all_results
          values$calculations_done <- TRUE
          
          # Update filter choices
          update_filter_choices()
          
          incProgress(0.05, detail = "Complete!")
        })
        
        showNotification("Revenue calculations completed!", type = "message", duration = 5)
        
      }, error = function(e) {
        showNotification(paste("Error in revenue calculation:", e$message), type = "error", duration = 10)
        print(paste("Detailed error:", e))
      })
    })
    
    # Helper function to calculate values and taxes for a scenario
    calculate_values_and_taxes <- function(data, prop_config, tax_config, scenario_name) {
      n_rows <- nrow(data)
      
      # Calculate property values using Module 2 configurations
      property_values <- calculate_property_values_module2(data, prop_config)
      business_values <- calculate_business_values_module2(data, prop_config)
      
      # Get property types
      property_types <- get_property_types(data)
      
      # Calculate property taxes using Module 3 configurations
      property_taxes <- calculate_property_taxes_module3(
        property_values, 
        property_types, 
        tax_config$property_tax
      )
      
      # Calculate business licenses
      business_areas <- if("business_area" %in% names(data)) data$business_area else rep(NA, n_rows)
      business_subcats <- if("business_sub_category" %in% names(data)) data$business_sub_category else rep(NA, n_rows)
      
      business_licenses <- calculate_business_licenses_module3(
        business_values,
        business_areas,
        business_subcats,
        tax_config$business_license
      )
      
      # Extract structure type info for filtering
      structure_types <- get_structure_types_from_data(data)
      
      # Combine into result dataframe
      result <- data.frame(
        id_property = data$id_property,
        scenario = scenario_name,
        property_type = property_types,
        structure_type = structure_types,
        property_value = property_values,
        property_tax = property_taxes,
        business_value = business_values,
        business_license = business_licenses,
        total_tax = property_taxes + business_licenses,
        made_payment = if("made_payment" %in% names(data)) data$made_payment else TRUE,
        business_category = if("business_category" %in% names(data)) data$business_category else NA,
        business_sub_category = if("business_sub_category" %in% names(data)) data$business_sub_category else NA
      )
      
      return(result)
    }
    
    # Get structure types from data
    get_structure_types_from_data <- function(data) {
      n_rows <- nrow(data)
      structure_types <- rep("None", n_rows)
      
      # Check commercial types
      commercial_cols <- names(data)[grepl("^commercial_type_", names(data))]
      for (col in commercial_cols) {
        type_name <- gsub("commercial_type_", "", col)
        if (type_name != "NA") {
          structure_types[data[[col]] == 1] <- paste("Commercial:", type_name)
        }
      }
      
      # Check institutional types
      institutional_cols <- names(data)[grepl("^institutional_type_", names(data))]
      for (col in institutional_cols) {
        type_name <- gsub("institutional_type_", "", col)
        if (type_name != "NA") {
          structure_types[data[[col]] == 1] <- paste("Institutional:", type_name)
        }
      }
      
      return(structure_types)
    }
    
    # Calculate property values using Module 2 logic
    calculate_property_values_module2 <- function(data, config) {
      n_rows <- nrow(data)
      
      # Calculate inflation-adjusted base value
      inflation_adjusted_base <- config$base_value * (1 + config$inflation)
      area_weight <- config$area_weight
      
      # Get property areas
      property_areas <- if("property_area" %in% names(data)) {
        data$property_area
      } else {
        rep(100, n_rows)  # Default area
      }
      
      # Calculate feature weights product
      all_features <- names(config$feature_weights)
      product_weights <- rep(1, n_rows)
      
      for (feat in all_features) {
        if (feat %in% names(data)) {
          weight <- config$feature_weights[[feat]]
          if (!is.null(weight) && !is.na(weight)) {
            feature_multiplier <- ifelse(data[[feat]] == 1, 
                                         (weight/100 + 1), 
                                         1)
            product_weights <- product_weights * feature_multiplier
          }
        }
      }
      
      # Calculate structure type weights
      all_structures <- names(config$structure_weights)
      structure_weights <- rep(0, n_rows)
      
      for (struct in all_structures) {
        if (struct %in% names(data)) {
          weight <- config$structure_weights[[struct]]
          if (!is.null(weight) && !is.na(weight)) {
            structure_weights <- ifelse(data[[struct]] == 1, weight, structure_weights)
          }
        }
      }
      
      # Apply structure type weights as multipliers
      structure_multipliers <- (structure_weights/100 + 1)
      
      # Calculate property values
      property_values <- ifelse(is.na(property_areas) | property_areas <= 0,
                                0,
                                inflation_adjusted_base * 
                                  (property_areas ^ area_weight) * 
                                  product_weights * 
                                  structure_multipliers)
      
      return(property_values)
    }
    
    # Calculate business values using Module 2 logic
    calculate_business_values_module2 <- function(data, config) {
      n_rows <- nrow(data)
      
      # Similar to property values but using business_area
      business_areas <- if("business_area" %in% names(data)) {
        data$business_area
      } else {
        rep(NA, n_rows)
      }
      
      # Only calculate for properties with businesses
      if (all(is.na(business_areas))) {
        return(rep(0, n_rows))
      }
      
      # Use same configuration as properties for now
      inflation_adjusted_base <- config$base_value * (1 + config$inflation)
      area_weight <- config$area_weight
      
      # Calculate feature weights product (same as properties)
      all_features <- names(config$feature_weights)
      product_weights <- rep(1, n_rows)
      
      for (feat in all_features) {
        if (feat %in% names(data)) {
          weight <- config$feature_weights[[feat]]
          if (!is.null(weight) && !is.na(weight)) {
            feature_multiplier <- ifelse(data[[feat]] == 1, 
                                         (weight/100 + 1), 
                                         1)
            product_weights <- product_weights * feature_multiplier
          }
        }
      }
      
      # Calculate structure type weights
      all_structures <- names(config$structure_weights)
      structure_weights <- rep(0, n_rows)
      
      for (struct in all_structures) {
        if (struct %in% names(data)) {
          weight <- config$structure_weights[[struct]]
          if (!is.null(weight) && !is.na(weight)) {
            structure_weights <- ifelse(data[[struct]] == 1, weight, structure_weights)
          }
        }
      }
      
      structure_multipliers <- (structure_weights/100 + 1)
      
      # Calculate business values
      business_values <- ifelse(!is.na(business_areas) & business_areas > 0,
                                inflation_adjusted_base * 
                                  (business_areas ^ area_weight) * 
                                  product_weights * 
                                  structure_multipliers,
                                0)
      
      return(business_values)
    }
    
    # Get property types from data
    get_property_types <- function(data) {
      n_rows <- nrow(data)
      
      # Check for dummy variable columns
      if("property_type_Domestic" %in% names(data)) {
        types <- character(n_rows)
        for (i in 1:n_rows) {
          if (!is.na(data$property_type_Domestic[i]) && data$property_type_Domestic[i] == 1) {
            types[i] <- "domestic"
          } else if ("property_type_Commercial" %in% names(data) && 
                     !is.na(data$property_type_Commercial[i]) && 
                     data$property_type_Commercial[i] == 1) {
            types[i] <- "commercial"
          } else if ("property_type_Institutional" %in% names(data) && 
                     !is.na(data$property_type_Institutional[i]) && 
                     data$property_type_Institutional[i] == 1) {
            types[i] <- "institutional"
          } else {
            types[i] <- "domestic"  # Default
          }
        }
        return(types)
      } else if ("property_type" %in% names(data)) {
        # Original property_type column exists
        types <- tolower(as.character(data$property_type))
        types[is.na(types)] <- "domestic"
        return(types)
      } else {
        # No property type information, default all to domestic
        return(rep("domestic", n_rows))
      }
    }
    
    # Calculate property taxes using Module 3 logic
    calculate_property_taxes_module3 <- function(property_values, property_types, tax_config) {
      n <- length(property_values)
      taxes <- numeric(n)
      
      # Handle case where no tax config exists
      if (is.null(tax_config) || length(tax_config) == 0) {
        # Use default rates
        for (i in 1:n) {
          if (!is.na(property_values[i]) && property_values[i] > 0) {
            taxes[i] <- max(property_values[i] * 0.0015, 100000)  # Default: 0.15% with 100k minimum
          }
        }
        return(taxes)
      }
      
      for (i in 1:n) {
        prop_type <- property_types[i]
        prop_value <- property_values[i]
        
        if (is.na(prop_value) || prop_value <= 0) {
          taxes[i] <- 0
          next
        }
        
        type_config <- tax_config[[prop_type]]
        if (is.null(type_config)) {
          type_config <- tax_config[["domestic"]]  # Default to domestic
          if (is.null(type_config)) {
            # Final fallback
            taxes[i] <- max(prop_value * 0.0015, 100000)
            next
          }
        }
        
        if (!is.null(type_config$use_slots) && !type_config$use_slots) {
          # Simple calculation
          rate <- if(!is.null(type_config$rate)) type_config$rate else 0.15
          minimum <- if(!is.null(type_config$minimum)) type_config$minimum else 100000
          taxes[i] <- max(prop_value * (rate / 100), minimum)
        } else if (!is.null(type_config$use_slots) && type_config$use_slots) {
          # Find which slot applies
          slot_num <- 3  # Default to highest slot
          for (s in 1:3) {
            slot <- type_config$slots[[paste0("slot", s)]]
            if (!is.null(slot) && !is.null(slot$min) && !is.null(slot$max) &&
                prop_value >= slot$min && prop_value < slot$max) {
              slot_num <- s
              break
            }
          }
          
          slot_config <- type_config$slots[[paste0("slot", slot_num)]]
          if (!is.null(slot_config)) {
            rate <- if(!is.null(slot_config$rate)) slot_config$rate else 0.15
            minimum <- if(!is.null(slot_config$minimum)) slot_config$minimum else 100000
            taxes[i] <- max(prop_value * (rate / 100), minimum)
          } else {
            # Fallback
            taxes[i] <- max(prop_value * 0.0015, 100000)
          }
        } else {
          # No use_slots specified, use simple calculation
          rate <- if(!is.null(type_config$rate)) type_config$rate else 0.15
          minimum <- if(!is.null(type_config$minimum)) type_config$minimum else 100000
          taxes[i] <- max(prop_value * (rate / 100), minimum)
        }
      }
      
      return(taxes)
    }
    
    # Calculate business licenses using Module 3 logic
    calculate_business_licenses_module3 <- function(business_values, business_areas, 
                                                    business_subcategories, license_config) {
      n <- length(business_values)
      licenses <- numeric(n)
      
      # Handle case where no business license config exists
      if (is.null(license_config) || length(license_config) == 0) {
        # Use default rates for all businesses
        for (i in 1:n) {
          if (!is.na(business_values[i]) && business_values[i] > 0) {
            licenses[i] <- max(business_values[i] * 0.001, 50000)  # Default: 0.1% with 50k minimum
          }
        }
        return(licenses)
      }
      
      for (i in 1:n) {
        if (is.na(business_subcategories[i]) || is.na(business_values[i]) || business_values[i] <= 0) {
          licenses[i] <- 0
          next
        }
        
        subcat_config <- license_config[[business_subcategories[i]]]
        if (is.null(subcat_config)) {
          # Use default configuration
          licenses[i] <- max(business_values[i] * 0.001, 50000)  # Default: 0.1% with 50k minimum
          next
        }
        
        # Handle different tax types with better error checking
        if (!is.null(subcat_config$tax_type)) {
          if (subcat_config$tax_type == "minimum_rate" && !is.null(subcat_config$use_slots) && !subcat_config$use_slots) {
            rate <- if(!is.null(subcat_config$rate)) subcat_config$rate else 0.1
            minimum <- if(!is.null(subcat_config$minimum)) subcat_config$minimum else 50000
            licenses[i] <- max(business_values[i] * (rate / 100), minimum)
          } else if (subcat_config$tax_type == "flat_tax" && !is.null(subcat_config$use_slots) && !subcat_config$use_slots) {
            licenses[i] <- if(!is.null(subcat_config$flat_amount)) subcat_config$flat_amount else 50000
          } else if (!is.null(subcat_config$use_slots) && subcat_config$use_slots) {
            # Handle slots
            value_to_check <- if(!is.null(subcat_config$slot_basis) && subcat_config$slot_basis == "value") {
              business_values[i]
            } else {
              business_areas[i]
            }
            
            if (!is.na(value_to_check)) {
              slot_num <- 3
              for (s in 1:3) {
                slot <- subcat_config$slots[[paste0("slot", s)]]
                if (!is.null(slot) && !is.null(slot$min) && !is.null(slot$max) && 
                    value_to_check >= slot$min && value_to_check < slot$max) {
                  slot_num <- s
                  break
                }
              }
              
              slot_config <- subcat_config$slots[[paste0("slot", slot_num)]]
              if (!is.null(slot_config)) {
                if (!is.null(subcat_config$tax_type) && subcat_config$tax_type == "minimum_rate") {
                  rate <- if(!is.null(slot_config$rate)) slot_config$rate else 0.1
                  minimum <- if(!is.null(slot_config$minimum)) slot_config$minimum else 50000
                  licenses[i] <- max(business_values[i] * (rate / 100), minimum)
                } else {
                  licenses[i] <- if(!is.null(slot_config$flat_amount)) slot_config$flat_amount else 50000
                }
              } else {
                licenses[i] <- 50000  # Default fallback
              }
            }
          } else {
            # Default fallback
            licenses[i] <- max(business_values[i] * 0.001, 50000)
          }
        } else {
          # No tax type specified, use default
          licenses[i] <- max(business_values[i] * 0.001, 50000)
        }
      }
      
      return(licenses)
    }
    
    # Update filter choices based on data
    update_filter_choices <- function() {
      req(values$revenue_data)
      
      # Get unique values from all scenarios
      all_data <- do.call(rbind, values$revenue_data)
      
      # Structure types
      structure_types <- unique(all_data$structure_type[all_data$structure_type != "None"])
      updateSelectInput(session, "filter_structure_types",
                        choices = c("All", sort(structure_types)),
                        selected = "All")
      
      # Property types
      property_types <- sort(unique(all_data$property_type))
      updateSelectInput(session, "filter_property_types",
                        choices = c("All", property_types),
                        selected = "All")
      
      # License categories
      categories <- unique(all_data$business_category[!is.na(all_data$business_category)])
      updateSelectInput(session, "filter_license_categories",
                        choices = c("All", sort(categories)),
                        selected = "All")
      
      # License subcategories
      subcategories <- unique(all_data$business_sub_category[!is.na(all_data$business_sub_category)])
      updateSelectInput(session, "filter_license_subcategories",
                        choices = c("All", sort(subcategories)),
                        selected = "All")
    }
    
    # Value boxes for total revenue
    output$existing_total_revenue <- renderValueBox({
      if (is.null(values$revenue_data)) {
        valueBox(
          value = "Not Calculated",
          subtitle = "Existing Scenario",
          icon = icon("calculator"),
          color = "black"
        )
      } else {
        scenario_data <- values$revenue_data[["existing"]]
        total <- sum(scenario_data$total_tax, na.rm = TRUE)
        
        valueBox(
          value = format(total, big.mark = ",", scientific = FALSE),
          subtitle = "Existing Scenario Revenue",
          icon = icon("dollar-sign"),
          color = "blue"
        )
      }
    })
    
    output$scenario_a_total_revenue <- renderValueBox({
      if (is.null(values$revenue_data)) {
        valueBox(
          value = "Not Calculated",
          subtitle = "Scenario A",
          icon = icon("calculator"),
          color = "black"
        )
      } else {
        scenario_data <- values$revenue_data[["scenario_a"]]
        total <- sum(scenario_data$total_tax, na.rm = TRUE)
        
        # Calculate change from existing
        existing_total <- sum(values$revenue_data[["existing"]]$total_tax, na.rm = TRUE)
        change_pct <- round((total - existing_total) / existing_total * 100, 1)
        
        valueBox(
          value = format(total, big.mark = ",", scientific = FALSE),
          subtitle = paste0("Scenario A (", ifelse(change_pct >= 0, "+", ""), change_pct, "%)"),
          icon = icon("dollar-sign"),
          color = if(change_pct > 0) "green" else if(change_pct < 0) "red" else "blue"
        )
      }
    })
    
    output$scenario_b_total_revenue <- renderValueBox({
      if (is.null(values$revenue_data)) {
        valueBox(
          value = "Not Calculated",
          subtitle = "Scenario B",
          icon = icon("calculator"),
          color = "black"
        )
      } else {
        scenario_data <- values$revenue_data[["scenario_b"]]
        total <- sum(scenario_data$total_tax, na.rm = TRUE)
        
        # Calculate change from existing
        existing_total <- sum(values$revenue_data[["existing"]]$total_tax, na.rm = TRUE)
        change_pct <- round((total - existing_total) / existing_total * 100, 1)
        
        valueBox(
          value = format(total, big.mark = ",", scientific = FALSE),
          subtitle = paste0("Scenario B (", ifelse(change_pct >= 0, "+", ""), change_pct, "%)"),
          icon = icon("dollar-sign"),
          color = if(change_pct > 0) "green" else if(change_pct < 0) "red" else "blue"
        )
      }
    })
    
    # Chart 1A: Revenue by type (All properties)
    output$revenue_by_type_plot <- renderPlot({
      req(values$revenue_data)
      
      plot_data <- list()
      for (scenario in names(values$revenue_data)) {
        scenario_data <- values$revenue_data[[scenario]]
        
        type_summary <- data.frame(
          scenario = scenario,
          Property_Tax = sum(scenario_data$property_tax, na.rm = TRUE),
          Business_License = sum(scenario_data$business_license, na.rm = TRUE)
        )
        
        plot_data[[scenario]] <- type_summary
      }
      
      combined_data <- do.call(rbind, plot_data)
      
      combined_long <- combined_data %>%
        tidyr::pivot_longer(cols = c(Property_Tax, Business_License),
                            names_to = "Type",
                            values_to = "Revenue")
      
      ggplot(combined_long, aes(x = scenario, y = Revenue, fill = Type)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Total Revenue by Type - All Properties",
             x = "Scenario",
             y = "Total Revenue",
             fill = "Revenue Type") +
        theme_minimal() +
        theme(legend.position = "bottom")
    })
    
    # Chart 1B: Revenue by type (Compliers only)
    output$revenue_by_type_compliers_plot <- renderPlot({
      req(values$revenue_data)
      
      plot_data <- list()
      for (scenario in names(values$revenue_data)) {
        scenario_data <- values$revenue_data[[scenario]]
        # Filter to compliers only
        scenario_data <- scenario_data[scenario_data$made_payment == TRUE, ]
        
        type_summary <- data.frame(
          scenario = scenario,
          Property_Tax = sum(scenario_data$property_tax, na.rm = TRUE),
          Business_License = sum(scenario_data$business_license, na.rm = TRUE)
        )
        
        plot_data[[scenario]] <- type_summary
      }
      
      combined_data <- do.call(rbind, plot_data)
      
      combined_long <- combined_data %>%
        tidyr::pivot_longer(cols = c(Property_Tax, Business_License),
                            names_to = "Type",
                            values_to = "Revenue")
      
      ggplot(combined_long, aes(x = scenario, y = Revenue, fill = Type)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Total Revenue by Type - Filtered to Compliers",
             x = "Scenario",
             y = "Total Revenue",
             fill = "Revenue Type") +
        theme_minimal() +
        theme(legend.position = "bottom")
    })
    
    # Chart 1C: Revenue with filtering options
    output$revenue_filtered_plot <- renderPlot({
      req(values$revenue_data)
      
      plot_data <- list()
      for (scenario in names(values$revenue_data)) {
        scenario_data <- values$revenue_data[[scenario]]
        
        # Apply all filters
        # Structure type filter
        if (!is.null(input$filter_structure_types) && !"All" %in% input$filter_structure_types) {
          scenario_data <- scenario_data[scenario_data$structure_type %in% input$filter_structure_types, ]
        }
        
        # Property type filter  
        if (!is.null(input$filter_property_types) && !"All" %in% input$filter_property_types) {
          scenario_data <- scenario_data[scenario_data$property_type %in% input$filter_property_types, ]
        }
        
        # License category filter
        if (!is.null(input$filter_license_categories) && !"All" %in% input$filter_license_categories) {
          scenario_data <- scenario_data[scenario_data$business_category %in% input$filter_license_categories |
                                           is.na(scenario_data$business_category), ]
        }
        
        # License subcategory filter
        if (!is.null(input$filter_license_subcategories) && !"All" %in% input$filter_license_subcategories) {
          scenario_data <- scenario_data[scenario_data$business_sub_category %in% input$filter_license_subcategories |
                                           is.na(scenario_data$business_sub_category), ]
        }
        
        type_summary <- data.frame(
          scenario = scenario,
          Property_Tax = sum(scenario_data$property_tax, na.rm = TRUE),
          Business_License = sum(scenario_data$business_license, na.rm = TRUE)
        )
        
        plot_data[[scenario]] <- type_summary
      }
      
      combined_data <- do.call(rbind, plot_data)
      
      combined_long <- combined_data %>%
        tidyr::pivot_longer(cols = c(Property_Tax, Business_License),
                            names_to = "Type",
                            values_to = "Revenue")
      
      # Build filter text
      filter_text <- ""
      active_filters <- c()
      
      if (!is.null(input$filter_structure_types) && !"All" %in% input$filter_structure_types) {
        active_filters <- c(active_filters, paste("Structure:", length(input$filter_structure_types), "selected"))
      }
      if (!is.null(input$filter_property_types) && !"All" %in% input$filter_property_types) {
        active_filters <- c(active_filters, paste("Property:", length(input$filter_property_types), "selected"))
      }
      if (!is.null(input$filter_license_categories) && !"All" %in% input$filter_license_categories) {
        active_filters <- c(active_filters, paste("Category:", length(input$filter_license_categories), "selected"))
      }
      if (!is.null(input$filter_license_subcategories) && !"All" %in% input$filter_license_subcategories) {
        active_filters <- c(active_filters, paste("Subcategory:", length(input$filter_license_subcategories), "selected"))
      }
      
      if (length(active_filters) > 0) {
        filter_text <- paste(" - Filtered by", paste(active_filters, collapse = ", "))
      } else {
        filter_text <- " - No Filters Applied"
      }
      
      ggplot(combined_long, aes(x = scenario, y = Revenue, fill = Type)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_y_continuous(labels = scales::comma) +
        labs(title = paste0("Total Revenue by Type", filter_text),
             x = "Scenario",
             y = "Total Revenue",
             fill = "Revenue Type") +
        theme_minimal() +
        theme(legend.position = "bottom")
    })
    
    # Download handler for detailed data
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("revenue_data_", input$detailed_scenario, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        scenario_data <- values$revenue_data[[input$detailed_scenario]]
        write.csv(scenario_data, file, row.names = FALSE)
      }
    )
    
    # Detailed data table
    output$detailed_data_table <- DT::renderDataTable({
      req(values$revenue_data)
      
      scenario_data <- values$revenue_data[[input$detailed_scenario]]
      
      # Limit rows for display
      n_rows <- min(input$detailed_rows, nrow(scenario_data))
      display_data <- scenario_data[1:n_rows, ]
      
      DT::datatable(
        display_data,
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
    
    # Return revenue data for use in other modules
    return(reactive({
      values$revenue_data
    }))
  })
}