# modules/module6_server.R
# Module 6: GIS Layer Revenue Filtering Server Logic - FIXED VERSION

module6_server <- function(id, revenue_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      gis_layers = list(),
      filtered_data = NULL,
      ward_boundaries = NULL,
      current_filters = list(),
      map_status = "initializing",
      error_message = NULL,
      load_progress = 0,
      debug_info = list()
    )

    # Status message output
    output$status_message <- renderUI({
      if (!is.null(values$error_message)) {
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          strong("Error: "),
          values$error_message
        )
      } else if (values$map_status == "loading") {
        div(
          class = "alert alert-info",
          icon("spinner", class = "fa-spin"),
          strong("Loading: "),
          "Processing geographic data..."
        )
      } else if (values$map_status == "ready") {
        div(
          class = "alert alert-success",
          icon("check-circle"),
          strong("Ready: "),
          "Map loaded successfully"
        )
      } else {
        div(
          class = "alert alert-warning",
          icon("hourglass-half"),
          strong("Initializing: "),
          "Setting up map components..."
        )
      }
    })

    # Debug information panel
    output$debug_info <- renderPrint({
      cat("=== Module 6 Debug Information ===\n")
      cat("Map Status:", values$map_status, "\n")
      cat("Revenue Data Available:", !is.null(revenue_data()), "\n")
      
      if (!is.null(revenue_data())) {
        cat("Scenarios Available:", paste(names(revenue_data()), collapse = ", "), "\n")
        
        current_scenario <- revenue_data()[[input$scenario_select]]
        if (!is.null(current_scenario)) {
          cat("Current Scenario Records:", nrow(current_scenario), "\n")
          cat("Columns Available:", paste(names(current_scenario)[1:min(10, length(names(current_scenario)))], collapse = ", "), "...\n")
          
          # Check for required columns
          required_cols <- c("coordinate_lat", "coordinate_lng", "total_tax", 
                          "property_tax", "business_license")
          missing_cols <- setdiff(required_cols, names(current_scenario))
          if (length(missing_cols) > 0) {
            cat("MISSING REQUIRED COLUMNS:", paste(missing_cols, collapse = ", "), "\n")
          }
          
          # Check coordinate validity
          if (all(c("coordinate_lat", "coordinate_lng") %in% names(current_scenario))) {
            valid_coords <- sum(!is.na(current_scenario$coordinate_lat) & 
                              !is.na(current_scenario$coordinate_lng))
            cat("Valid Coordinates:", valid_coords, "out of", nrow(current_scenario), "\n")
          }
        }
      }
      
      cat("\nGIS Layers Loaded:", length(values$gis_layers), "\n")
      if (length(values$gis_layers) > 0) {
        cat("Layer Names:", paste(names(values$gis_layers), collapse = ", "), "\n")
      }
      
      cat("Ward Boundaries Loaded:", !is.null(values$ward_boundaries), "\n")
      if (!is.null(values$ward_boundaries)) {
        cat("Number of Wards:", nrow(values$ward_boundaries), "\n")
      }
      
      cat("\nFiltered Data:", 
          ifelse(is.null(values$filtered_data), "None", 
                paste(nrow(values$filtered_data), "records")), "\n")
      
      if (!is.null(values$error_message)) {
        cat("\n!!! ERROR !!!\n", values$error_message, "\n")
      }
    })

    # FIRST OBSERVE BLOCK: Load GIS layers on initialization
    observe({
      values$map_status <- "loading"
      values$error_message <- NULL
      
      # Check if revenue data exists
      if (is.null(revenue_data())) {
        values$error_message <- "No revenue data available. Please complete Modules 1-4 first."
        values$map_status <- "error"
        return()
      }
      
      # Load geopackage files from gis_layers directory
      gis_dir <- "gis_layers"
      
      if (!dir.exists(gis_dir)) {
        showNotification(
          paste("GIS directory not found:", gis_dir, 
                "\nCreating directory. Please add .gpkg files to", gis_dir),
          type = "warning",
          duration = 10
        )
        dir.create(gis_dir, showWarnings = FALSE)
        values$map_status <- "ready"
        return()
      }
      
      gpkg_files <- list.files(gis_dir, pattern = "\\.gpkg$", full.names = TRUE)
      
      if (length(gpkg_files) == 0) {
        showNotification(
          paste("No .gpkg files found in", gis_dir, 
                "\nMap will work with property data columns only."),
          type = "info",
          duration = 8
        )
        values$map_status <- "ready"
        return()
      }
      
      for (file_path in gpkg_files) {
        layer_name <- tools::file_path_sans_ext(basename(file_path))
        
        tryCatch({
          # Special handling for wards layer
          if (layer_name == "wards") {
            values$ward_boundaries <- sf::st_read(file_path, quiet = TRUE)
            
            # Update ward selection choices
            if ("ward_number" %in% names(values$ward_boundaries)) {
              updateSelectizeInput(
                session,
                "ward_select",
                choices = sort(unique(values$ward_boundaries$ward_number)),
                selected = NULL
              )
            } else {
              showNotification(
                "Ward file loaded but 'ward_number' column not found",
                type = "warning",
                duration = 5
              )
            }
          } else {
            # Load other GIS layers
            values$gis_layers[[layer_name]] <- sf::st_read(file_path, quiet = TRUE)
          }
          
          showNotification(
            paste("Loaded GIS layer:", layer_name),
            type = "message",
            duration = 2
          )
          
        }, error = function(e) {
          showNotification(
            paste("Error loading", layer_name, ":", e$message),
            type = "error",
            duration = 5
          )
        })
      }
      
      values$map_status <- "ready"
    })  # END OF FIRST OBSERVE BLOCK
    
    # SECOND OBSERVE BLOCK: Initialize map when data is available
    observe({
      req(revenue_data())
      
      # Get data from selected scenario
      scenario_data <- revenue_data()[[input$scenario_select]]
      
      if (is.null(scenario_data)) {
        return()
      }
      
      # Check for required columns
      required_cols <- c("coordinate_lat", "coordinate_lng")
      if (!all(required_cols %in% names(scenario_data))) {
        values$error_message <- paste("Missing required columns:", 
                                    paste(setdiff(required_cols, names(scenario_data)), 
                                          collapse = ", "))
        return()
      }
      
      # If no filtered data exists, use full scenario data
      if (is.null(values$filtered_data)) {
        values$filtered_data <- scenario_data
      }
    })  # END OF SECOND OBSERVE BLOCK
    
    # Helper function to check if property is within a GIS layer
    check_property_in_layer <- function(properties_sf, layer_sf) {
      if (is.null(layer_sf) || nrow(layer_sf) == 0) return(rep(FALSE, nrow(properties_sf)))
      
      # Perform spatial join
      within_layer <- sf::st_within(properties_sf, layer_sf, sparse = FALSE)
      
      # Return logical vector indicating which properties are within the layer
      return(apply(within_layer, 1, any))
    }
    
    # Helper function to determine which ward each property belongs to
    assign_property_to_ward <- function(properties_sf, wards_sf) {
      if (is.null(wards_sf) || nrow(wards_sf) == 0) return(rep(NA, nrow(properties_sf)))
      
      # Perform spatial join to find ward for each property
      ward_join <- sf::st_join(properties_sf, wards_sf, join = sf::st_within)
      
      return(ward_join$ward_number)
    }
    
    # Apply filters when button clicked
    observeEvent(input$apply_filters, {
      req(revenue_data())
      
      values$map_status <- "loading"
      values$error_message <- NULL
      
      # Get the selected scenario data
      scenario_data <- revenue_data()[[input$scenario_select]]
      
      if (is.null(scenario_data)) {
        values$error_message <- "No data available for selected scenario"
        values$map_status <- "error"
        showNotification("No data available for selected scenario", type = "error")
        return()
      }
      
      # Start with all data
      filter_mask <- rep(TRUE, nrow(scenario_data))
      filters_applied <- 0
      
      # Apply tourist area filters
      if (length(input$tourist_filters) > 0) {
        tourist_mask <- rep(FALSE, nrow(scenario_data))
        
        for (filter_col in input$tourist_filters) {
          if (filter_col %in% names(scenario_data)) {
            tourist_mask <- tourist_mask | (scenario_data[[filter_col]] == "Yes")
          }
        }
        
        filter_mask <- filter_mask & tourist_mask
        filters_applied <- filters_applied + 1
      }
      
      # Apply commercial/industrial filters
      if (length(input$commercial_filters) > 0) {
        commercial_mask <- rep(FALSE, nrow(scenario_data))
        
        for (filter_col in input$commercial_filters) {
          if (filter_col %in% names(scenario_data)) {
            commercial_mask <- commercial_mask | (scenario_data[[filter_col]] == "Yes")
          }
        }
        
        filter_mask <- filter_mask & commercial_mask
        filters_applied <- filters_applied + 1
      }
      
      # Apply other area filters
      if (length(input$other_filters) > 0) {
        other_mask <- rep(FALSE, nrow(scenario_data))
        
        for (filter_col in input$other_filters) {
          if (filter_col %in% names(scenario_data)) {
            other_mask <- other_mask | (scenario_data[[filter_col]] == "Yes")
          }
        }
        
        filter_mask <- filter_mask & other_mask
        filters_applied <- filters_applied + 1
      }
      
      # Apply ward filters
      if (length(input$ward_select) > 0) {
        if ("ward_number" %in% names(scenario_data)) {
          ward_mask <- scenario_data$ward_number %in% as.integer(input$ward_select)
          filter_mask <- filter_mask & ward_mask
          filters_applied <- filters_applied + 1
        }
      }
      
      # Apply the filter
      if (filters_applied > 0) {
        values$filtered_data <- scenario_data[filter_mask, ]
      } else {
        values$filtered_data <- scenario_data
      }
      
      # Store current filters for display
      values$current_filters <- list(
        tourist = input$tourist_filters,
        commercial = input$commercial_filters,
        other = input$other_filters,
        wards = input$ward_select
      )
      
      values$map_status <- "ready"
      
      showNotification(
        paste("Filters applied. Found", nrow(values$filtered_data), "properties"),
        type = "success",
        duration = 3
      )
    })
    
    # Clear all filters
    observeEvent(input$clear_filters, {
      updateCheckboxGroupInput(session, "tourist_filters", selected = character(0))
      updateCheckboxGroupInput(session, "commercial_filters", selected = character(0))
      updateCheckboxGroupInput(session, "other_filters", selected = character(0))
      updateSelectizeInput(session, "ward_select", selected = character(0))
      
      # Reset to full dataset
      values$filtered_data <- revenue_data()[[input$scenario_select]]
      values$current_filters <- list()
      
      showNotification("All filters cleared", type = "info", duration = 2)
    })
    
    # Get display data (either filtered or full)
    display_data <- reactive({
      if (!is.null(values$filtered_data)) {
        values$filtered_data
      } else if (!is.null(revenue_data())) {
        revenue_data()[[input$scenario_select]]
      } else {
        NULL
      }
    })
    
    # Revenue summary boxes
    output$total_revenue_box <- renderValueBox({
      data <- display_data()
      
      if (is.null(data)) {
        valueBox(
          value = "N/A",
          subtitle = "Total Revenue",
          icon = icon("coins"),
          color = "gray"
        )
      } else {
        total_revenue <- sum(data$total_tax, na.rm = TRUE)
        
        valueBox(
          value = scales::dollar(total_revenue, prefix = ""),
          subtitle = "Total Revenue",
          icon = icon("coins"),
          color = "green"
        )
      }
    })
    
    output$property_tax_box <- renderValueBox({
      data <- display_data()
      
      if (is.null(data)) {
        valueBox(
          value = "N/A",
          subtitle = "Property Tax",
          icon = icon("home"),
          color = "gray"
        )
      } else {
        property_tax <- sum(data$property_tax, na.rm = TRUE)
        
        valueBox(
          value = scales::dollar(property_tax, prefix = ""),
          subtitle = "Property Tax",
          icon = icon("home"),
          color = "blue"
        )
      }
    })
    
    output$business_license_box <- renderValueBox({
      data <- display_data()
      
      if (is.null(data)) {
        valueBox(
          value = "N/A",
          subtitle = "Business License",
          icon = icon("building"),
          color = "gray"
        )
      } else {
        business_license <- sum(data$business_license, na.rm = TRUE)
        
        valueBox(
          value = scales::dollar(business_license, prefix = ""),
          subtitle = "Business License",
          icon = icon("building"),
          color = "yellow"
        )
      }
    })
    
    # Property and business counts
    output$property_count <- renderText({
      data <- display_data()
      if (is.null(data)) {
        "No data available"
      } else {
        n_properties <- n_distinct(data$id_property)
        paste(scales::comma(n_properties), "properties")
      }
    })
    
    output$business_count <- renderText({
      data <- display_data()
      if (is.null(data)) {
        "No data available"
      } else {
        n_businesses <- sum(!is.na(data$business_name))
        paste(scales::comma(n_businesses), "businesses")
      }
    })
    
    # Interactive map with error handling
    output$revenue_map <- renderLeaflet({
      
      # Create base map first
      map <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -13.2317, lat = 8.4657, zoom = 12)  # Centered on Freetown
      
      # Check if we have data
      if (is.null(display_data())) {
        map <- map %>%
          addControl(
            html = "<div style='padding: 10px; background: white; border-radius: 5px;'>
                    <h4>No Data Available</h4>
                    <p>Please complete Modules 1-4 to load revenue data.</p>
                    </div>",
            position = "topright"
          )
        return(map)
      }
      
      data <- display_data()
      
      # Filter for valid coordinates
      data_valid <- data %>%
        filter(!is.na(coordinate_lat) & !is.na(coordinate_lng))
      
      if (nrow(data_valid) == 0) {
        map <- map %>%
          addControl(
            html = "<div style='padding: 10px; background: white; border-radius: 5px;'>
                    <h4>No Valid Coordinates</h4>
                    <p>The data doesn't contain valid geographic coordinates.</p>
                    </div>",
            position = "topright"
          )
        return(map)
      }
      
      # Add status indicator
      map <- map %>%
        addControl(
          html = paste0("<div style='padding: 5px; background: white; border-radius: 3px;'>
                        <small>Showing ", nrow(data_valid), " properties</small>
                        </div>"),
          position = "topleft"
        )
      
      # Add data based on aggregation level
      tryCatch({
        if (input$map_aggregation == "property") {
          # Individual property points
          
          # Select metric for coloring
          metric_col <- switch(input$map_metric,
                             "total" = "total_tax",
                             "property_tax" = "property_tax",
                             "business_license" = "business_license",
                             "density" = "total_tax")
          
          # Check if metric column exists
          if (!metric_col %in% names(data_valid)) {
            metric_col <- "total_tax"
            if (!metric_col %in% names(data_valid)) {
              # Create a dummy column if nothing exists
              data_valid$total_tax <- 1
            }
          }
          
          # Create color palette
          pal <- colorNumeric("YlOrRd", domain = data_valid[[metric_col]], na.color = "gray")
          
          map <- map %>%
            addCircleMarkers(
              data = data_valid,
              lng = ~coordinate_lng,
              lat = ~coordinate_lat,
              radius = 5,
              fillColor = ~pal(get(metric_col)),
              fillOpacity = 0.7,
              stroke = TRUE,
              color = "white",
              weight = 1,
              popup = ~paste(
                "<b>Property ID:</b>", id_property, "<br>",
                "<b>Property Tax:</b>", scales::dollar(property_tax, prefix = ""), "<br>",
                "<b>Business License:</b>", scales::dollar(business_license, prefix = ""), "<br>",
                "<b>Total Tax:</b>", scales::dollar(total_tax, prefix = "")
              )
            ) %>%
            addLegend(
              position = "bottomright",
              pal = pal,
              values = data_valid[[metric_col]],
              title = gsub("_", " ", metric_col),
              labFormat = labelFormat(prefix = "$")
            )
            
        } else if (input$map_aggregation == "ward" && !is.null(values$ward_boundaries)) {
          # Ward-level aggregation
          
          # Check if ward_number exists in data
          if (!"ward_number" %in% names(data_valid)) {
            map <- map %>%
              addControl(
                html = "<div style='padding: 10px; background: white; border-radius: 5px;'>
                        <h4>Ward Data Not Available</h4>
                        <p>The property data doesn't contain ward information.</p>
                        </div>",
                position = "topright"
              )
            return(map)
          }
          
          # Aggregate by ward
          ward_summary <- data_valid %>%
            group_by(ward_number) %>%
            summarise(
              total_revenue = sum(total_tax, na.rm = TRUE),
              property_tax = sum(property_tax, na.rm = TRUE),
              business_license = sum(business_license, na.rm = TRUE),
              n_properties = n(),
              .groups = "drop"
            )
          
          # Join with ward boundaries
          ward_data <- values$ward_boundaries %>%
            left_join(ward_summary, by = "ward_number")
          
          # Select metric for coloring
          metric_col <- switch(input$map_metric,
                             "total" = "total_revenue",
                             "property_tax" = "property_tax",
                             "business_license" = "business_license",
                             "density" = "total_revenue")
          
          # Create color palette
          pal <- colorNumeric("YlOrRd", domain = ward_data[[metric_col]], na.color = "lightgray")
          
          map <- map %>%
            addPolygons(
              data = ward_data,
              fillColor = ~pal(get(metric_col)),
              fillOpacity = 0.7,
              color = "white",
              weight = 2,
              highlightOptions = highlightOptions(
                weight = 4,
                color = "#666",
                fillOpacity = 0.8,
                bringToFront = TRUE
              ),
              label = ~paste("Ward", ward_number),
              popup = ~paste(
                "<b>Ward:</b>", ward_number, "<br>",
                "<b>Properties:</b>", scales::comma(n_properties), "<br>",
                "<b>Property Tax:</b>", scales::dollar(property_tax, prefix = ""), "<br>",
                "<b>Business License:</b>", scales::dollar(business_license, prefix = ""), "<br>",
                "<b>Total Revenue:</b>", scales::dollar(total_revenue, prefix = "")
              )
            ) %>%
            addLegend(
              position = "bottomright",
              pal = pal,
              values = ward_data[[metric_col]],
              title = gsub("_", " ", metric_col),
              labFormat = labelFormat(prefix = "$")
            )
            
        } else if (input$map_aggregation == "heat") {
          # Heat map
          
          # Check if we have enough data for heatmap
          if (nrow(data_valid) < 10) {
            map <- map %>%
              addControl(
                html = "<div style='padding: 10px; background: white; border-radius: 5px;'>
                        <h4>Insufficient Data for Heat Map</h4>
                        <p>Need at least 10 properties. Try 'Individual Properties' view.</p>
                        </div>",
                position = "topright"
              )
          } else {
            # Use leaflet.extras for heatmap if available
            if (requireNamespace("leaflet.extras", quietly = TRUE)) {
              map <- map %>%
                leaflet.extras::addHeatmap(
                  data = data_valid,
                  lng = ~coordinate_lng,
                  lat = ~coordinate_lat,
                  intensity = ~total_tax,
                  blur = 20,
                  radius = 15
                )
            } else {
              # Fallback to circle markers with transparency
              map <- map %>%
                addCircleMarkers(
                  data = data_valid,
                  lng = ~coordinate_lng,
                  lat = ~coordinate_lat,
                  radius = 10,
                  fillColor = "red",
                  fillOpacity = 0.2,
                  stroke = FALSE
                )
            }
          }
        } else {
          # Default to property view if ward boundaries not available
          map <- map %>%
            addCircleMarkers(
              data = data_valid,
              lng = ~coordinate_lng,
              lat = ~coordinate_lat,
              radius = 5,
              fillColor = "blue",
              fillOpacity = 0.5,
              stroke = TRUE,
              color = "white",
              weight = 1
            )
        }
        
      }, error = function(e) {
        map <- map %>%
          addControl(
            html = paste0("<div style='padding: 10px; background: white; border-radius: 5px;'>
                          <h4>Map Error</h4>
                          <p>", e$message, "</p>
                          </div>"),
            position = "topright"
          )
      })
      
      map
    })
    
    # Layer comparison plot
    output$layer_comparison_plot <- renderPlot({
      req(display_data())
      
      # Calculate revenue by each GIS attribute
      data <- display_data()
      
      # List of all GIS columns to analyze
      gis_columns <- c(
        "aberdeen_lumley_tourist", "juba_levuma_tourist",
        "buffered_commercial_corridors", "cbd",
        "dock_industrial", "kissy_industrial_area",
        "kissy_texaco_terminal_area", "wellington_industrial_estate",
        "hazardous_zones", "informal_settlements"
      )
      
      # Calculate revenue for each layer
      layer_revenue <- tibble()
      
      for (col in gis_columns) {
        if (col %in% names(data)) {
          in_layer <- data %>%
            filter(get(col) == "Yes") %>%
            summarise(
              layer = col,
              total_revenue = sum(total_tax, na.rm = TRUE),
              property_tax = sum(property_tax, na.rm = TRUE),
              business_license = sum(business_license, na.rm = TRUE),
              n_properties = n()
            )
          
          if (nrow(in_layer) > 0) {
            layer_revenue <- bind_rows(layer_revenue, in_layer)
          }
        }
      }
      
      if (nrow(layer_revenue) == 0) {
        # No GIS columns found - create empty plot
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                  label = "No GIS layer data available\nCheck that GIS columns exist in your data",
                  size = 6, hjust = 0.5) +
          theme_void()
      } else {
        # Clean layer names for display
        layer_revenue <- layer_revenue %>%
          mutate(
            layer_display = str_replace_all(layer, "_", " ") %>%
              str_to_title()
          )
        
        # Create comparison plot
        layer_revenue %>%
          pivot_longer(
            cols = c(property_tax, business_license),
            names_to = "type",
            values_to = "revenue"
          ) %>%
          ggplot(aes(x = reorder(layer_display, total_revenue), y = revenue, fill = type)) +
          geom_col() +
          coord_flip() +
          scale_fill_manual(
            values = c("property_tax" = "#e74c3c", "business_license" = "#3498db"),
            labels = c("property_tax" = "Property Tax", "business_license" = "Business License")
          ) +
          scale_y_continuous(labels = scales::dollar_format(prefix = "")) +
          labs(
            title = "Revenue by Geographic Layer",
            x = NULL,
            y = "Total Revenue",
            fill = "Revenue Type"
          ) +
          theme_minimal() +
          theme(legend.position = "bottom")
      }
    })
    
    # Ward revenue plot
    output$ward_revenue_plot <- renderPlot({
      data <- display_data()
      
      if (is.null(data) || !"ward_number" %in% names(data)) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                  label = "Ward data not available",
                  size = 6, hjust = 0.5) +
          theme_void()
      } else {
        # Aggregate by ward
        ward_summary <- data %>%
          filter(!is.na(ward_number)) %>%
          group_by(ward_number) %>%
          summarise(
            total_revenue = sum(total_tax, na.rm = TRUE),
            property_tax = sum(property_tax, na.rm = TRUE),
            business_license = sum(business_license, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(desc(total_revenue)) %>%
          head(20)  # Top 20 wards
        
        if (nrow(ward_summary) == 0) {
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                    label = "No ward revenue data to display",
                    size = 6, hjust = 0.5) +
            theme_void()
        } else {
          # Create plot
          ward_summary %>%
            pivot_longer(
              cols = c(property_tax, business_license),
              names_to = "type",
              values_to = "revenue"
            ) %>%
            ggplot(aes(x = reorder(factor(ward_number), total_revenue), y = revenue, fill = type)) +
            geom_col() +
            coord_flip() +
            scale_fill_manual(
              values = c("property_tax" = "#e74c3c", "business_license" = "#3498db"),
              labels = c("property_tax" = "Property Tax", "business_license" = "Business License")
            ) +
            scale_y_continuous(labels = scales::dollar_format(prefix = "")) +
            labs(
              title = "Top 20 Wards by Revenue",
              x = "Ward Number",
              y = "Total Revenue",
              fill = "Revenue Type"
            ) +
            theme_minimal() +
            theme(legend.position = "bottom")
        }
      }
    })
    
    # Download handlers
    output$download_filtered_csv <- downloadHandler(
      filename = function() {
        paste0("gis_filtered_revenue_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data <- display_data()
        if (!is.null(data)) {
          write_csv(data, file)
        }
      }
    )
    
    # Return values for use in other modules
    return(list(
      filtered_data = reactive(values$filtered_data),
      current_filters = reactive(values$current_filters),
      map_status = reactive(values$map_status),
      error_message = reactive(values$error_message)
    ))
  })
}