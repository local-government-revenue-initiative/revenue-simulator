# modules/module6_server.R
# Module 6: GIS Layer Revenue Filtering Server Logic

module6_server <- function(id, revenue_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      gis_layers = list(),
      filtered_data = NULL,
      ward_boundaries = NULL,
      current_filters = list(),
      map_status = "initializing",  # ADD THIS
      error_message = NULL,          # ADD THIS
      load_progress = 0,            # ADD THIS
      debug_info = list()           # ADD THIS
    )

    # Add this status message output after the reactiveValues:
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

    # Add this debug info output:
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

    # Fix the observe block for loading GIS layers - add status updates:
    observe({
      values$map_status <- "loading"  # ADD THIS
      values$error_message <- NULL     # ADD THIS
      
      # Check if revenue data exists
      if (is.null(revenue_data())) {
        values$error_message <- "No revenue data available. Please complete Modules 1-4 first."
        values$map_status <- "error"
        return()
      }    

      # Load GIS layers on initialization
      observe({
        # Load geopackage files from gis_layers directory
        gis_dir <- "gis_layers"
        
        if (dir.exists(gis_dir)) {
          gpkg_files <- list.files(gis_dir, pattern = "\\.gpkg$", full.names = TRUE)
          
          for (file_path in gpkg_files) {
            layer_name <- tools::file_path_sans_ext(basename(file_path))
            
            tryCatch({
              # Special handling for wards layer
              if (layer_name == "wards") {
                values$ward_boundaries <- sf::st_read(file_path, quiet = TRUE)
                
                # Update ward selection choices
                updateSelectizeInput(
                  session,
                  "ward_select",
                  choices = unique(values$ward_boundaries$ward_number),
                  selected = NULL
                )
              } else {
                # Load other GIS layers
                values$gis_layers[[layer_name]] <- sf::st_read(file_path, quiet = TRUE)
              }
            }, error = function(e) {
              showNotification(
                paste("Error loading", layer_name, ":", e$message),
                type = "error",
                duration = 5
              )
            })
          }
        }
      })
      
      # Function to check if property is within a GIS layer
      check_property_in_layer <- function(properties_sf, layer_sf) {
        if (is.null(layer_sf) || nrow(layer_sf) == 0) return(rep(FALSE, nrow(properties_sf)))
        
        # Perform spatial join
        within_layer <- sf::st_within(properties_sf, layer_sf, sparse = FALSE)
        
        # Return logical vector indicating which properties are within the layer
        return(apply(within_layer, 1, any))
      }
      
      # Function to determine which ward each property belongs to
      assign_property_to_ward <- function(properties_sf, wards_sf) {
        if (is.null(wards_sf) || nrow(wards_sf) == 0) return(rep(NA, nrow(properties_sf)))
        
        # Perform spatial join to find ward for each property
        ward_join <- sf::st_join(properties_sf, wards_sf, join = sf::st_within)
        
        return(ward_join$ward_number)
      }
      
      # Apply filters when button clicked
      observeEvent(input$apply_filters, {
          show_modal_spinner(spin = "circle", text = "Applying GIS filters...")
        
        req(revenue_data())
        
        values$map_status <- "loading"  # ADD THIS
        values$error_message <- NULL     # ADD THIS

        # Get the selected scenario data
        scenario_data <- revenue_data()[[input$scenario_select]]
        
        if (is.null(scenario_data)) {
          showNotification("No data available for selected scenario", type = "error")
          return()
        }
        
        # Convert properties to sf object using coordinates
        properties_sf <- scenario_data %>%
          filter(!is.na(coordinate_lat) & !is.na(coordinate_lng)) %>%
          sf::st_as_sf(coords = c("coordinate_lng", "coordinate_lat"), 
                      crs = 4326, 
                      remove = FALSE)
        
        # Initialize filter mask (all TRUE to start)
        filter_mask <- rep(TRUE, nrow(properties_sf))
        
        # Apply tourist area filters
        if (length(input$tourist_filters) > 0) {
          tourist_mask <- rep(FALSE, nrow(properties_sf))
          
          for (filter_col in input$tourist_filters) {
            if (filter_col %in% names(scenario_data)) {
              # Check if property has this attribute
              tourist_mask <- tourist_mask | (scenario_data[[filter_col]] == "Yes")
            }
          }
          
          filter_mask <- filter_mask & tourist_mask
        }
        
        # Apply commercial/industrial filters
        if (length(input$commercial_filters) > 0) {
          commercial_mask <- rep(FALSE, nrow(properties_sf))
          
          for (filter_col in input$commercial_filters) {
            if (filter_col %in% names(scenario_data)) {
              commercial_mask <- commercial_mask | (scenario_data[[filter_col]] == "Yes")
            }
          }
          
          filter_mask <- filter_mask & commercial_mask
        }
        
        # Apply other area filters
        if (length(input$other_filters) > 0) {
          other_mask <- rep(FALSE, nrow(properties_sf))
          
          for (filter_col in input$other_filters) {
            if (filter_col %in% names(scenario_data)) {
              other_mask <- other_mask | (scenario_data[[filter_col]] == "Yes")
            }
          }
          
          filter_mask <- filter_mask & other_mask
        }
        
        # Apply ward filters
        if (length(input$ward_select) > 0) {
          ward_mask <- scenario_data$ward_number %in% as.integer(input$ward_select)
          filter_mask <- filter_mask & ward_mask
        }
        
        # Apply the filter
        values$filtered_data <- scenario_data[filter_mask, ]
        
        # Store current filters for display
        values$current_filters <- list(
          tourist = input$tourist_filters,
          commercial = input$commercial_filters,
          other = input$other_filters,
          wards = input$ward_select
        )
        
        showNotification(
          paste("Filters applied. Found", nrow(values$filtered_data), "properties"),
          type = "success",
          duration = 3
        )
        remove_modal_spinner()

        values$map_status <- "ready"  # ADD THIS at the end
        
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
        } else {
          revenue_data()[[input$scenario_select]]
        }
      })
      
      # Revenue summary boxes
      output$total_revenue_box <- renderValueBox({
        data <- display_data()
        total_revenue <- sum(data$total_tax, na.rm = TRUE)
        
        valueBox(
          value = scales::dollar(total_revenue, prefix = ""),
          subtitle = "Total Revenue",
          icon = icon("coins"),
          color = "green"
        )
      })
      
      output$property_tax_box <- renderValueBox({
        data <- display_data()
        property_tax <- sum(data$property_tax, na.rm = TRUE)
        
        valueBox(
          value = scales::dollar(property_tax, prefix = ""),
          subtitle = "Property Tax",
          icon = icon("home"),
          color = "blue"
        )
      })
      
      output$business_license_box <- renderValueBox({
        data <- display_data()
        business_license <- sum(data$business_license, na.rm = TRUE)
        
        valueBox(
          value = scales::dollar(business_license, prefix = ""),
          subtitle = "Business License",
          icon = icon("building"),
          color = "yellow"
        )
      })
      
      # Property and business counts
      output$property_count <- renderText({
        data <- display_data()
        n_properties <- n_distinct(data$id_property)
        paste(scales::comma(n_properties), "properties")
      })
      
      output$business_count <- renderText({
        data <- display_data()
        n_businesses <- sum(!is.na(data$business_name))
        paste(scales::comma(n_businesses), "businesses")
      })
      
      # Interactive map
      output$revenue_map <- renderLeaflet({
        req(display_data())
        
        data <- display_data() %>%
          filter(!is.na(coordinate_lat) & !is.na(coordinate_lng))
        
        # Create base map
        map <- leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = -13.2317, lat = 8.4657, zoom = 12)  # Centered on Freetown
        
        # Add data based on aggregation level
        if (input$map_aggregation == "property") {
          # Individual property points
          
          # Select metric for coloring
          metric_col <- switch(input$map_metric,
                            "total" = "total_tax",
                            "property_tax" = "property_tax",
                            "business_license" = "business_license",
                            "density" = "total_tax")
          
          # Create color palette
          pal <- colorNumeric("YlOrRd", domain = data[[metric_col]])
          
          map <- map %>%
            addCircleMarkers(
              data = data,
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
                "<b>Property Tax:</b>", scales::dollar(property_tax), "<br>",
                "<b>Business License:</b>", scales::dollar(business_license), "<br>",
                "<b>Total Tax:</b>", scales::dollar(total_tax)
              )
            ) %>%
            addLegend(
              position = "bottomright",
              pal = pal,
              values = data[[metric_col]],
              title = gsub("_", " ", metric_col),
              labFormat = labelFormat(prefix = "$")
            )
            
        } else if (input$map_aggregation == "ward") {
          # Ward-level aggregation
          req(values$ward_boundaries)
          
          # Aggregate by ward
          ward_summary <- data %>%
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
          pal <- colorNumeric("YlOrRd", domain = ward_data[[metric_col]])
          
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
                "<b>Property Tax:</b>", scales::dollar(property_tax), "<br>",
                "<b>Business License:</b>", scales::dollar(business_license), "<br>",
                "<b>Total Revenue:</b>", scales::dollar(total_revenue)
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
          map <- map %>%
            addHeatmap(
              data = data,
              lng = ~coordinate_lng,
              lat = ~coordinate_lat,
              intensity = ~total_tax,
              blur = 20,
              radius = 15,
              gradient = "YlOrRd"
            )
        }
        
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
            
            layer_revenue <- bind_rows(layer_revenue, in_layer)
          }
        }
        
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
      })
      
      # Ward revenue plot
      output$ward_revenue_plot <- renderPlot({
        req(display_data())
        
        data <- display_data()
        
        # Aggregate by ward
        ward_summary <- data %>%
          group_by(ward_number) %>%
          summarise(
            total_revenue = sum(total_tax, na.rm = TRUE),
            property_tax = sum(property_tax, na.rm = TRUE),
            business_license = sum(business_license, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(desc(total_revenue)) %>%
          head(20)  # Top 20 wards
        
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
      })
      
      # Download handlers
      output$download_filtered_csv <- downloadHandler(
        filename = function() {
          paste0("gis_filtered_revenue_", Sys.Date(), ".csv")
        },
        content = function(file) {
          write_csv(display_data(), file)
        }
      )
      
      # Return values for use in other modules
      return(list(
        filtered_data = reactive(values$filtered_data),
        current_filters = reactive(values$current_filters)
      ))
      values$map_status <- "ready"  # ADD THIS at the end
    })
}