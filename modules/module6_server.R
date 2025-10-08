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
      error_message = NULL
    )
    
    # IMPORTANT: Map display names to actual scenario keys
    scenario_map <- reactive({
      c("Existing" = "existing",
        "Scenario A" = "scenario_a", 
        "Scenario B" = "scenario_b")
    })
    
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
              if (!is.null(values$ward_boundaries) && "ward_number" %in% names(values$ward_boundaries)) {
                updateSelectizeInput(
                  session,
                  "ward_select",
                  choices = sort(unique(values$ward_boundaries$ward_number)),
                  selected = NULL
                )
              }
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
        
        values$map_status <- "ready"
      } else {
        values$map_status <- "ready"  # Still set to ready even without GIS layers
      }
    })
    
    # Apply filters when button clicked
    observeEvent(input$apply_filters, {
      req(revenue_data())
      
      # Get the actual scenario key using the mapping
      actual_scenario <- scenario_map()[input$scenario_select]
      
      # Get the selected scenario data using the mapped key
      scenario_data <- revenue_data()[[actual_scenario]]
      
      if (is.null(scenario_data)) {
        showNotification(
          paste("No data available for", input$scenario_select),
          type = "error"
        )
        return()
      }
      
      # Initialize with full dataset for this scenario
      filtered_data <- scenario_data
      
      # Apply tourist area filters
      if (length(input$tourist_filters) > 0) {
        filter_mask <- rep(FALSE, nrow(filtered_data))
        
        for (filter_col in input$tourist_filters) {
          if (filter_col %in% names(filtered_data)) {
            filter_mask <- filter_mask | (filtered_data[[filter_col]] == "Yes")
          }
        }
        
        filtered_data <- filtered_data[filter_mask, ]
      }
      
      # Apply commercial/industrial filters  
      if (length(input$commercial_filters) > 0) {
        filter_mask <- rep(FALSE, nrow(filtered_data))
        
        for (filter_col in input$commercial_filters) {
          if (filter_col %in% names(filtered_data)) {
            filter_mask <- filter_mask | (filtered_data[[filter_col]] == "Yes")
          }
        }
        
        filtered_data <- filtered_data[filter_mask, ]
      }
      
      # Apply other area filters
      if (length(input$other_filters) > 0) {
        filter_mask <- rep(FALSE, nrow(filtered_data))
        
        for (filter_col in input$other_filters) {
          if (filter_col %in% names(filtered_data)) {
            filter_mask <- filter_mask | (filtered_data[[filter_col]] == "Yes")
          }
        }
        
        filtered_data <- filtered_data[filter_mask, ]
      }
      
      # Apply ward filters
      if (length(input$ward_select) > 0) {
        if ("ward_number" %in% names(filtered_data)) {
          filtered_data <- filtered_data[filtered_data$ward_number %in% as.integer(input$ward_select), ]
        }
      }
      
      # Store the filtered data
      values$filtered_data <- filtered_data
      
      # Store current filters for display
      values$current_filters <- list(
        tourist = input$tourist_filters,
        commercial = input$commercial_filters,
        other = input$other_filters,
        wards = input$ward_select
      )
      
      showNotification(
        paste("Filters applied. Found", nrow(filtered_data), "properties"),
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
      
      # Get the actual scenario key
      actual_scenario <- scenario_map()[input$scenario_select]
      
      # Reset to full dataset
      values$filtered_data <- revenue_data()[[actual_scenario]]
      values$current_filters <- list()
      
      showNotification("All filters cleared", type = "info", duration = 2)
    })
    
    # Get display data (either filtered or full)
    display_data <- reactive({
      if (!is.null(values$filtered_data)) {
        values$filtered_data
      } else if (!is.null(revenue_data())) {
        # Use the mapping to get the correct scenario
        actual_scenario <- scenario_map()[input$scenario_select]
        revenue_data()[[actual_scenario]]
      } else {
        NULL
      }
    })
    
    # Update scenario selection when revenue data changes
    observe({
      req(revenue_data())
      
      # Get available scenarios from the data
      available_scenarios <- names(revenue_data())
      
      # Map them to display names
      display_names <- c()
      if ("existing" %in% available_scenarios) display_names <- c(display_names, "Existing")
      if ("scenario_a" %in% available_scenarios) display_names <- c(display_names, "Scenario A")
      if ("scenario_b" %in% available_scenarios) display_names <- c(display_names, "Scenario B")
      
      # Update the select input if needed
      updateSelectInput(
        session,
        "scenario_select",
        choices = display_names,
        selected = display_names[1]
      )
    })
    
    # Revenue summary boxes
    output$total_revenue_box <- renderValueBox({
      data <- display_data()
      
      if (is.null(data) || nrow(data) == 0) {
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
      
      if (is.null(data) || nrow(data) == 0) {
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
      
      if (is.null(data) || nrow(data) == 0) {
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
    
    # Interactive map
    output$revenue_map <- renderLeaflet({
      # Check if we have data
      if (is.null(display_data()) || nrow(display_data()) == 0) {
        # Return a map with a message
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = -13.2317, lat = 8.4657, zoom = 12) %>%
          addControl(
            html = "<div style='padding: 10px; background: white; border-radius: 5px;'>
                    <b>No data available</b><br/>
                    Please complete Modules 1-4 to load revenue data.
                    </div>",
            position = "topright"
          )
      } else {
        data <- display_data() %>%
          filter(!is.na(coordinate_lat) & !is.na(coordinate_lng))
        
        if (nrow(data) == 0) {
          # No properties with coordinates
          leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -13.2317, lat = 8.4657, zoom = 12) %>%
            addControl(
              html = "<div style='padding: 10px; background: white; border-radius: 5px;'>
                      <b>No properties with coordinates</b><br/>
                      Check that your data includes valid lat/lng values.
                      </div>",
              position = "topright"
            )
        } else {
          # Create base map
          map <- leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = mean(data$coordinate_lng), 
                    lat = mean(data$coordinate_lat), 
                    zoom = 12)
          
          # Add data based on aggregation level
          if (input$map_aggregation == "property") {
            # Individual property points
            metric_col <- switch(input$map_metric,
                               "total" = "total_tax",
                               "property_tax" = "property_tax",
                               "business_license" = "business_license",
                               "density" = "total_tax")
            
            # Check if the column exists
            if (!(metric_col %in% names(data))) {
              metric_col <- "total_tax"  # Fallback
            }
            
            # Create color palette
            if (sum(data[[metric_col]], na.rm = TRUE) > 0) {
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
            } else {
              # No revenue to display
              map <- map %>%
                addCircleMarkers(
                  data = data,
                  lng = ~coordinate_lng,
                  lat = ~coordinate_lat,
                  radius = 5,
                  fillColor = "gray",
                  fillOpacity = 0.7,
                  stroke = TRUE,
                  color = "white",
                  weight = 1
                )
            }
            
          } else if (input$map_aggregation == "ward" && !is.null(values$ward_boundaries)) {
            # Ward-level aggregation
            
            # Aggregate by ward
            ward_summary <- data %>%
              filter(!is.na(ward_number)) %>%
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
            
            # Check if we have revenue data
            if (!is.null(ward_data[[metric_col]]) && sum(ward_data[[metric_col]], na.rm = TRUE) > 0) {
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
            } else {
              # No revenue data for wards
              map <- map %>%
                addPolygons(
                  data = ward_data,
                  fillColor = "gray",
                  fillOpacity = 0.5,
                  color = "white",
                  weight = 2,
                  label = ~paste("Ward", ward_number)
                )
            }
            
          } else if (input$map_aggregation == "heat") {
            # Heat map
            if (sum(data$total_tax, na.rm = TRUE) > 0) {
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
            } else {
              map <- map %>%
                addCircleMarkers(
                  data = data,
                  lng = ~coordinate_lng,
                  lat = ~coordinate_lat,
                  radius = 3,
                  fillColor = "gray",
                  fillOpacity = 0.5
                )
            }
          }
          
          map
        }
      }
    })
    
    # The rest of your outputs remain the same...
    # (layer_comparison_plot, ward_revenue_plot, download handlers, etc.)
    
    # Return values for use in other modules
    return(list(
      filtered_data = reactive(values$filtered_data),
      current_filters = reactive(values$current_filters),
      map_status = reactive(values$map_status)
    ))
  })
}