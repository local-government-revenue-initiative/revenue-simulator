# R/module6_functions.R
# Module 6: GIS Layer Processing Functions

# Function to load all GIS layers from a directory
load_gis_layers <- function(gis_directory = "gis_layers") {
  layers <- list()
  
  if (!dir.exists(gis_directory)) {
    warning(paste("GIS directory not found:", gis_directory))
    return(layers)
  }
  
  # Find all geopackage files
  gpkg_files <- list.files(gis_directory, 
                          pattern = "\\.gpkg$", 
                          full.names = TRUE)
  
  for (file_path in gpkg_files) {
    layer_name <- tools::file_path_sans_ext(basename(file_path))
    
    tryCatch({
      # Read the geopackage
      layer_data <- st_read(file_path, quiet = TRUE)
      
      # Ensure CRS is WGS84 for consistency
      layer_data <- st_transform(layer_data, crs = 4326)
      
      layers[[layer_name]] <- layer_data
      
      message(paste("Loaded GIS layer:", layer_name))
      
    }, error = function(e) {
      warning(paste("Error loading", layer_name, ":", e$message))
    })
  }
  
  return(layers)
}

# Function to perform spatial join between properties and GIS layers
spatial_join_properties <- function(property_data, gis_layers) {
  
  # Convert property data to sf object
  if (!inherits(property_data, "sf")) {
    property_sf <- property_data %>%
      filter(!is.na(coordinate_lat) & !is.na(coordinate_lng)) %>%
      st_as_sf(coords = c("coordinate_lng", "coordinate_lat"), 
               crs = 4326, 
               remove = FALSE)
  } else {
    property_sf <- property_data
  }
  
  # Initialize results with original data
  result <- property_sf
  
  # Process each GIS layer
  for (layer_name in names(gis_layers)) {
    layer_data <- gis_layers[[layer_name]]
    
    if (layer_name == "wards") {
      # Special handling for wards - join and get ward attributes
      ward_join <- st_join(property_sf, layer_data, join = st_within)
      
      # Add ward columns to result
      if ("ward_number" %in% names(ward_join)) {
        result[[paste0("gis_ward")]] <- ward_join$ward_number
      }
      if ("ward_name" %in% names(ward_join)) {
        result[[paste0("gis_ward_name")]] <- ward_join$ward_name
      }
      
    } else {
      # For other layers, create binary indicator
      within_layer <- st_within(property_sf, layer_data, sparse = FALSE)
      result[[paste0("gis_", layer_name)]] <- apply(within_layer, 1, any)
    }
  }
  
  return(result)
}

# Function to calculate revenue statistics by GIS layer
calculate_gis_revenue_stats <- function(data, gis_column) {
  
  if (!(gis_column %in% names(data))) {
    return(NULL)
  }
  
  stats <- data %>%
    group_by(!!sym(gis_column)) %>%
    summarise(
      n_properties = n(),
      total_revenue = sum(total_tax, na.rm = TRUE),
      property_tax = sum(property_tax, na.rm = TRUE),
      business_license = sum(business_license, na.rm = TRUE),
      avg_property_value = mean(property_value, na.rm = TRUE),
      avg_business_value = mean(business_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pct_of_total_revenue = total_revenue / sum(total_revenue) * 100,
      revenue_per_property = total_revenue / n_properties
    )
  
  return(stats)
}

# Function to create ward-level aggregations
aggregate_by_ward <- function(data, ward_boundaries = NULL) {
  
  # Aggregate revenue by ward
  ward_stats <- data %>%
    filter(!is.na(ward_number)) %>%
    group_by(ward_number) %>%
    summarise(
      n_properties = n(),
      n_businesses = sum(!is.na(business_name)),
      total_revenue = sum(total_tax, na.rm = TRUE),
      property_tax = sum(property_tax, na.rm = TRUE),
      business_license = sum(business_license, na.rm = TRUE),
      avg_property_value = mean(property_value, na.rm = TRUE),
      median_property_value = median(property_value, na.rm = TRUE),
      total_property_area = sum(property_area, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      revenue_per_property = total_revenue / n_properties,
      revenue_density = total_revenue / total_property_area
    )
  
  # If ward boundaries provided, join with spatial data
  if (!is.null(ward_boundaries)) {
    ward_stats <- ward_boundaries %>%
      left_join(ward_stats, by = "ward_number") %>%
      mutate(across(where(is.numeric), ~replace_na(., 0)))
  }
  
  return(ward_stats)
}

# Function to filter properties by multiple GIS layers
filter_by_gis_layers <- function(data, layer_filters) {
  
  filtered_data <- data
  
  # Apply each filter
  for (layer_name in names(layer_filters)) {
    filter_values <- layer_filters[[layer_name]]
    
    if (!is.null(filter_values) && length(filter_values) > 0) {
      col_name <- paste0("gis_", layer_name)
      
      if (col_name %in% names(filtered_data)) {
        if (is.logical(filtered_data[[col_name]])) {
          # For boolean columns
          filtered_data <- filtered_data %>%
            filter(!!sym(col_name) == TRUE)
        } else {
          # For categorical columns (like ward numbers)
          filtered_data <- filtered_data %>%
            filter(!!sym(col_name) %in% filter_values)
        }
      }
    }
  }
  
  return(filtered_data)
}

# Function to create revenue heat map data
create_heatmap_data <- function(data, resolution = 100) {
  
  # Get bounds
  lat_range <- range(data$coordinate_lat, na.rm = TRUE)
  lng_range <- range(data$coordinate_lng, na.rm = TRUE)
  
  # Create grid
  lat_breaks <- seq(lat_range[1], lat_range[2], length.out = resolution)
  lng_breaks <- seq(lng_range[1], lng_range[2], length.out = resolution)
  
  # Assign properties to grid cells
  data_grid <- data %>%
    mutate(
      lat_bin = cut(coordinate_lat, breaks = lat_breaks, include.lowest = TRUE),
      lng_bin = cut(coordinate_lng, breaks = lng_breaks, include.lowest = TRUE)
    ) %>%
    group_by(lat_bin, lng_bin) %>%
    summarise(
      total_revenue = sum(total_tax, na.rm = TRUE),
      n_properties = n(),
      avg_lat = mean(coordinate_lat, na.rm = TRUE),
      avg_lng = mean(coordinate_lng, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(total_revenue > 0)
  
  return(data_grid)
}

# Function to compare revenue across different GIS layers
compare_gis_layers <- function(data, gis_columns) {
  
  comparison_results <- tibble()
  
  for (col in gis_columns) {
    if (col %in% names(data)) {
      
      if (is.logical(data[[col]]) || data[[col]] %in% c("Yes", "No")) {
        # Binary columns
        layer_stats <- data %>%
          filter(!!sym(col) == TRUE | !!sym(col) == "Yes") %>%
          summarise(
            layer = col,
            n_properties = n(),
            total_revenue = sum(total_tax, na.rm = TRUE),
            property_tax = sum(property_tax, na.rm = TRUE),
            business_license = sum(business_license, na.rm = TRUE),
            avg_property_value = mean(property_value, na.rm = TRUE)
          )
      } else {
        # Categorical columns (like wards)
        layer_stats <- data %>%
          filter(!is.na(!!sym(col))) %>%
          summarise(
            layer = col,
            n_properties = n(),
            total_revenue = sum(total_tax, na.rm = TRUE),
            property_tax = sum(property_tax, na.rm = TRUE),
            business_license = sum(business_license, na.rm = TRUE),
            avg_property_value = mean(property_value, na.rm = TRUE)
          )
      }
      
      comparison_results <- bind_rows(comparison_results, layer_stats)
    }
  }
  
  # Calculate percentages
  total_revenue_all <- sum(data$total_tax, na.rm = TRUE)
  comparison_results <- comparison_results %>%
    mutate(
      pct_of_total = total_revenue / total_revenue_all * 100,
      revenue_per_property = total_revenue / n_properties
    ) %>%
    arrange(desc(total_revenue))
  
  return(comparison_results)
}

# Function to export filtered data with GIS attributes
export_gis_filtered_data <- function(data, file_path, format = "csv") {
  
  if (format == "csv") {
    # For CSV, drop geometry if present
    if (inherits(data, "sf")) {
      data <- st_drop_geometry(data)
    }
    write_csv(data, file_path)
    
  } else if (format == "geojson") {
    # For GeoJSON, ensure it's an sf object
    if (!inherits(data, "sf")) {
      data <- st_as_sf(data, 
                       coords = c("coordinate_lng", "coordinate_lat"), 
                       crs = 4326)
    }
    st_write(data, file_path, driver = "GeoJSON", delete_dsn = TRUE)
    
  } else if (format == "shapefile") {
    # For Shapefile
    if (!inherits(data, "sf")) {
      data <- st_as_sf(data, 
                       coords = c("coordinate_lng", "coordinate_lat"), 
                       crs = 4326)
    }
    st_write(data, file_path, delete_dsn = TRUE)
  }
  
  return(invisible(TRUE))
}

# Function to calculate overlap statistics between layers
calculate_layer_overlap <- function(data, layer1, layer2) {
  
  if (!(layer1 %in% names(data)) || !(layer2 %in% names(data))) {
    return(NULL)
  }
  
  overlap_stats <- data %>%
    group_by(!!sym(layer1), !!sym(layer2)) %>%
    summarise(
      n_properties = n(),
      total_revenue = sum(total_tax, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      combination = paste(layer1, "=", !!sym(layer1), 
                         "&", layer2, "=", !!sym(layer2))
    )
  
  return(overlap_stats)
}

# Function to generate summary report for filtered areas
generate_gis_summary_report <- function(data, filters_applied) {
  
  report <- list()
  
  # Overall statistics
  report$total_properties <- nrow(data)
  report$total_revenue <- sum(data$total_tax, na.rm = TRUE)
  report$property_tax_total <- sum(data$property_tax, na.rm = TRUE)
  report$business_license_total <- sum(data$business_license, na.rm = TRUE)
  
  # Statistics by property type
  report$by_property_type <- data %>%
    group_by(property_type) %>%
    summarise(
      n = n(),
      total_revenue = sum(total_tax, na.rm = TRUE),
      avg_revenue = mean(total_tax, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Top contributing properties
  report$top_properties <- data %>%
    arrange(desc(total_tax)) %>%
    select(id_property, property_type, total_tax, property_tax, business_license) %>%
    head(20)
  
  # Filters applied
  report$filters <- filters_applied
  
  # Spatial distribution
  if (all(c("coordinate_lat", "coordinate_lng") %in% names(data))) {
    report$spatial_bounds <- list(
      lat_min = min(data$coordinate_lat, na.rm = TRUE),
      lat_max = max(data$coordinate_lat, na.rm = TRUE),
      lng_min = min(data$coordinate_lng, na.rm = TRUE),
      lng_max = max(data$coordinate_lng, na.rm = TRUE)
    )
  }
  
  return(report)
}