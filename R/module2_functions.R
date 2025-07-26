# R/module2_functions.R

# Function to extract unique property types from processed data
get_property_types <- function(processed_data) {
  # Look for columns that start with "property_type_"
  property_type_cols <- grep("^property_type_", names(processed_data), value = TRUE)
  
  # Extract the property type names (remove prefix)
  property_types <- gsub("^property_type_", "", property_type_cols)
  
  # If no dummy columns found, try to get from original column
  if (length(property_types) == 0 && "property_type" %in% names(processed_data)) {
    property_types <- unique(processed_data$property_type)
    property_types <- property_types[!is.na(property_types)]
  }
  
  return(property_types)
}

# Function to extract unique business categories
get_business_categories <- function(processed_data) {
  if ("business_category" %in% names(processed_data)) {
    categories <- unique(processed_data$business_category)
    categories <- categories[!is.na(categories)]
    return(categories)
  }
  return(character(0))
}

# Function to get property features for weighting
get_property_features <- function(processed_data) {
  # List of features that should have weights
  feature_patterns <- c(
    "^wall_material_", "^roof_material_", "^window_material_",
    "^wall_condition_", "^roof_condition_", 
    "^street_access_", "^drainage_", "^street_quality_",
    "^has_", "^aid_conditioning_", "^tourist_area_",
    "^environmental_hazard_", "^main_road_high_visibility_",
    "^informal_settlement_", "^commercial_corridor_"
  )
  
  all_features <- character()
  for (pattern in feature_patterns) {
    matching_cols <- grep(pattern, names(processed_data), value = TRUE)
    all_features <- c(all_features, matching_cols)
  }
  
  # Also include numeric features
  numeric_features <- c("property_area", "coordinate_lat", "coordinate_lng")
  numeric_features <- numeric_features[numeric_features %in% names(processed_data)]
  
  all_features <- c(all_features, numeric_features)
  
  return(unique(all_features))
}

# Function to create default configuration
create_default_config <- function(processed_data) {
  config <- list(
    inflation_adjustment = 0,
    property_types = list(),
    property_features = list(),
    business_categories = list()
  )
  
  # Set default property type configs
  property_types <- get_property_types(processed_data)
  for (type in property_types) {
    config$property_types[[type]] <- list(
      tax_rate = 0.01,  # 1% default
      minimum_tax = 100  # Default minimum
    )
  }
  
  # Set default feature weights
  features <- get_property_features(processed_data)
  for (feature in features) {
    # Different default weights based on feature type
    if (grepl("^has_", feature)) {
      weight <- 0.1  # Binary features get lower weight
    } else if (feature == "property_area") {
      weight <- 1.0  # Area gets highest weight
    } else {
      weight <- 0.5  # Other features get medium weight
    }
    config$property_features[[feature]] <- weight
  }
  
  # Set default business category configs
  categories <- get_business_categories(processed_data)
  for (category in categories) {
    config$business_categories[[category]] <- list(
      tax_rate = 0.02,  # 2% default for businesses
      minimum_tax = 200  # Higher minimum for businesses
    )
  }
  
  return(config)
}

# Function to save configuration
save_configuration <- function(config, scenario_name) {
  # Create configs directory if it doesn't exist
  if (!dir.exists("configs")) {
    dir.create("configs")
  }
  
  # Save as RDS file
  filename <- paste0("configs/", scenario_name, "_", Sys.Date(), ".rds")
  saveRDS(config, filename)
  
  return(filename)
}

# Function to load configuration
load_configuration <- function(filename) {
  if (file.exists(filename)) {
    return(readRDS(filename))
  } else {
    stop("Configuration file not found")
  }
}

# Function to calculate property value based on features and weights
calculate_property_values <- function(processed_data, feature_weights) {
  # Initialize value with base amount
  values <- rep(1000, nrow(processed_data))  # Base value
  
  # Add weighted feature contributions
  for (feature in names(feature_weights)) {
    if (feature %in% names(processed_data)) {
      weight <- feature_weights[[feature]]
      
      if (feature == "property_area") {
        # Area has multiplicative effect
        values <- values * (1 + processed_data[[feature]] * weight / 1000)
      } else if (is.numeric(processed_data[[feature]])) {
        # Numeric features
        values <- values + (processed_data[[feature]] * weight * 100)
      } else {
        # Binary/categorical features (dummy variables)
        values <- values + (processed_data[[feature]] * weight * 500)
      }
    }
  }
  
  return(values)
}

# Function to apply inflation adjustment
apply_inflation <- function(values, inflation_rate) {
  return(values * (1 + inflation_rate / 100))
}