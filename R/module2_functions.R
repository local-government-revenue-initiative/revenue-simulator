# R/module2_functions.R

# Function to get default weights for property features
get_default_weights <- function() {
  list(
    base_value = 231.859128,  # Default from Freetown
    inflation = 0,  # 0% = no adjustment
    area_weight = 0.5,  # Default from Freetown
    
    # All feature weights in a single flat list for easy lookup
    feature_weights = list(
      # Wall material features
      wall_material_Masonry = 0,
      wall_material_Mud = -28,
      wall_material_Stone = 0,
      wall_material_Wood = -28,
      wall_material_Zinc = -49,
      # Wall condition features
      wall_condition_Average = 0,
      wall_condition_Bad = -11,
      wall_condition_Good = 25,
      # Veranda features
      has_veranda_No = 0,
      has_veranda_Yes = 3,
      # Roof material features
      roof_material_Asbestos = -21,
      roof_material_Concrete = 0,
      `roof_material_Galvanised Aluminium` = 0,
      `roof_material_Not Visible` = 0,
      roof_material_Tile = 0,
      `roof_material_Zinc / Metal Sheeting` = -21,
      # Roof condition features
      roof_condition_Average = 0,
      roof_condition_Bad = -38,
      roof_condition_Good = 20,
      `roof_condition_Not Visible` = 0,
      # Window material features
      `window_material_Breeze Block` = -5,
      window_material_Louvre = 0,
      `window_material_No Windows` = -20,
      `window_material_Sliding panels with aluminium frame` = 15,
      `window_material_Traditional glazed casement set in metal frame or wood frame` = 5,
      window_material_Wood = -20,
      drainage_No = 0,
      drainage_Yes = 2,
      air_conditioning_No = 0,
      air_conditioning_Yes = 20,
      has_security_No = 0,
      has_security_Yes = 25,
      has_pool_No = 0,
      has_pool_Yes = 50,
      has_outbuilding_No = 0,
      has_outbuilding_Yes = 10,
      has_water_No = 0,
      has_water_Yes = 2,
      street_access_Difficult = -7,
      street_access_Easy = 0,
      street_access_NA = 0,
      `street_quality_Average Road` = 0,
      `street_quality_Bad Road` = -10,
      `street_quality_Good Road` = 4,
      street_lanes_Four = 4,
      street_lanes_None = -15,
      street_lanes_One = 0,
      street_lanes_Two = 2,
      old_tourist_area_No = 0,
      old_tourist_area_Yes  = 24,
      old_environmental_hazard_No = 0,
      old_environmental_hazard_Yes = -15,
      main_road_high_visibility_No = 0,
      main_road_high_visibility_Yes = 18,
      old_informal_settlement_Yes = -21,
      old_informal_settlement_No = 0,
      old_commercial_corridor_No = 0,
      old_commercial_corridor_Yes = 25,
      potential_to_build_No = 0,
      potential_to_build_Yes = 3,
      domestic_use_groundfloor_No = 5,
      domestic_use_groundfloor_Yes = 0,
      ward_number_399 = 20,
      ward_number_400 = 30,
      ward_number_401 = 17,
      ward_number_402 = 47,
      ward_number_403 = 23,
      ward_number_404 = 4,
      ward_number_405 = 14,
      ward_number_406 = 15,
      ward_number_407 = 27,
      ward_number_408 = 30,
      ward_number_409 = 11,
      ward_number_410 = 6,
      ward_number_411 = -15,
      ward_number_412 = 42,
      ward_number_413 = 31,
      ward_number_414 = 52,
      ward_number_415 = 45,
      ward_number_416 = 35,
      ward_number_417 = 56,
      ward_number_418 = 20,
      ward_number_419 = 25,
      ward_number_420 = 68,
      ward_number_421 = 106,
      ward_number_422 = 60,
      ward_number_423 = 36,
      ward_number_424 = 38,
      ward_number_425 = 78,
      ward_number_426 = 38,
      ward_number_427 = 220,
      ward_number_428 = 189,
      ward_number_429 = 192,
      ward_number_430 = 211,
      ward_number_431 = 213,
      ward_number_432 = 139,
      ward_number_433 = 176,
      ward_number_434 = 46,
      ward_number_435 = 71,
      ward_number_436 = 217,
      ward_number_437 = 178,
      ward_number_438 = 196,
      ward_number_439 = 82,
      ward_number_440 = 120,
      ward_number_441 = 123,
      ward_number_442 = 126,
      ward_number_443 = 129,
      ward_number_444 = 118,
      ward_number_445 = 61,
      ward_number_446 = 180,
      ward_number_NA = 0,
      aberdeen_lumley_tourist_Yes = 0,
      aberdeen_lumley_tourist_No = 0,
      juba_levuma_tourist_Yes = 0,
      juba_levuma_tourist_No = 0,
      buffered_commercial_corridors_Yes = 0,
      buffered_commercial_corridors_No = 0,
      cbd_Yes = 0,
      cbd_No = 0,
      dock_industrial_Yes = 0,
      dock_industrial_No = 0,
      kissy_industrial_area_Yes = 0,
      kissy_industrial_area_No = 0,
      kissy_texaco_terminal_area_Yes = 0,
      kissy_texaco_terminal_area_No = 0,
      wellington_industrial_estate_Yes = 0,
      wellington_industrial_estate_No = 0,
      hazardous_zones_Yes = 0,
      hazardous_zones_No = 0,
      informal_settlements_Yes = 0,
      informal_settlements_No = 0      
    ),
    # Structure type weights in a separate list
    structure_weights = list(
      `commercial_type_Industrial Manufacturing` = -37,
      `commercial_type_Industrial Warehouse` = -47,
      commercial_type_NA = 0,
      `commercial_type_Bank` = 106,
      `commercial_type_Car Dealership` = 24,
      `commercial_type_Commercial Warehouse` = 47,
      `commercial_type_Golf Clubhouse` = 0,
      `commercial_type_Guest House` = 4,
      `commercial_type_Gym` = 0,
      `commercial_type_Hotel` = 70,
      `commercial_type_House or dwelling form` = 0,
      `commercial_type_Industrial Manufacturing` = -37,
      `commercial_type_Industrial Warehouse` = -47,
      `commercial_type_Mixed Retail / Office` = 13,
      `commercial_type_Motor Garage` = -30,
      `commercial_type_Office` = 37,
      `commercial_type_Petrol Station` = 53,
      `commercial_type_Private Clinic` = 72,
      `commercial_type_Private Clinic/Hospital` = 72,
      `commercial_type_Private School` = 44,
      `commercial_type_Retail` = 3,
      `commercial_type_Squash Court` = 0,
      `commercial_type_Storage` = 0,
      `commercial_type_Super Market` = 7,
      `commercial_type_NA` = 0,
      `institutional_type_Government Office` = 79,
      `instutional_type_Parliament Buildings` = 79,
      `institutional_type_Police Buildings` = 70,
      `institutional_type_Police compounds` = 70,
      `institutional_type_Court Buildings` = 79,
      `institutional_type_Municipal Offices` = 79,
      `institutional_type_Army barraks and installations` = 43,
      `institutional_type_Fire Station` = 43,
      `institutional_type_Bank` = 106,
      `institutional_type_Bollore Garage` = 1894,
      `institutional_type_Commercial Warehouse` = 47,
      `institutional_type_Dangote Office` = 346,
      `institutional_type_House or dwelling form` = 0,
      `institutional_type_Industrial Manufacturing` = -37,
      `institutional_type_Industrial Warehouse` = -47,
      `institutional_type_Leocem Industrial Manufacturing` = -37,
      `institutional_type_Leocem Office` = 37,
      `institutional_type_Mixed Retail / Office` = 13,
      `institutional_type_Nectar House Form` = 174,
      `institutional_type_Nectar Office` = 275,
      `institutional_type_Nectar Cargo` = 182,
      `institutional_type_Nectar Warehouse` = 46,
      `institutional_type_Office` = 275,
      `institutional_type_SLPA Industrial` = 158,
      `institutional_type_SLPA Warehouse` = 119,
      `institutional_type_SLPA Canteen` = 322,
      `institutional_type_SLPA House Form` = 310,
      `institutional_type_SLPA Motor Garage` = 186,
      `institutional_type_SLPA Office` = 461,
      `institutional_type_Bollore Office` = 3819,
      `institutional_type_Bollore Warehouse` = 1426,
      `institutional_type_Dangote Warehouse` = 73,
      institutional_type_NA = 0
    )
  )
}

# Function to group feature columns by category (excluding _na variables from UI)
group_feature_columns <- function(column_names) {
  # Filter out variables ending in _na for UI display
    visible_columns <- column_names[!grepl("(_na|_NA)$", column_names)]
  
  # Remove the dummy variable suffixes to get base feature names
  base_features <- unique(gsub("_[^_]+$", "", visible_columns))
  
  structure_features <- c("wall_material", "wall_condition", "roof_material", 
                          "roof_condition", "window_material", "has_veranda")
  
  utility_features <- c("has_water", "drainage", "air_conditioning",
                        "has_security", "has_pool", "has_outbuilding")
  
  location_features <- c("street_access", "street_quality", "street_lanes", 
                         "tourist_area", "old_tourist_area", 
                         "environmental_hazard", "old_environmental_hazard",
                         "main_road_high_visibility", 
                         "informal_settlement", "old_informal_settlement",
                         "commercial_corridor", "old_commercial_corridor")
  
  location_zones <- c("aberdeen_lumley_tourist", "juba_levuma_tourist",
                     "buffered_commercial_corridors", "cbd", "dock_industrial",
                     "kissy_industrial_area", "kissy_texaco_terminal_area",
                     "wellington_industrial_estate", "hazardous_zones",
                     "informal_settlements")
  
  property_characteristics <- c("potential_to_build", "domestic_use_groundfloor")
  
  # Get actual column names for each category (excluding _na columns)
  grouped <- list(
    structure_features = visible_columns[grepl(paste0("^(", paste(structure_features, collapse = "|"), ")_"), visible_columns)],
    utility_features = visible_columns[grepl(paste0("^(", paste(utility_features, collapse = "|"), ")_"), visible_columns)],
    location_features = visible_columns[grepl(paste0("^(", paste(location_features, collapse = "|"), ")_"), visible_columns)],
    location_zones = visible_columns[grepl(paste0("^(", paste(location_zones, collapse = "|"), ")_"), visible_columns)],
    property_characteristics = visible_columns[grepl(paste0("^(", paste(property_characteristics, collapse = "|"), ")_"), visible_columns)],
    ward_features = visible_columns[grepl("^ward_(number|rank)_", visible_columns)]
  )
  
  return(grouped)
}

# Function to extract structure type columns (excluding _na variables from UI)
get_structure_type_columns <- function(column_names) {
  # Filter out variables ending in _na for UI display
  visible_columns <- column_names[!grepl("(_na|_NA)$", column_names)]
  
  structure_types <- c("commercial_type", "institutional_type")
  structure_cols <- visible_columns[grepl(paste0("^(", paste(structure_types, collapse = "|"), ")_"), visible_columns)]
  return(structure_cols)
}

# Function to get all feature columns including hidden _na variables (for calculations)
get_all_feature_columns <- function(column_names) {
  # Remove non-feature columns
  feature_cols <- column_names[!column_names %in% c("id_property", "coordinate_lat", 
                                                    "coordinate_lng", "property_area",
                                                    "made_payment", "business_name",
                                                    "business_area", "business_category",
                                                    "business_sub_category")]
  
  # Group features by category (including _na variables)
  structure_features <- c("wall_material", "wall_condition", "roof_material", 
                          "roof_condition", "window_material", "has_veranda")
  
  utility_features <- c("has_water", "drainage", "air_conditioning",
                        "has_security", "has_pool", "has_outbuilding")
  
  location_features <- c("street_access", "street_quality", "street_lanes", 
                         "tourist_area", "old_tourist_area", 
                         "environmental_hazard", "old_environmental_hazard",
                         "main_road_high_visibility", 
                         "informal_settlement", "old_informal_settlement",
                         "commercial_corridor", "old_commercial_corridor")
  
  location_zones <- c("aberdeen_lumley_tourist", "juba_levuma_tourist",
                     "buffered_commercial_corridors", "cbd", "dock_industrial",
                     "kissy_industrial_area", "kissy_texaco_terminal_area",
                     "wellington_industrial_estate", "hazardous_zones",
                     "informal_settlements")
  
  property_characteristics <- c("potential_to_build", "domestic_use_groundfloor")
  
  # Get all columns (including _na) for calculations
  all_features <- c(
    feature_cols[grepl(paste0("^(", paste(structure_features, collapse = "|"), ")_"), feature_cols)],
    feature_cols[grepl(paste0("^(", paste(utility_features, collapse = "|"), ")_"), feature_cols)],
    feature_cols[grepl(paste0("^(", paste(location_features, collapse = "|"), ")_"), feature_cols)],
    feature_cols[grepl(paste0("^(", paste(location_zones, collapse = "|"), ")_"), feature_cols)],
    feature_cols[grepl(paste0("^(", paste(property_characteristics, collapse = "|"), ")_"), feature_cols)],
    feature_cols[grepl("^ward_", feature_cols)]
  )
  
  return(all_features)
}

# Function to get all structure type columns including _na (for calculations)
get_all_structure_columns <- function(column_names) {
  structure_types <- c("commercial_type", "institutional_type")
  structure_cols <- column_names[grepl(paste0("^(", paste(structure_types, collapse = "|"), ")_"), column_names)]
  return(structure_cols)
}

# Function to validate weight ranges
validate_weights <- function(weights, weight_type = "feature") {
  errors <- character()
  
  if (weight_type == "feature") {
    # Feature weights typically range from -250 to 250
    for (name in names(weights)) {
      if (!is.numeric(weights[[name]])) {
        errors <- c(errors, paste(name, "must be numeric"))
      } else if (weights[[name]] < -250 || weights[[name]] > 250) {
        errors <- c(errors, paste(name, "should be between -250 and 250"))
      }
    }
  } else if (weight_type == "structure") {
    # Structure type weights can go up to 5000
    for (name in names(weights)) {
      if (!is.numeric(weights[[name]])) {
        errors <- c(errors, paste(name, "must be numeric"))
      } else if (weights[[name]] < 0 || weights[[name]] > 5000) {
        errors <- c(errors, paste(name, "should be between 0 and 5000"))
      }
    }
  }
  
  return(errors)
}

# Function to save scenario configuration
save_scenario_config <- function(scenario_name, config) {
  # Convert to JSON for storage
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE, pretty = TRUE)
  
  # For now, just return the JSON
  # In production, you might save to a file or database
  return(config_json)
}

# Function to load scenario configuration
load_scenario_config <- function(scenario_json) {
  # Parse JSON back to list
  config <- jsonlite::fromJSON(scenario_json, simplifyVector = FALSE)
  return(config)
}

# Function to calculate property value based on formula
calculate_property_value <- function(base_value, inflation, area, area_weight, feature_weights, feature_values) {
  # Formula: property_value = base_value * inflation * area^area_weight * product((feature_weight/100 + 1)^feature)
  
  value <- base_value * inflation * (area ^ area_weight)
  
  # Apply feature weights
  for (i in seq_along(feature_weights)) {
    if (!is.na(feature_values[i]) && feature_values[i] == 1) {
      value <- value * ((feature_weights[i] / 100) + 1)
    }
  }
  
  return(value)
}

# Add this debugging version of calculate_property_value to your functions
calculate_property_value_debug <- function(base_value, inflation, area, area_weight, 
                                         feature_weights, feature_values, property_id = NULL) {
  
  if (!is.null(property_id)) {
    cat("=== DEBUG for property:", property_id, "===\n")
  }
  
  # Calculate inflation-adjusted base value
  inflation_adjusted_base <- base_value * (1 + inflation/100)
  cat("Base value:", base_value, "\n")
  cat("Inflation:", inflation, "\n")
  cat("Inflation-adjusted base:", inflation_adjusted_base, "\n")
  
  # Calculate area component
  area_component <- area^area_weight
  cat("Area:", area, "\n")
  cat("Area weight:", area_weight, "\n")
  cat("Area component (area^weight):", area_component, "\n")
  
  # Feature weights calculation - this is the key part
  cat("\n--- Feature Analysis ---\n")
  
  # Identify which features have non-zero weights
  non_zero_weights <- feature_weights[feature_weights != 0]
  cat("Features with non-zero weights:\n")
  print(non_zero_weights)
  
  # Calculate individual feature contributions
  feature_contributions <- feature_weights * feature_values
  non_zero_contributions <- feature_contributions[feature_contributions != 0]
  cat("\nNon-zero feature contributions (weight * value):\n")
  print(non_zero_contributions)
  
  # Product of all feature weights
  product_weights <- prod(feature_weights^feature_values)
  cat("\nProduct of all feature weights:", product_weights, "\n")
  
  # Manual verification
  cat("\nManual calculation check:\n")
  manual_product <- 1
  for(i in 1:length(feature_weights)) {
    if(feature_values[i] != 0) {
      contribution <- feature_weights[i]^feature_values[i]
      manual_product <- manual_product * contribution
      cat("Feature", i, ": weight =", feature_weights[i], 
          ", value =", feature_values[i], 
          ", contribution =", contribution, 
          ", running product =", manual_product, "\n")
    }
  }
  
  # Final calculation
  property_value <- inflation_adjusted_base * area_component * product_weights
  cat("\nFinal calculation:", inflation_adjusted_base, "*", area_component, "*", product_weights, "=", property_value, "\n")
  cat("===========================\n\n")
  
  return(property_value)
}