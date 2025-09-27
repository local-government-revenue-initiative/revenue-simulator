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
      aid_conditioning_No = 0,
      aid_conditioning_Yes = 20,
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
      tourist_area_No = 0,
      tourist_area_Yes = 24,
      environmental_hazard_No = 0,
      environmental_hazard_Yes = -15,
      main_road_high_visibility_No = 0,
      main_road_high_visibility_Yes = 18,
      informal_settlement_No = 0,
      informal_settlement_Yes = -21,
      commercial_corridor_No = 0,
      commercial_corridor_Yes = 25,
      potential_to_build_No = 0,
      potential_to_build_Yes = 3,
      domestic_use_of_groundfloor_No = 5,
      domestic_use_of_groundfloor_Yes = 0,
      ward_399 = 20,
      ward_400 = 30,
      ward_401 = 17,
      ward_402 = 47,
      ward_403 = 23,
      ward_404 = 4,
      ward_405 = 14,
      ward_406 = 15,
      ward_407 = 27,
      ward_408 = 30,
      ward_409 = 11,
      ward_410 = 6,
      ward_411 = -15,
      ward_412 = 42,
      ward_413 = 31,
      ward_414 = 52,
      ward_415 = 45,
      ward_416 = 35,
      ward_417 = 56,
      ward_418 = 20,
      ward_419 = 25,
      ward_420 = 68,
      ward_421 = 106,
      ward_422 = 60,
      ward_423 = 36,
      ward_424 = 38,
      ward_425 = 78,
      ward_426 = 38,
      ward_427 = 220,
      ward_428 = 189,
      ward_429 = 192,
      ward_430 = 211,
      ward_431 = 213,
      ward_432 = 139,
      ward_433 = 176,
      ward_434 = 46,
      ward_435 = 71,
      ward_436 = 217,
      ward_437 = 178,
      ward_438 = 196,
      ward_439 = 82,
      ward_440 = 120,
      ward_441 = 123,
      ward_442 = 126,
      ward_443 = 129,
      ward_444 = 118,
      ward_445 = 61,
      ward_446 = 180,
      ward_NA = 0,
      aberdeen_lumley_tourist_Yes = 25,
      aberdeen_lumley_tourist_No = 0,
      juba_levuma_tourist_Yes = 20,
      juba_levuma_tourist_No = 0,
      buffered_commercial_corridors_Yes = 30,
      buffered_commercial_corridors_No = 0,
      cbd_Yes = 50,
      cbd_No = 0,
      dock_industrial_Yes = -20,
      dock_industrial_No = 0,
      kissy_industrial_area_Yes = -25,
      kissy_industrial_area_No = 0,
      kissy_texaco_terminal_area_Yes = -15,
      kissy_texaco_terminal_area_No = 0,
      wellington_industrial_estate_Yes = -20,
      wellington_industrial_estate_No = 0,
      hazardous_zones_Yes = -30,
      hazardous_zones_No = 0,
      informal_settlements_Yes = -25,
      informal_settlements_No = 0      
    ),
    # Structure type weights in a separate list
    structure_weights = list(
      `commercial_type_Industrial Manufacturing` = -37,
      `commercial_type_Industrial Warehouse` = -47,
      commercial_type_NA = 0,
      `institutional_type_Nectar Cargo` = 182,
      `institutional_type_Nectar Warehouse` = 46,
      `institutional_type_SLPA Industrial` = 158,
      `institutional_type_SLPA Warehouse` = 119,
      `institutional_type_Bollore Office` = 3819,
      `institutional_type_Bollore Warehouse` = 1426,
      `institutional_type_Dangote Warehouse` = 73,
      institutional_type_NA = 0
    )
  )
}

# Function to group feature columns by category
group_feature_columns <- function(column_names) {
  # Remove the dummy variable suffixes to get base feature names
  base_features <- unique(gsub("_[^_]+$", "", column_names))
  
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
  
  # Get actual column names for each category
  grouped <- list(
    structure_features = column_names[grepl(paste0("^(", paste(structure_features, collapse = "|"), ")_"), column_names)],
    utility_features = column_names[grepl(paste0("^(", paste(utility_features, collapse = "|"), ")_"), column_names)],
    location_features = column_names[grepl(paste0("^(", paste(location_features, collapse = "|"), ")_"), column_names)],
    location_zones = column_names[grepl(paste0("^(", paste(location_zones, collapse = "|"), ")_"), column_names)],
    property_characteristics = column_names[grepl(paste0("^(", paste(property_characteristics, collapse = "|"), ")_"), column_names)],
    ward_features = column_names[grepl("^ward_", column_names)]
  )
  
  return(grouped)
}

# Function to extract structure type columns
get_structure_type_columns <- function(column_names) {
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