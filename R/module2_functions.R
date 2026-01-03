# R/module2_functions.R
# Module 2 Functions - Updated to use parameter tables for defaults

# ==============================================================================
# NULL COALESCING OPERATOR
# ==============================================================================

#' Null coalescing operator
#' Returns y if x is NULL, otherwise returns x
#' @param x Value to check
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ==============================================================================
# DEFAULT WEIGHTS FROM PARAMETER TABLES
# ==============================================================================

#' Build default feature weights from param_features table
#' Creates a named list with full column names (e.g., "has_water_Yes", "wall_material_Masonry")
#' @param param_features Data frame with columns: feature, feature_options,
#'   weight_feature_option, feature_label, feature_category
#' @return Named list of feature weights keyed by full column name
build_feature_weights_from_params <- function(param_features) {
  if (is.null(param_features) || nrow(param_features) == 0) {
    return(list())
  }

  weights <- list()

  for (i in seq_len(nrow(param_features))) {
    row <- param_features[i, ]

    # Build the full column name format: "feature_option"
    # e.g., "has_water" + "Yes" → "has_water_Yes"
    # e.g., "wall_material" + "Masonry" → "wall_material_Masonry"
    col_name <- paste0(row$feature, "_", row$feature_options)
    weights[[col_name]] <- row$weight_feature_option
  }

  return(weights)
}

#' Build default structure type weights from param_prop_struct_type table
#' @param param_prop_struct_type Data frame with columns: property_type,
#'   structure_type, weight_structure_type
#' @return Named list of structure weights with column name format
#'   (e.g., "commercial_type_Bank")
build_structure_weights_from_params <- function(param_prop_struct_type) {
  if (is.null(param_prop_struct_type) || nrow(param_prop_struct_type) == 0) {
    return(list())
  }

  weights <- list()

  for (i in seq_len(nrow(param_prop_struct_type))) {
    row <- param_prop_struct_type[i, ]
    prop_type <- tolower(row$property_type)
    struct_type <- row$structure_type
    weight <- row$weight_structure_type

    # Build column name format: "commercial_type_Bank", "institutional_type_Office"
    col_name <- paste0(prop_type, "_type_", struct_type)
    weights[[col_name]] <- weight
  }

  return(weights)
}

#' Get base value and area weight from param_additions table
#' @param param_additions Data frame with columns: base_value, area_weight
#' @return List with base_value and area_weight
get_base_params_from_additions <- function(param_additions) {
  if (is.null(param_additions) || nrow(param_additions) == 0) {
    return(list(base_value = 231.859128, area_weight = 0.5))
  }

  return(list(
    base_value = param_additions$base_value[1],
    area_weight = param_additions$area_weight[1]
  ))
}

#' Get feature metadata for UI generation (labels and categories)
#' @param param_features Data frame with feature parameters
#' @return Data frame with unique features, their labels, and categories
get_feature_metadata <- function(param_features) {
  if (is.null(param_features) || nrow(param_features) == 0) {
    return(data.frame(
      feature = character(),
      feature_label = character(),
      feature_category = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Get unique features with their labels and categories
  # Include all feature_options, not just "Yes"
  metadata <- param_features %>%
    select(feature, feature_options, feature_label, feature_category) %>%
    distinct() %>%
    # Create full column name for matching
    mutate(full_col_name = paste0(feature, "_", feature_options))

  return(metadata)
}

#' Look up feature weight from param_features table
#' Handles both direct column name lookup and base_feature + option lookup
#' @param feature_col_name Full column name like "has_water_Yes" or "wall_material_Masonry"
#' @param param_features Data frame with feature parameters
#' @param default_weights Named list of pre-built weights (optional, for faster lookup)
#' @return Weight value or 0 if not found
get_feature_weight <- function(
  feature_col_name,
  param_features = NULL,
  default_weights = NULL
) {
  # First try direct lookup in pre-built weights
  if (
    !is.null(default_weights) && feature_col_name %in% names(default_weights)
  ) {
    return(default_weights[[feature_col_name]])
  }

  # If param_features provided, try to extract base feature and option
  if (!is.null(param_features)) {
    # Try to split: "wall_condition_Good" → base="wall_condition", option="Good"
    # Use negative lookahead to split on last underscore
    parts <- strsplit(feature_col_name, "_(?=[^_]+$)", perl = TRUE)[[1]]

    if (length(parts) == 2) {
      base_feature <- parts[1]
      option <- parts[2]

      match_row <- param_features[
        param_features$feature == base_feature &
          param_features$feature_options == option,
      ]

      if (nrow(match_row) > 0) {
        return(match_row$weight_feature_option[1])
      }
    }
  }

  # Default to 0 if not found
  return(0)
}

# ==============================================================================
# LEGACY FUNCTION - Get default weights (now wrapper around param-based functions)
# ==============================================================================

#' Get default weights - legacy function maintained for compatibility
#' This is called when parameter tables are not available
#' @return List with feature_weights and structure_weights
get_default_weights <- function() {
  # Return empty lists - actual defaults come from parameter tables
  # This function is kept for backward compatibility
  list(
    feature_weights = list(),
    structure_weights = list()
  )
}

# ==============================================================================
# FEATURE COLUMN DETECTION
# ==============================================================================

#' Categorize feature columns from data based on naming patterns
#' @param column_names Vector of column names from the data
#' @return List of categorized feature columns
categorize_feature_columns <- function(column_names) {
  # Define patterns for each category
  structure_features <- c(
    "wall_material",
    "wall_condition",
    "roof_material",
    "roof_condition",
    "window_material",
    "has_veranda",
    "air_conditioning"
  )

  utility_features <- c(
    "has_water",
    "drainage",
    "has_security",
    "has_pool",
    "has_outbuilding"
  )

  location_features <- c(
    "street_access",
    "street_quality",
    "street_lanes",
    "main_road_high_visibility",
    "potential_to_build",
    "domestic_use_groundfloor"
  )

  location_zones <- c(
    "old_tourist_area",
    "old_environmental_hazard",
    "old_informal_settlement",
    "old_commercial_corridor",
    "aberdeen_lumley_tourist",
    "juba_levuma_tourist",
    "buffered_commercial_corridors",
    "cbd",
    "dock_industrial",
    "kissy_industrial_area",
    "kissy_texaco_terminal_area",
    "grassfield_industrial_area",
    "wellington_industrial_estate",
    "hazardous_zones",
    "informal_settlements"
  )

  # Find matching columns (with _Yes suffix for dummy variables)
  find_feature_cols <- function(patterns) {
    matched <- character()
    for (pattern in patterns) {
      # Match columns like "wall_material_Masonry" or "has_water_Yes"
      matches <- column_names[grepl(paste0("^", pattern, "_"), column_names)]
      # Exclude _NA columns
      matches <- matches[!grepl("_NA$|_na$", matches)]
      matched <- c(matched, matches)
    }
    unique(matched)
  }

  # Find ward columns
  ward_cols <- column_names[grepl("^ward_number_", column_names)]
  ward_cols <- ward_cols[!grepl("_NA$|_na$", ward_cols)]

  list(
    structure_features = find_feature_cols(structure_features),
    utility_features = find_feature_cols(utility_features),
    location_features = find_feature_cols(location_features),
    location_zones = find_feature_cols(location_zones),
    property_characteristics = character(), # Can be expanded if needed
    ward_columns = ward_cols
  )
}

#' Get all feature columns from data (for UI generation and calculations)
#' @param column_names Vector of column names
#' @return Vector of all feature column names
get_all_feature_columns <- function(column_names) {
  # Get categorized columns
  categorized <- categorize_feature_columns(column_names)

  # Combine all
  all_features <- c(
    categorized$structure_features,
    categorized$utility_features,
    categorized$location_features,
    categorized$location_zones,
    categorized$property_characteristics,
    categorized$ward_columns
  )

  unique(all_features)
}

#' Get structure type columns from data
#' @param column_names Vector of column names
#' @return List with commercial_type_columns and institutional_type_columns
get_structure_type_columns <- function(column_names) {
  commercial_cols <- column_names[grepl("^commercial_type_", column_names)]
  commercial_cols <- commercial_cols[!grepl("_NA$|_na$", commercial_cols)]

  institutional_cols <- column_names[grepl(
    "^institutional_type_",
    column_names
  )]
  institutional_cols <- institutional_cols[
    !grepl("_NA$|_na$", institutional_cols)
  ]

  list(
    commercial_type_columns = commercial_cols,
    institutional_type_columns = institutional_cols
  )
}

# ==============================================================================
# WEIGHT VALIDATION
# ==============================================================================

#' Validate weight ranges
#' @param weights Named list of weights
#' @param weight_type Either "feature" or "structure"
#' @return Character vector of error messages (empty if valid)
validate_weights <- function(weights, weight_type = "feature") {
  errors <- character()

  if (weight_type == "feature") {
    for (name in names(weights)) {
      if (!is.numeric(weights[[name]])) {
        errors <- c(errors, paste(name, "must be numeric"))
      } else if (weights[[name]] < -250 || weights[[name]] > 250) {
        errors <- c(errors, paste(name, "should be between -250 and 250"))
      }
    }
  } else if (weight_type == "structure") {
    for (name in names(weights)) {
      if (!is.numeric(weights[[name]])) {
        errors <- c(errors, paste(name, "must be numeric"))
      } else if (weights[[name]] < -100 || weights[[name]] > 5000) {
        errors <- c(errors, paste(name, "should be between -100 and 5000"))
      }
    }
  }

  errors
}

# ==============================================================================
# PROPERTY VALUE CALCULATION
# ==============================================================================

#' Calculate property value based on formula
#' Formula: property_value = base_value * (1 + inflation/100) * area^area_weight *
#'          product((feature_weight/100 + 1)^feature_value)
#' @param base_value Base property value
#' @param inflation Inflation percentage (0 = no inflation, 50 = 50%)
#' @param area Property area
#' @param area_weight Exponent for area
#' @param feature_weights Named vector of feature weights
#' @param feature_values Named vector of feature values (0 or 1)
#' @return Calculated property value
calculate_property_value <- function(
  base_value,
  inflation,
  area,
  area_weight,
  feature_weights,
  feature_values
) {
  # Calculate inflation-adjusted base
  inflation_adjusted <- base_value * (1 + inflation / 100)

  # Calculate area component
  area_component <- area^area_weight

  # Calculate feature multiplier
  feature_multiplier <- 1
  for (i in seq_along(feature_weights)) {
    if (!is.na(feature_values[i]) && feature_values[i] == 1) {
      feature_multiplier <- feature_multiplier *
        ((feature_weights[i] / 100) + 1)
    }
  }

  inflation_adjusted * area_component * feature_multiplier
}

# ==============================================================================
# CONFIGURATION SAVE/LOAD (kept for compatibility)
# ==============================================================================

#' Save scenario configuration to JSON
#' @param scenario_name Name of the scenario
#' @param config Configuration list
#' @return JSON string
save_scenario_config <- function(scenario_name, config) {
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE, pretty = TRUE)
  config_json
}

#' Load scenario configuration from JSON
#' @param scenario_json JSON string
#' @return Configuration list
load_scenario_config <- function(scenario_json) {
  jsonlite::fromJSON(scenario_json, simplifyVector = FALSE)
}
