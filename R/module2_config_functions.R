# ==============================================================================
# MODULE 2: Configuration Save/Load Functions - COMPLETE FIXED VERSION
# ==============================================================================
# This file contains ALL fixes:
# 1. location_zones included in collect and apply functions
# 2. Proper space-to-underscore conversion for structure type names
#
# Replace the entire content of: R/module2_config_functions.R
# ==============================================================================

# Function to collect all Module 2 configuration from inputs
collect_module2_config <- function(
  input,
  scenario_suffix,
  feature_columns,
  commercial_type_columns,
  institutional_type_columns,
  ward_columns
) {
  config <- list(
    module = "module2",
    scenario = scenario_suffix,
    timestamp = Sys.time(),

    # Base parameters
    base_value = input[[paste0("base_value_", scenario_suffix)]],
    inflation = input[[paste0("inflation_", scenario_suffix)]],
    area_weight = input[[paste0("area_weight_", scenario_suffix)]],

    # Feature weights
    feature_weights = list(),

    # Structure type weights
    structure_type_weights = list()
  )

  # FIXED: Collect all feature weights INCLUDING location_zones
  all_features <- c(
    feature_columns$structure_features,
    feature_columns$utility_features,
    feature_columns$location_features,
    feature_columns$location_zones, # <--- FIXED: Added this line!
    feature_columns$property_characteristics,
    ward_columns
  )

  for (feat in all_features) {
    feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
    input_id <- paste0("weight_", feat_safe, "_", scenario_suffix)
    weight_value <- input[[input_id]]

    if (!is.null(weight_value)) {
      config$feature_weights[[feat]] <- weight_value
    }
  }

  # Collect all structure type weights
  all_structures <- c(commercial_type_columns, institutional_type_columns)

  for (struct in all_structures) {
    # FIXED: Convert spaces to underscores so configs are consistent
    # Data columns may have spaces like 'commercial_type_Car Dealership'
    # We want to save as 'commercial_type_Car_Dealership' in the JSON
    struct_clean <- gsub(" ", "_", struct)

    # Additional sanitization for special characters
    struct_safe <- gsub("[^A-Za-z0-9_]", "_", struct_clean)
    struct_safe <- gsub("_+", "_", struct_safe) # Replace multiple underscores

    input_id <- paste0("weight_", struct_safe, "_", scenario_suffix)
    weight_value <- input[[input_id]]

    if (!is.null(weight_value)) {
      # Save with cleaned name (spaces converted to underscores)
      config$structure_type_weights[[struct_clean]] <- weight_value
    }
  }

  return(config)
}

# Function to save configuration to JSON file
save_module2_config <- function(config, scenario_name = NULL) {
  if (is.null(scenario_name)) {
    scenario_name <- config$scenario
  }

  # Convert to JSON with pretty formatting
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE, pretty = TRUE)

  # Create filename with timestamp
  filename <- paste0(
    "module2_",
    scenario_name,
    "_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    ".json"
  )

  # Create temporary file
  temp_file <- tempfile(fileext = ".json")
  writeLines(config_json, temp_file)

  return(list(
    filepath = temp_file,
    filename = filename,
    config = config
  ))
}

# Function to load configuration from JSON
load_module2_config <- function(filepath) {
  tryCatch(
    {
      # Read JSON from file
      config_json <- readLines(filepath, warn = FALSE)
      config_json <- paste(config_json, collapse = "\n")

      # Parse JSON back to list
      config <- jsonlite::fromJSON(config_json, simplifyVector = FALSE)

      # Validate it's a Module 2 config
      if (is.null(config$module) || config$module != "module2") {
        stop("This is not a valid Module 2 configuration file")
      }

      return(config)
    },
    error = function(e) {
      stop(paste("Error loading configuration:", e$message))
    }
  )
}

# FIXED: Function to apply loaded configuration with proper name matching
apply_module2_config <- function(
  session,
  config,
  scenario_suffix,
  feature_columns,
  commercial_type_columns,
  institutional_type_columns,
  ward_columns
) {
  # Update base parameters
  updateNumericInput(
    session,
    paste0("base_value_", scenario_suffix),
    value = config$base_value
  )
  updateNumericInput(
    session,
    paste0("inflation_", scenario_suffix),
    value = config$inflation
  )
  updateNumericInput(
    session,
    paste0("area_weight_", scenario_suffix),
    value = config$area_weight
  )

  # Update all feature weights (including location_zones)
  all_features <- c(
    feature_columns$structure_features,
    feature_columns$utility_features,
    feature_columns$location_features,
    feature_columns$location_zones, # FIXED: Include location zones
    feature_columns$property_characteristics,
    ward_columns
  )

  for (feat in all_features) {
    if (!is.null(config$feature_weights[[feat]])) {
      feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
      input_id <- paste0("weight_", feat_safe, "_", scenario_suffix)
      updateNumericInput(
        session,
        input_id,
        value = config$feature_weights[[feat]]
      )
    }
  }

  # ============================================================================
  # FIXED: Update structure type weights with proper name matching
  # ============================================================================
  # KEY INSIGHT: Data columns have SPACES ('commercial_type_Car Dealership')
  #              Config keys have UNDERSCORES ('commercial_type_Car_Dealership')
  #              We need to convert the data column name to match config format
  # ============================================================================

  all_structures <- c(commercial_type_columns, institutional_type_columns)

  for (struct in all_structures) {
    # Strategy 1: Try exact match first (for columns already using underscores)
    weight_value <- config$structure_type_weights[[struct]]

    # Strategy 2: Convert spaces to underscores in the data column name
    # This is the KEY FIX: 'commercial_type_Car Dealership' → 'commercial_type_Car_Dealership'
    if (is.null(weight_value)) {
      struct_normalized <- gsub(" ", "_", struct)
      weight_value <- config$structure_type_weights[[struct_normalized]]
    }

    # Strategy 3: Try reverse (for backward compatibility with old configs that have spaces)
    if (is.null(weight_value)) {
      struct_with_spaces <- gsub("_", " ", struct)
      weight_value <- config$structure_type_weights[[struct_with_spaces]]
    }

    # Apply if found
    if (!is.null(weight_value)) {
      # Sanitize struct name for use in input ID
      struct_safe <- gsub("[^A-Za-z0-9_]", "_", struct)
      struct_safe <- gsub("_+", "_", struct_safe) # Replace multiple underscores

      input_id <- paste0("weight_", struct_safe, "_", scenario_suffix)

      updateNumericInput(
        session,
        input_id,
        value = weight_value
      )
    }
  }
}
