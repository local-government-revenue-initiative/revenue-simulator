# ==============================================================================
# MODULE 2: Configuration Save/Load Functions - COMPLETE FIXED VERSION
# ==============================================================================
# This file contains BOTH fixes:
# 1. location_zones included in collect function
# 2. Flexible name matching in apply function
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
    # Ensure struct name uses underscores, not spaces
    struct_clean <- gsub(" ", "_", struct)

    struct_safe <- gsub("[^A-Za-z0-9_]", "_", struct_clean)
    input_id <- paste0("weight_", struct_safe, "_", scenario_suffix)
    weight_value <- input[[input_id]]

    if (!is.null(weight_value)) {
      # Save with cleaned name (underscores)
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

# FIXED: Function to apply loaded configuration to UI inputs with flexible matching
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
    feature_columns$location_zones, # Include location zones
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
  # FIXED: Update structure type weights with flexible name matching
  # ============================================================================
  all_structures <- c(commercial_type_columns, institutional_type_columns)

  # Track how many weights we successfully apply
  applied_count <- 0
  missing_count <- 0
  missing_structures <- character()

  for (struct in all_structures) {
    # Strategy 1: Try exact match with column name (e.g., "commercial_type_Bank")
    weight_value <- config$structure_type_weights[[struct]]

    # Strategy 2: If not found, try with spaces instead of underscores
    # (e.g., try "commercial_type Car Dealership" for "commercial_type_Car_Dealership")
    if (is.null(weight_value)) {
      struct_with_spaces <- gsub("_", " ", struct)
      weight_value <- config$structure_type_weights[[struct_with_spaces]]
    }

    # Strategy 3: If still not found, manually search config keys by converting
    # spaces to underscores (handles older JSON files with inconsistent naming)
    if (is.null(weight_value)) {
      for (config_key in names(config$structure_type_weights)) {
        # Sanitize the config key by replacing spaces with underscores
        config_key_sanitized <- gsub(" ", "_", config_key)

        # Check if this matches our column name
        if (config_key_sanitized == struct) {
          weight_value <- config$structure_type_weights[[config_key]]
          break
        }
      }
    }

    # If we successfully found a weight value, update the corresponding UI input
    if (!is.null(weight_value)) {
      # Sanitize struct name for use in input ID (replace special chars with _)
      struct_safe <- gsub("[^A-Za-z0-9_]", "_", struct)
      input_id <- paste0("weight_", struct_safe, "_", scenario_suffix)

      # Update the numeric input in the UI
      updateNumericInput(
        session,
        input_id,
        value = weight_value
      )

      applied_count <- applied_count + 1
    } else {
      # Track structures that couldn't be matched (for debugging)
      missing_count <- missing_count + 1
      missing_structures <- c(missing_structures, struct)
    }
  }

  # Log summary of what was applied
  message(paste("Configuration applied for scenario:", scenario_suffix))
  message(paste("  - Base value:", config$base_value))
  message(paste(
    "  - Feature weights available:",
    length(config$feature_weights)
  ))
  message(paste(
    "  - Structure type weights applied:",
    applied_count,
    "/",
    length(all_structures)
  ))

  if (missing_count > 0 && missing_count <= 5) {
    message(paste(
      "  - Missing structures:",
      paste(missing_structures, collapse = ", ")
    ))
  } else if (missing_count > 5) {
    message(paste(
      "  - Missing structures:",
      missing_count,
      "(first 5:",
      paste(head(missing_structures, 5), collapse = ", "),
      "...)"
    ))
  }
}
