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
  cat("\n========================================\n")
  cat("APPLYING MODULE 2 CONFIGURATION\n")
  cat("========================================\n")
  cat("Scenario:", scenario_suffix, "\n")
  cat("Timestamp:", as.character(config$timestamp), "\n\n")

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

  cat("✓ Base parameters updated\n")
  cat("  Base value:", config$base_value, "\n")
  cat("  Inflation:", config$inflation, "\n")
  cat("  Area weight:", config$area_weight, "\n\n")

  # Update all feature weights (including location_zones)
  all_features <- c(
    feature_columns$structure_features,
    feature_columns$utility_features,
    feature_columns$location_features,
    feature_columns$location_zones, # FIXED: Include location zones
    feature_columns$property_characteristics,
    ward_columns
  )

  cat("Feature weights to apply:", length(config$feature_weights), "\n")
  feature_applied <- 0
  feature_skipped <- 0

  for (feat in all_features) {
    if (!is.null(config$feature_weights[[feat]])) {
      feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
      input_id <- paste0("weight_", feat_safe, "_", scenario_suffix)
      updateNumericInput(
        session,
        input_id,
        value = config$feature_weights[[feat]]
      )
      feature_applied <- feature_applied + 1
    } else {
      feature_skipped <- feature_skipped + 1
    }
  }

  cat("✓ Feature weights updated:", feature_applied, "\n")
  if (feature_skipped > 0) {
    cat("  (Skipped", feature_skipped, "features not in config)\n")
  }

  # ============================================================================
  # DEBUGGED: Update structure type weights with detailed logging
  # ============================================================================
  cat("\n--- STRUCTURE TYPE WEIGHTS ---\n")

  all_structures <- c(commercial_type_columns, institutional_type_columns)

  cat("Commercial columns available:", length(commercial_type_columns), "\n")
  cat(
    "Institutional columns available:",
    length(institutional_type_columns),
    "\n"
  )
  cat("Total structure columns to update:", length(all_structures), "\n")
  cat(
    "Structure weights in config:",
    length(config$structure_type_weights),
    "\n\n"
  )

  # Debug: Show first few structure columns
  if (length(all_structures) > 0) {
    cat("First 5 structure columns from data:\n")
    for (i in 1:min(5, length(all_structures))) {
      cat(sprintf("  %d. '%s'\n", i, all_structures[i]))
    }
    cat("\n")
  } else {
    cat("⚠️  WARNING: No structure columns found!\n")
    cat(
      "   Check that commercial_type_columns and institutional_type_columns are populated\n\n"
    )
  }

  # Debug: Show first few config keys
  config_keys <- names(config$structure_type_weights)
  if (length(config_keys) > 0) {
    cat("First 5 structure weights from config:\n")
    for (i in 1:min(5, length(config_keys))) {
      cat(sprintf(
        "  %d. '%s' = %s\n",
        i,
        config_keys[i],
        config$structure_type_weights[[config_keys[i]]]
      ))
    }
    cat("\n")
  }

  applied_count <- 0
  skipped_count <- 0
  skipped_list <- character()

  # Special debug for Gym
  gym_in_columns <- "commercial_type_Gym" %in% all_structures
  gym_in_config <- !is.null(config$structure_type_weights[[
    "commercial_type_Gym"
  ]])

  cat("🔍 GYM DEBUG:\n")
  cat("  Gym in data columns?:", ifelse(gym_in_columns, "✓ YES", "✗ NO"), "\n")
  cat("  Gym in config?:", ifelse(gym_in_config, "✓ YES", "✗ NO"), "\n")
  if (gym_in_config) {
    cat(
      "  Gym value in config:",
      config$structure_type_weights[["commercial_type_Gym"]],
      "\n"
    )
  }
  cat("\n")

  for (struct in all_structures) {
    # Strategy 1: Try exact match
    weight_value <- config$structure_type_weights[[struct]]

    # Strategy 2: Try with spaces instead of underscores
    if (is.null(weight_value)) {
      struct_with_spaces <- gsub("_", " ", struct)
      weight_value <- config$structure_type_weights[[struct_with_spaces]]
    }

    # Strategy 3: Manual search with sanitization
    if (is.null(weight_value)) {
      for (config_key in config_keys) {
        config_key_sanitized <- gsub(" ", "_", config_key)
        if (config_key_sanitized == struct) {
          weight_value <- config$structure_type_weights[[config_key]]
          break
        }
      }
    }

    # Apply if found
    if (!is.null(weight_value)) {
      struct_safe <- gsub("[^A-Za-z0-9_]", "_", struct)
      input_id <- paste0("weight_", struct_safe, "_", scenario_suffix)

      # Debug specific cases
      if (struct == "commercial_type_Gym") {
        cat("🎯 UPDATING GYM:\n")
        cat("  Struct:", struct, "\n")
        cat("  Struct safe:", struct_safe, "\n")
        cat("  Input ID:", input_id, "\n")
        cat("  Value:", weight_value, "\n")
      }

      updateNumericInput(
        session,
        input_id,
        value = weight_value
      )

      applied_count <- applied_count + 1
    } else {
      skipped_count <- skipped_count + 1
      if (skipped_count <= 5) {
        skipped_list <- c(skipped_list, struct)
      }
    }
  }

  cat(
    "\n✓ Structure type weights applied:",
    applied_count,
    "/",
    length(all_structures),
    "\n"
  )

  if (skipped_count > 0) {
    cat("✗ Skipped", skipped_count, "structure types (not in config)\n")
    if (length(skipped_list) > 0) {
      cat("  First few skipped:\n")
      for (s in skipped_list) {
        cat(sprintf("    - '%s'\n", s))
      }
    }
  }

  cat("\n========================================\n")
  cat("CONFIGURATION APPLIED\n")
  cat("========================================\n\n")
}
