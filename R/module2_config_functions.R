# R/module2_config_functions.R
# Module 2: Configuration Save/Load Functions

#' Collect all Module 2 configuration from inputs
#' @param input Shiny input object
#' @param scenario_suffix Scenario identifier ("existing", "scenario_a", "scenario_b")
#' @param feature_columns List of categorized feature columns
#' @param commercial_type_columns Vector of commercial type column names
#' @param institutional_type_columns Vector of institutional type column names
#' @param ward_columns Vector of ward column names
#' @return Configuration list
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
    structure_weights = list()
  )

  # Collect all feature weights
  all_features <- c(
    feature_columns$structure_features,
    feature_columns$utility_features,
    feature_columns$location_features,
    feature_columns$location_zones,
    feature_columns$property_characteristics,
    ward_columns
  )

  for (feat in all_features) {
    feat_safe <- gsub("[^A-Za-z0-9_]", "_", feat)
    input_id <- paste0("weight_", feat_safe, "_", scenario_suffix)
    if (!is.null(input[[input_id]])) {
      config$feature_weights[[feat]] <- input[[input_id]]
    }
  }

  # Collect structure type weights
  all_structures <- c(commercial_type_columns, institutional_type_columns)

  for (struct in all_structures) {
    struct_safe <- gsub("[^A-Za-z0-9_]", "_", struct)
    input_id <- paste0("weight_", struct_safe, "_", scenario_suffix)
    if (!is.null(input[[input_id]])) {
      # Store with both original name and safe name for compatibility
      config$structure_weights[[struct]] <- input[[input_id]]
      config$structure_weights[[struct_safe]] <- input[[input_id]]
    }
  }

  return(config)
}

#' Load Module 2 configuration from file
#' @param file_path Path to JSON configuration file
#' @return Configuration list
load_module2_config <- function(file_path) {
  tryCatch(
    {
      config_json <- readLines(file_path, warn = FALSE)
      config <- jsonlite::fromJSON(
        paste(config_json, collapse = "\n"),
        simplifyVector = FALSE
      )

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

#' Apply loaded configuration to Module 2 inputs
#' @param session Shiny session object
#' @param config Configuration list from load_module2_config
#' @param scenario_suffix Scenario to apply to
#' @param feature_columns List of categorized feature columns
#' @param commercial_type_columns Vector of commercial type column names
#' @param institutional_type_columns Vector of institutional type column names
#' @param ward_columns Vector of ward column names
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

  # Update feature weights
  all_features <- c(
    feature_columns$structure_features,
    feature_columns$utility_features,
    feature_columns$location_features,
    feature_columns$location_zones,
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

  # Update structure type weights
  all_structures <- c(commercial_type_columns, institutional_type_columns)

  for (struct in all_structures) {
    # Try to find the weight in config (could be stored with original or safe name)
    struct_safe <- gsub("[^A-Za-z0-9_]", "_", struct)

    weight_value <- NULL
    if (!is.null(config$structure_weights[[struct]])) {
      weight_value <- config$structure_weights[[struct]]
    } else if (!is.null(config$structure_weights[[struct_safe]])) {
      weight_value <- config$structure_weights[[struct_safe]]
    }

    if (!is.null(weight_value)) {
      input_id <- paste0("weight_", struct_safe, "_", scenario_suffix)
      updateNumericInput(
        session,
        input_id,
        value = weight_value
      )
    }
  }
}
