# R/module1_functions.R
# Simplified Module 1 Functions
# Most data processing is now done outside the app during data preparation

# Function to safely load RDS file with error handling
load_city_data <- function(city_name, data_dir = "data") {
  # Construct file path
  file_path <- file.path(data_dir, paste0(city_name, "_data.rds"))

  # Check if file exists
  if (!file.exists(file_path)) {
    return(list(
      success = FALSE,
      error = paste("Data file not found:", file_path),
      data = NULL
    ))
  }

  # Try to load the file
  tryCatch(
    {
      data_bundle <- readRDS(file_path)

      return(list(
        success = TRUE,
        error = NULL,
        data = data_bundle
      ))
    },
    error = function(e) {
      return(list(
        success = FALSE,
        error = paste("Error reading data file:", e$message),
        data = NULL
      ))
    }
  )
}

# Function to validate the structure of loaded data bundle
validate_data_bundle <- function(data_bundle) {
  errors <- character()
  warnings <- character()

  # Required components
  required_components <- c(
    "param_additions",
    "param_features",
    "param_prop_struct_type",
    "param_tax_min_rate",
    "param_license"
  )

  # Check for required parameter tables
  for (component in required_components) {
    if (!component %in% names(data_bundle)) {
      errors <- c(errors, paste("Missing required component:", component))
    }
  }

  # Check for combined data (could have different names)
  combined_data_names <- c(
    "combined_data",
    "combined_data_sample_rows",
    "data"
  )

  has_combined_data <- any(combined_data_names %in% names(data_bundle))
  if (!has_combined_data) {
    # Check if first element is a data frame
    if (length(data_bundle) > 0 && is.data.frame(data_bundle[[1]])) {
      warnings <- c(warnings, "Combined data found as unnamed first element")
    } else {
      errors <- c(errors, "Could not find combined data in bundle")
    }
  }

  # If combined data exists, validate its structure
  combined_data <- NULL
  for (name in combined_data_names) {
    if (name %in% names(data_bundle)) {
      combined_data <- data_bundle[[name]]
      break
    }
  }

  if (is.null(combined_data) && length(data_bundle) > 0) {
    if (is.data.frame(data_bundle[[1]])) {
      combined_data <- data_bundle[[1]]
    }
  }

  if (!is.null(combined_data)) {
    # Check for required columns in combined data
    required_columns <- c(
      "id_property",
      "property_type",
      "property_area"
    )

    missing_cols <- required_columns[
      !required_columns %in% names(combined_data)
    ]
    if (length(missing_cols) > 0) {
      errors <- c(
        errors,
        paste(
          "Missing required columns in combined data:",
          paste(missing_cols, collapse = ", ")
        )
      )
    }
  }

  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  ))
}

# Function to get summary statistics from combined data
get_data_summary <- function(combined_data) {
  if (is.null(combined_data) || !is.data.frame(combined_data)) {
    return(NULL)
  }

  summary_stats <- list(
    total_rows = nrow(combined_data),
    unique_properties = length(unique(combined_data$id_property)),
    unique_businesses = if ("id_business" %in% names(combined_data)) {
      sum(!is.na(combined_data$id_business))
    } else {
      0
    },
    property_types = if ("property_type" %in% names(combined_data)) {
      table(combined_data$property_type)
    } else {
      NULL
    },
    payment_status = if ("made_payment" %in% names(combined_data)) {
      table(combined_data$made_payment)
    } else {
      NULL
    }
  )

  return(summary_stats)
}

# Function to extract combined data from bundle with flexible naming
extract_combined_data <- function(data_bundle) {
  # Try different possible names for the combined data
  possible_names <- c(
    "combined_data",
    "combined_data_sample_rows",
    "freetown_data_all",
    "kenema_data_all",
    "makeni_data_all",
    "data"
  )

  for (name in possible_names) {
    if (name %in% names(data_bundle)) {
      return(data_bundle[[name]])
    }
  }

  # If no named match, try the first element
  if (length(data_bundle) > 0 && is.data.frame(data_bundle[[1]])) {
    return(data_bundle[[1]])
  }

  return(NULL)
}
