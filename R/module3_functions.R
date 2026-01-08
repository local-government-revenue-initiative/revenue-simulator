# R/module3_functions.R
# Module 3 Functions - Updated to use parameter tables for defaults
# Fixed defensive checks to handle vectors properly

# ==============================================================================
# BUILD DEFAULTS FROM PARAMETER TABLES
# ==============================================================================

#' Build default property tax configuration from param_tax_min_rate table
#' @param param_tax_min_rate Data frame with columns: property_type, prop_tax_rate, min_prop_tax
#' @return List with tax config by property type
build_property_tax_defaults <- function(param_tax_min_rate) {
  if (is.null(param_tax_min_rate) || nrow(param_tax_min_rate) == 0) {
    # Return hardcoded defaults if no parameter table
    return(list(
      domestic = list(use_slots = FALSE, minimum = 200, rate = 0.025),
      commercial = list(use_slots = FALSE, minimum = 200, rate = 0.04),
      institutional = list(use_slots = FALSE, minimum = 200, rate = 0.025)
    ))
  }

  config <- list()

  for (i in seq_len(nrow(param_tax_min_rate))) {
    row <- param_tax_min_rate[i, ]
    prop_type <- tolower(row$property_type)

    config[[prop_type]] <- list(
      use_slots = FALSE,
      minimum = row$min_prop_tax,
      rate = row$prop_tax_rate # Already in decimal form (0.04 = 4%)
    )
  }

  # Ensure all three property types exist
  if (!"domestic" %in% names(config)) {
    config$domestic <- list(use_slots = FALSE, minimum = 200, rate = 0.025)
  }
  if (!"commercial" %in% names(config)) {
    config$commercial <- list(use_slots = FALSE, minimum = 200, rate = 0.04)
  }
  if (!"institutional" %in% names(config)) {
    config$institutional <- list(use_slots = FALSE, minimum = 200, rate = 0.025)
  }

  return(config)
}

#' Build default business license configuration from param_license table
#' @param param_license Data frame with columns: business_category, business_sub_category,
#'   license_calculation_method, license_minimum, license_rate, license_flat_amount
#' @return List with license config by subcategory
build_business_license_defaults <- function(param_license) {
  if (is.null(param_license) || nrow(param_license) == 0) {
    # Return minimal default if no parameter table
    return(list(
      default = list(
        calculation_method = "minimum_rate",
        minimum = 350,
        rate = 0.035,
        flat_amount = 0
      )
    ))
  }

  config <- list()

  for (i in seq_len(nrow(param_license))) {
    row <- param_license[i, ]

    # Use subcategory as key
    subcat <- as.character(row$business_sub_category)

    # Map calculation method
    calc_method <- tolower(as.character(row$license_calculation_method))
    if (calc_method %in% c("minimum and rate", "minimum_rate")) {
      calc_method <- "minimum_rate"
    } else if (calc_method == "flat") {
      calc_method <- "flat"
    } else {
      calc_method <- "minimum_rate" # Default fallback
    }

    config[[subcat]] <- list(
      calculation_method = calc_method,
      minimum = as.numeric(row$license_minimum) %||% 350,
      rate = as.numeric(row$license_rate) %||% 0.035,
      flat_amount = as.numeric(row$license_flat_amount) %||% 0,
      category = as.character(row$business_category)
    )
  }

  return(config)
}

#' Get list of business subcategories from param_license
#' @param param_license Data frame with license parameters
#' @return Character vector of unique subcategories
get_license_subcategories <- function(param_license) {
  if (is.null(param_license) || !"business_sub_category" %in% names(param_license)) {
    return(character(0))
  }
  unique(as.character(param_license$business_sub_category))
}

#' Get default tax configuration - legacy function
#' Use build_property_tax_defaults and build_business_license_defaults instead
get_default_tax_config <- function() {
  list(
    property_tax = list(
      domestic = list(use_slots = FALSE, minimum = 200, rate = 0.025),
      commercial = list(use_slots = FALSE, minimum = 200, rate = 0.04),
      institutional = list(use_slots = FALSE, minimum = 200, rate = 0.025)
    ),
    business_license = list(
      default_subcategory = list(
        calculation_method = "minimum_rate",
        minimum = 350,
        rate = 0.035,
        flat_amount = 0
      )
    )
  )
}

# ==============================================================================
# TAX CALCULATION FUNCTIONS
# ==============================================================================

#' Calculate property tax for a single property
#' @param property_value Numeric property value (scalar)
#' @param property_type Property type (domestic, commercial, institutional)
#' @param tax_config Tax configuration list
#' @return List with tax_amount, rate_used, minimum_used, slot_used
calculate_property_tax <- function(property_value, property_type, tax_config) {
  # FIXED: Defensive checks using any() to handle potential vector inputs
  # This ensures the || operator receives scalar logical values
  if (
    length(property_value) == 0 ||
      is.null(property_value) ||
      all(is.na(property_value))
  ) {
    return(list(
      tax_amount = 0,
      rate_used = 0,
      minimum_used = 0,
      slot_used = NA
    ))
  }

  # If somehow a vector was passed, use only the first element
  if (length(property_value) > 1) {
    warning("calculate_property_tax received a vector; using first element only")
    property_value <- property_value[1]
  }

  # Handle NA in the single value case
  if (is.na(property_value)) {
    return(list(
      tax_amount = 0,
      rate_used = 0,
      minimum_used = 0,
      slot_used = NA
    ))
  }

  if (length(property_type) == 0 || is.null(property_type)) {
    property_type <- "domestic"
  }

  # If property_type is a vector, take first element
  if (length(property_type) > 1) {
    property_type <- property_type[1]
  }

  # Handle NA property type
  if (is.na(property_type)) {
    property_type <- "domestic"
  }

  # Get configuration for this property type
  type_config <- tax_config[[tolower(property_type)]]

  if (is.null(type_config)) {
    type_config <- tax_config$domestic %||%
      list(
        use_slots = FALSE,
        minimum = 200,
        rate = 0.025
      )
  }

  # Calculate tax
  if (!isTRUE(type_config$use_slots)) {
    # Simple calculation: max(value * rate, minimum)
    tax_amount <- max(property_value * type_config$rate, type_config$minimum)
    return(list(
      tax_amount = tax_amount,
      rate_used = type_config$rate,
      minimum_used = type_config$minimum,
      slot_used = NA
    ))
  } else {
    # Slot-based calculation
    slot_num <- 3 # Default to highest slot
    for (s in 1:3) {
      slot <- type_config$slots[[paste0("slot", s)]]
      if (
        !is.null(slot) &&
          property_value >= slot$min &&
          property_value < slot$max
      ) {
        slot_num <- s
        break
      }
    }

    slot_config <- type_config$slots[[paste0("slot", slot_num)]]
    if (is.null(slot_config)) {
      # Fallback if slot config is missing
      slot_config <- list(rate = type_config$rate %||% 0.025, minimum = type_config$minimum %||% 200)
    }
    tax_amount <- max(property_value * slot_config$rate, slot_config$minimum)

    return(list(
      tax_amount = tax_amount,
      rate_used = slot_config$rate,
      minimum_used = slot_config$minimum,
      slot_used = slot_num
    ))
  }
}

#' Calculate business license for a single business
#' @param business_value Numeric business value (scalar)
#' @param business_area Numeric business area (scalar)
#' @param business_subcategory Subcategory name
#' @param license_config License configuration list
#' @param business_category Optional category name
#' @return List with license_amount, method_used, rate_used, minimum_used, flat_amount_used
calculate_business_license <- function(
  business_value,
  business_area,
  business_subcategory,
  license_config,
  business_category = NULL
) {
  # Default return
  default_result <- list(
    license_amount = 0,
    method_used = "none",
    rate_used = 0,
    minimum_used = 0,
    flat_amount_used = 0
  )

  # FIXED: Handle vector inputs safely
  if (length(business_value) == 0 || is.null(business_value)) {
    return(default_result)
  }

  if (length(business_value) > 1) {
    business_value <- business_value[1]
  }

  if (is.na(business_value) || business_value <= 0) {
    return(default_result)
  }

  if (length(business_subcategory) == 0 || is.null(business_subcategory)) {
    return(default_result)
  }

  if (length(business_subcategory) > 1) {
    business_subcategory <- business_subcategory[1]
  }

  if (is.na(business_subcategory)) {
    return(default_result)
  }

  # Handle business_area if provided
  if (length(business_area) > 1) {
    business_area <- business_area[1]
  }

  # Get configuration for this subcategory
  subcat_config <- license_config[[business_subcategory]]

  # Try composite key if not found
  if (is.null(subcat_config) && !is.null(business_category)) {
    if (length(business_category) > 1) {
      business_category <- business_category[1]
    }
    composite_key <- paste0(business_category, "_", business_subcategory)
    subcat_config <- license_config[[composite_key]]
  }

  # Use default if still not found
  if (is.null(subcat_config)) {
    subcat_config <- license_config[["default"]] %||% list(
      calculation_method = "minimum_rate",
      minimum = 350,
      rate = 0.035,
      flat_amount = 0
    )
  }

  # Calculate license based on method
  method <- subcat_config$calculation_method %||% "minimum_rate"

  if (method == "flat") {
    return(list(
      license_amount = subcat_config$flat_amount %||% 0,
      method_used = "flat",
      rate_used = 0,
      minimum_used = 0,
      flat_amount_used = subcat_config$flat_amount %||% 0
    ))
  } else if (method == "flat_value_bands") {
    # Value-based bands
    bands <- subcat_config$value_bands
    if (is.null(bands)) {
      # Fallback to minimum_rate if bands not configured
      license_amount <- max(
        business_value * (subcat_config$rate %||% 0.035),
        subcat_config$minimum %||% 350
      )
      return(list(
        license_amount = license_amount,
        method_used = "minimum_rate",
        rate_used = subcat_config$rate %||% 0.035,
        minimum_used = subcat_config$minimum %||% 350,
        flat_amount_used = 0
      ))
    }

    # Determine which band applies
    if (business_value <= (bands$band1$max %||% 25000)) {
      license_amount <- bands$band1$tax %||% 300
    } else if (business_value <= (bands$band2$max %||% 50000)) {
      license_amount <- bands$band2$tax %||% 500
    } else {
      license_amount <- bands$band3$tax %||% 1000
    }

    return(list(
      license_amount = license_amount,
      method_used = "flat_value_bands",
      rate_used = 0,
      minimum_used = 0,
      flat_amount_used = license_amount
    ))
  } else if (method == "flat_area_bands") {
    # Area-based bands
    bands <- subcat_config$area_bands
    if (is.null(bands) || is.na(business_area) || business_area <= 0) {
      # Fallback to minimum_rate
      license_amount <- max(
        business_value * (subcat_config$rate %||% 0.035),
        subcat_config$minimum %||% 350
      )
      return(list(
        license_amount = license_amount,
        method_used = "minimum_rate",
        rate_used = subcat_config$rate %||% 0.035,
        minimum_used = subcat_config$minimum %||% 350,
        flat_amount_used = 0
      ))
    }

    # Determine which band applies
    if (business_area <= (bands$band1$max %||% 50)) {
      license_amount <- bands$band1$tax %||% 300
    } else if (business_area <= (bands$band2$max %||% 200)) {
      license_amount <- bands$band2$tax %||% 1000
    } else {
      license_amount <- bands$band3$tax %||% 2000
    }

    return(list(
      license_amount = license_amount,
      method_used = "flat_area_bands",
      rate_used = 0,
      minimum_used = 0,
      flat_amount_used = license_amount
    ))
  } else {
    # Default: minimum_rate method
    license_amount <- max(
      business_value * (subcat_config$rate %||% 0.035),
      subcat_config$minimum %||% 350
    )

    return(list(
      license_amount = license_amount,
      method_used = "minimum_rate",
      rate_used = subcat_config$rate %||% 0.035,
      minimum_used = subcat_config$minimum %||% 350,
      flat_amount_used = 0
    ))
  }
}

# ==============================================================================
# VECTORIZED TAX CALCULATION FUNCTIONS (for batch processing)
# ==============================================================================

#' Calculate property taxes for multiple properties (vectorized)
#' @param property_values Numeric vector of property values
#' @param property_types Character vector of property types
#' @param tax_config Tax configuration list
#' @return Numeric vector of tax amounts
calculate_property_taxes_vectorized <- function(property_values, property_types, tax_config) {
  n <- length(property_values)

  if (n == 0) {
    return(numeric(0))
  }

  # Ensure property_types has same length
  if (length(property_types) != n) {
    property_types <- rep(property_types[1], n)
  }

  # Pre-allocate result vector
  tax_amounts <- numeric(n)

  # Process each property
  for (i in seq_len(n)) {
    result <- calculate_property_tax(
      property_values[i],
      property_types[i],
      tax_config
    )
    tax_amounts[i] <- result$tax_amount
  }

  return(tax_amounts)
}

#' Calculate business licenses for Module 3 (vectorized)
#' @param business_values Numeric vector of business values
#' @param business_areas Numeric vector of business areas
#' @param business_subcategories Character vector of subcategories
#' @param license_config License configuration list
#' @return Numeric vector of license amounts
calculate_business_licenses_module3 <- function(
  business_values,
  business_areas,
  business_subcategories,
  license_config
) {
  n <- length(business_values)

  if (n == 0) {
    return(numeric(0))
  }

  # Ensure all vectors have same length
  if (length(business_areas) != n) {
    business_areas <- rep(NA, n)
  }
  if (length(business_subcategories) != n) {
    business_subcategories <- rep(NA, n)
  }

  # Pre-allocate result vector
  license_amounts <- numeric(n)

  # Process each business
  for (i in seq_len(n)) {
    result <- calculate_business_license(
      business_values[i],
      business_areas[i],
      business_subcategories[i],
      license_config
    )
    license_amounts[i] <- result$license_amount
  }

  return(license_amounts)
}

# ==============================================================================
# BUSINESS VALUE CALCULATION FUNCTION (for Module 3 Preview)
# This is a standalone version that can be used without Module 4
# ==============================================================================

#' Calculate business values using Module 2 configuration
#' @param data Data frame with property/business data
#' @param config Module 2 configuration list with base_value, inflation, area_weight,
#'   feature_weights, and structure_weights
#' @return Numeric vector of business values
calculate_business_values_module2 <- function(data, config) {
  n_rows <- nrow(data)

  # Get business areas
  business_areas <- if ("business_area" %in% names(data)) {
    data$business_area
  } else {
    rep(NA, n_rows)
  }

  # Only calculate for properties with businesses
  if (all(is.na(business_areas)) || all(business_areas <= 0, na.rm = TRUE)) {
    return(rep(0, n_rows))
  }

  # Get configuration values with defaults
  base_value <- config$base_value %||% 0.5
  inflation <- config$inflation %||% 0
  area_weight <- config$area_weight %||% 0.5

  # Calculate inflation-adjusted base value
  inflation_adjusted_base <- base_value * (1 + inflation)

  # Calculate feature weights product
  all_features <- names(config$feature_weights)
  product_weights <- rep(1, n_rows)

  if (!is.null(all_features) && length(all_features) > 0) {
    for (feat in all_features) {
      if (feat %in% names(data)) {
        weight <- config$feature_weights[[feat]]
        if (!is.null(weight) && !is.na(weight)) {
          feature_multiplier <- ifelse(
            data[[feat]] == 1,
            (weight / 100 + 1),
            1
          )
          product_weights <- product_weights * feature_multiplier
        }
      }
    }
  }

  # Calculate structure type weights using matrix approach
  all_structures <- names(config$structure_weights)
  structure_multipliers <- rep(1, n_rows)

  if (!is.null(all_structures) && length(all_structures) > 0) {
    structure_matrix <- matrix(0, nrow = n_rows, ncol = length(all_structures))
    weight_vector <- numeric(length(all_structures))

    for (j in seq_along(all_structures)) {
      struct <- all_structures[j]

      if (struct %in% names(data)) {
        col_values <- data[[struct]]
        structure_matrix[, j] <- ifelse(
          !is.na(col_values) & col_values == 1,
          1,
          0
        )

        weight <- config$structure_weights[[struct]]
        if (is.null(weight) || is.na(weight)) {
          weight <- 0
        }
        weight_vector[j] <- weight
      }
    }

    # Calculate structure weights using matrix multiplication
    structure_weights <- structure_matrix %*% weight_vector
    structure_weights <- as.vector(structure_weights)

    # Apply structure type weights as multipliers
    structure_multipliers <- (structure_weights / 100 + 1)
  }

  # Calculate business values
  business_values <- ifelse(
    is.na(business_areas) | business_areas <= 0,
    0,
    inflation_adjusted_base *
      (business_areas^area_weight) *
      product_weights *
      structure_multipliers
  )

  return(business_values)
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Get property types from one-hot encoded columns
#' @param data Data frame with property_type_* columns
#' @return Character vector of property types
get_property_types <- function(data) {
  n_rows <- nrow(data)
  property_types <- rep("domestic", n_rows) # Default

  if ("property_type_Commercial" %in% names(data)) {
    property_types[data$property_type_Commercial == 1] <- "commercial"
  }

  if ("property_type_Institutional" %in% names(data)) {
    property_types[data$property_type_Institutional == 1] <- "institutional"
  }

  return(property_types)
}

#' Calculate property taxes with deduplication for multiple property types
#' @param data Data frame with property data
#' @param property_values Numeric vector of property values
#' @param property_types Character vector of property types
#' @param tax_config Tax configuration list
#' @return Numeric vector of property taxes (deduplicated per property)
calculate_property_taxes_with_deduplication <- function(
  data,
  property_values,
  property_types,
  tax_config
) {
  n_rows <- nrow(data)

  # Calculate taxes for each row
  property_taxes <- calculate_property_taxes_vectorized(
    property_values,
    property_types,
    tax_config
  )

  # If data has id_property, we need to handle deduplication
  # Properties might appear multiple times if they have multiple types
  if ("id_property" %in% names(data)) {
    # For now, just return the taxes - deduplication happens in Module 4
    # when aggregating results
  }

  return(property_taxes)
}
