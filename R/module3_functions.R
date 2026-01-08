# R/module3_functions.R
# Module 3 Functions - Updated to use parameter tables for defaults

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
    subcategory <- row$business_sub_category
    method <- row$license_calculation_method

    # Map method names from CSV to internal names
    internal_method <- switch(
      method,
      "minimum_and_rate" = "minimum_rate",
      "flat_amount_fixed" = "flat",
      "minimum_rate" # Default
    )

    config[[subcategory]] <- list(
      category = row$business_category,
      calculation_method = internal_method,
      minimum = if (is.na(row$license_minimum)) 0 else row$license_minimum,
      rate = if (is.na(row$license_rate)) 0 else row$license_rate,
      flat_amount = if (is.na(row$license_flat_amount)) {
        0
      } else {
        row$license_flat_amount
      }
    )
  }

  # Add default fallback
  config$default <- list(
    calculation_method = "minimum_rate",
    minimum = 350,
    rate = 0.035,
    flat_amount = 0
  )

  return(config)
}

#' Get unique business subcategories from param_license
#' @param param_license Data frame with license parameters
#' @return Vector of unique subcategory names
get_license_subcategories <- function(param_license) {
  if (is.null(param_license) || nrow(param_license) == 0) {
    return(character())
  }
  unique(param_license$business_sub_category)
}

#' Get license defaults for a specific subcategory
#' @param subcategory Subcategory name
#' @param license_defaults List of license defaults from build_business_license_defaults
#' @return List with minimum, rate, flat_amount, calculation_method
get_subcategory_defaults <- function(
  subcategory,
  license_defaults = NULL,
  category = NULL
) {
  # Default values
  defaults <- list(
    minimum = 350,
    rate = 3.5, # Percentage for display
    flat_amount = 0,
    calculation_method = "minimum_rate"
  )

  if (is.null(license_defaults)) {
    return(defaults)
  }

  # Try to find subcategory in defaults
  if (subcategory %in% names(license_defaults)) {
    subcat_config <- license_defaults[[subcategory]]
    return(list(
      minimum = subcat_config$minimum %||% 0,
      rate = (subcat_config$rate %||% 0) * 100, # Convert to percentage
      flat_amount = subcat_config$flat_amount %||% 0,
      calculation_method = subcat_config$calculation_method %||% "minimum_rate"
    ))
  }

  # Try composite key with category
  if (!is.null(category)) {
    composite_key <- paste0(category, "::", subcategory)
    if (composite_key %in% names(license_defaults)) {
      subcat_config <- license_defaults[[composite_key]]
      return(list(
        minimum = subcat_config$minimum %||% 0,
        rate = (subcat_config$rate %||% 0) * 100,
        flat_amount = subcat_config$flat_amount %||% 0,
        calculation_method = subcat_config$calculation_method %||%
          "minimum_rate"
      ))
    }
  }

  # Return default
  return(defaults)
}

# ==============================================================================
# LEGACY FUNCTION - Get default tax config (backward compatibility)
# ==============================================================================

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
#' @param property_value Numeric property value
#' @param property_type Property type (domestic, commercial, institutional)
#' @param tax_config Tax configuration list
#' @return List with tax_amount, rate_used, minimum_used, slot_used
calculate_property_tax <- function(property_value, property_type, tax_config) {
  # Defensive checks
  if (
    length(property_value) == 0 ||
      is.null(property_value) ||
      is.na(property_value)
  ) {
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
#' @param business_value Numeric business value
#' @param business_area Numeric business area
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

  if (is.na(business_value) || business_value <= 0) {
    return(default_result)
  }

  if (is.na(business_subcategory)) {
    return(default_result)
  }

  # Get configuration for this subcategory
  subcat_config <- license_config[[business_subcategory]]

  # Try composite key if not found
  if (is.null(subcat_config) && !is.null(business_category)) {
    composite_key <- paste0(business_category, "::", business_subcategory)
    subcat_config <- license_config[[composite_key]]
  }

  # Use default if still not found
  if (is.null(subcat_config)) {
    subcat_config <- license_config$default %||%
      list(
        calculation_method = "minimum_rate",
        minimum = 350,
        rate = 0.035,
        flat_amount = 0
      )
  }

  # Calculate based on method
  method <- subcat_config$calculation_method %||% "minimum_rate"

  if (method == "minimum_rate") {
    tax_amount <- max(
      business_value * (subcat_config$rate %||% 0),
      subcat_config$minimum %||% 0
    )
  } else if (method == "flat") {
    tax_amount <- subcat_config$flat_amount %||% 0
  } else if (method == "flat_value_bands") {
    # Flat amount based on business value bands
    bands <- subcat_config$value_bands
    tax_amount <- bands$band3$tax %||% 0 # Default to highest band

    if (!is.null(bands$band1) && business_value <= bands$band1$max) {
      tax_amount <- bands$band1$tax
    } else if (!is.null(bands$band2) && business_value <= bands$band2$max) {
      tax_amount <- bands$band2$tax
    }
  } else if (method == "flat_area_bands") {
    # Flat amount based on business area bands
    bands <- subcat_config$area_bands
    tax_amount <- bands$band3$tax %||% 0

    if (
      !is.null(bands$band1) &&
        !is.na(business_area) &&
        business_area <= bands$band1$max
    ) {
      tax_amount <- bands$band1$tax
    } else if (
      !is.null(bands$band2) &&
        !is.na(business_area) &&
        business_area <= bands$band2$max
    ) {
      tax_amount <- bands$band2$tax
    }
  } else {
    # Fallback
    tax_amount <- max(business_value * 0.035, 350)
  }

  return(list(
    license_amount = tax_amount,
    method_used = method,
    rate_used = subcat_config$rate %||% 0,
    minimum_used = subcat_config$minimum %||% 0,
    flat_amount_used = subcat_config$flat_amount %||% 0
  ))
}
