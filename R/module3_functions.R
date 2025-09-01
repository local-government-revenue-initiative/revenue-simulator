# R/module3_functions.R

# Function to get default tax configurations
get_default_tax_config <- function() {
  list(
    # Property tax defaults
    property_tax = list(
      # Global band definitions
      bands = list(
        band1 = list(min = 0, max = 350),
        band2 = list(min = 350, max = 700),
        band3 = list(min = 700, max = Inf)
      ),
      # Tax rates by property type
      domestic = list(
        use_bands = FALSE,
        minimum = 200,        # Changed from 350 to 200
        rate = 0.025,         # Changed from 0.5 to 0.025 (2.5%)
        band_rates = list(rate1 = 0.02, rate2 = 0.025, rate3 = 0.03),
        band_minimums = list(min1 = 100, min2 = 200, min3 = 300)
      ),
      commercial = list(
        use_bands = FALSE,
        minimum = 200,        # Changed from 200000 to 200
        rate = 0.04,          # Changed from 0.25 to 0.04 (4%)
        band_rates = list(rate1 = 0.03, rate2 = 0.04, rate3 = 0.05),
        band_minimums = list(min1 = 150, min2 = 200, min3 = 400)
      ),
      institutional = list(
        use_bands = FALSE,
        minimum = 200,        # Changed from 150000 to 200
        rate = 0.025,         # Changed from 0.20 to 0.025 (2.5%)
        band_rates = list(rate1 = 0.02, rate2 = 0.025, rate3 = 0.03),
        band_minimums = list(min1 = 100, min2 = 200, min3 = 300)
      )
    ),
    
    # Business license defaults
    business_license = list(
      calculation_method = "minimum_rate",  # Global default
      # Band definitions
      value_bands = list(
        band1 = list(min = 0, max = 1000000),
        band2 = list(min = 1000000, max = 5000000),
        band3 = list(min = 5000000, max = Inf)
      ),
      area_bands = list(
        band1 = list(min = 0, max = 100),
        band2 = list(min = 100, max = 500),
        band3 = list(min = 500, max = Inf)
      ),
      # Default configuration for subcategories
      default_subcategory = list(
        calculation_method = "default",  # Use global default
        use_bands = FALSE,
        minimum = 350,        # Changed from 50000 to 350
        rate = 0.05,          # Changed from 0.10 to 0.05 (5%)
        flat_amount = 350,    # Changed from 50000 to 350
        flat_amounts_value = list(band1 = 350, band2 = 700, band3 = 1000),
        flat_amounts_area = list(band1 = 350, band2 = 500, band3 = 750)
      )
    )
  )
}

# Function to calculate property tax with individual property type bands
calculate_property_tax <- function(property_value, property_type, tax_config) {
  # Get the configuration for this property type
  type_config <- tax_config$property_tax[[tolower(property_type)]]
  
  if (is.null(type_config)) {
    # Default to domestic if type not found
    type_config <- tax_config$property_tax$domestic
  }
  
  if (type_config$use_bands) {
    # Find which band this property falls into
    band_num <- 1
    for (i in 1:3) {
      band <- tax_config$property_tax$bands[[paste0("band", i)]]
      if (property_value >= band$min && property_value < band$max) {
        band_num <- i
        break
      }
    }
    
    # Get band-specific rate and minimum
    rate <- type_config$band_rates[[paste0("rate", band_num)]]
    minimum <- type_config$band_minimums[[paste0("min", band_num)]]
  } else {
    # Use flat rate and minimum
    rate <- type_config$rate
    minimum <- type_config$minimum
  }
  
  # Calculate tax: max of (value * rate) or minimum
  tax_amount <- max(property_value * (rate / 100), minimum)
  
  return(list(
    tax_amount = tax_amount,
    rate_used = rate,
    minimum_used = minimum,
    band_used = if(type_config$use_bands) band_num else NA
  ))
}

# Function to calculate business license with subcategory-specific configurations
calculate_business_license <- function(business_value, business_area, business_subcategory, tax_config) {
  # Get the configuration for this business subcategory
  subcat_config <- tax_config$business_license$subcategories[[business_subcategory]]
  
  if (is.null(subcat_config)) {
    # Use default configuration if subcategory not found
    subcat_config <- tax_config$business_license$default_subcategory
  }
  
  # Determine calculation method
  calc_method <- if (subcat_config$calculation_method == "default") {
    tax_config$business_license$calculation_method
  } else {
    subcat_config$calculation_method
  }
  
  if (calc_method == "minimum_rate") {
    # Traditional calculation: max of (value * rate) or minimum
    tax_amount <- max(business_value * (subcat_config$rate / 100), subcat_config$minimum)
    
  } else if (calc_method == "flat_value") {
    # Flat amount based on business value bands
    if (subcat_config$use_value_bands) {
      band_num <- 1
      for (i in 1:3) {
        band <- tax_config$business_license$value_bands[[paste0("band", i)]]
        if (business_value >= band$min && business_value < band$max) {
          band_num <- i
          break
        }
      }
      tax_amount <- subcat_config$flat_amounts_value[[paste0("band", band_num)]]
    } else {
      # If no bands, use flat amount
      tax_amount <- subcat_config$flat_amount
    }
    
  } else if (calc_method == "flat_area") {
    # Flat amount based on business area bands
    if (subcat_config$use_area_bands) {
      band_num <- 1
      for (i in 1:3) {
        band <- tax_config$business_license$area_bands[[paste0("band", i)]]
        if (business_area >= band$min && business_area < band$max) {
          band_num <- i
          break
        }
      }
      tax_amount <- subcat_config$flat_amounts_area[[paste0("band", band_num)]]
    } else {
      # If no bands, use flat amount
      tax_amount <- subcat_config$flat_amount
    }
  }
  
  return(list(
    license_amount = tax_amount,
    method_used = calc_method,
    band_used = if(subcat_config$use_bands || subcat_config$use_value_bands || subcat_config$use_area_bands) band_num else NA
  ))
}