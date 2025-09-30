# R/module3_functions.R

# Updated get_default_tax_config function with complete mappings
get_default_tax_config <- function() {
  list(
    # Property tax defaults (unchanged)
    property_tax = list(
      # Global band definitions
      bands = list(
        band1 = list(min = 0, max = 0),
        band2 = list(min = 0, max = 0),
        band3 = list(min = 0, max = Inf)
      ),
      # Tax rates by property type
      domestic = list(
        use_bands = FALSE,
        minimum = 200,        
        rate = 0.025,         
        band_rates = list(rate1 = 0.02, rate2 = 0.025, rate3 = 0.03),
        band_minimums = list(min1 = 100, min2 = 200, min3 = 300)
      ),
      commercial = list(
        use_bands = FALSE,
        minimum = 200,        
        rate = 0.04,          
        band_rates = list(rate1 = 0.03, rate2 = 0.04, rate3 = 0.05),
        band_minimums = list(min1 = 150, min2 = 200, min3 = 400)
      ),
      institutional = list(
        use_bands = FALSE,
        minimum = 200,        
        rate = 0.025,         
        band_rates = list(rate1 = 0.02, rate2 = 0.025, rate3 = 0.03),
        band_minimums = list(min1 = 100, min2 = 200, min3 = 300)
      )
    ),
    
    # Business license defaults - complete explicit mappings
    business_license = list(
      # Complete subcategory mappings with exact names from your CSV
      subcategories = list(
        # Communication services
        "Hardware Equipment and Consumables" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.035
        ),
        "Information Technology" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.035
        ),
        "Media Company" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.035
        ),
        "Software services" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.035
        ),
        "Telecommunications" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.035
        ),
        
        # Consumer discretionary
        "Barbers/hair or nail saloon" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        "Building materials retail" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        "Diamond, Jewellery shop" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        "Hotels" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        "Household durable products" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        "Motor Cars and components" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        "Nursery" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        "Private Schools" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        "Private healthcare" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        "Restaurants Bars" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        "Security" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        "Solar retail" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        "Tailor shop" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        "Textiles, Clothes, Luxury Goods" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        "Transport company" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03
        ),
        
        # Consumer staples
        "Drug stores or pharmacies" = list(
          calculation_method = "minimum_rate",
          minimum = 450,
          rate = 0.02
        ),
        "Foods, Beverages and Staples" = list(
          calculation_method = "minimum_rate",
          minimum = 450,
          rate = 0.02
        ),
        "Household and personal products" = list(
          calculation_method = "minimum_rate",
          minimum = 450,
          rate = 0.02
        ),
        "Typical provision shop" = list(
          calculation_method = "minimum_rate",
          minimum = 450,
          rate = 0.02
        ),
        "Wholesaler of consumer staples" = list(
          calculation_method = "minimum_rate",
          minimum = 450,
          rate = 0.02
        ),
        
        # Energy
        "Consumer Fuels" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.065
        ),
        "Oil and Gas" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.065
        ),
        "Petrol station" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.065
        ),
        "Solar Energy Provider" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.065
        ),
        
        # Financials
        "Banks" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.055
        ),
        "Currency exchange" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.055
        ),
        "Discount house" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.055
        ),
        "Insurance company" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.055
        ),
        "Microcredit" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.055
        ),
        
        # Materials Manufacturing/Industrials/Services
        "Agri producers, processor, dealer or exporter" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.05
        ),
        "Biotech, Pharmaceuticals, Life Sciences" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.05
        ),
        "Building Materials production" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.05
        ),
        "Construction company" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.05
        ),
        "Consulting & Legal Services and other professional services" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.05
        ),
        "Fishing" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.05
        ),
        "Industrial plant or processor" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.05
        ),
        "Media/TV/newspaper" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.05
        ),
        "Mining or natural extraction (incl. Exporter)" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.05
        ),
        "Warehousing" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.05
        ),
        "Workshop or repair contractor" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.05
        ),
        
        # Portfolio - flat amounts as specified
        "Clearing and forwarding companies" = list(
          calculation_method = "flat",
          flat_amount = 4000
        ),
        "Clinic" = list(
          calculation_method = "flat",
          flat_amount = 1350
        ),
        "Construction Ministry of Works - Class I" = list(
          calculation_method = "flat",
          flat_amount = 6000
        ),
        "Construction Ministry of Works - Class II" = list(
          calculation_method = "flat",
          flat_amount = 4000
        ),
        "Construction Ministry of Works - Class III" = list(
          calculation_method = "flat",
          flat_amount = 2500
        ),
        "Construction Ministry of Works - Premium" = list(
          calculation_method = "flat",
          flat_amount = 10000
        ),
        "Consultancy" = list(
          calculation_method = "flat",
          flat_amount = 4000
        ),
        "Consumer Discretionary (certificate)" = list(
          calculation_method = "flat",
          flat_amount = 10000
        ),
        "Consumer Staples (certificate)" = list(
          calculation_method = "flat",
          flat_amount = 500
        ),
        "Funeral Parlour" = list(
          calculation_method = "flat",
          flat_amount = 2000
        ),
        "General Merchandise (Limited Company)" = list(
          calculation_method = "flat",
          flat_amount = 1350
        ),
        "General Merchandise /Services" = list(
          calculation_method = "flat",
          flat_amount = 4000
        ),
        "General Supplies (Limited company)" = list(
          calculation_method = "flat",
          flat_amount = 1350
        ),
        "Hospital" = list(
          calculation_method = "flat",
          flat_amount = 2700
        ),
        "Logistics" = list(
          calculation_method = "flat",
          flat_amount = 4000
        ),
        "Pharmaceutical" = list(
          calculation_method = "flat",
          flat_amount = 1350
        ),
        "Pharmacy (certificate)" = list(
          calculation_method = "flat",
          flat_amount = 500
        ),
        "Procurement Suppliers" = list(
          calculation_method = "flat",
          flat_amount = 4000
        ),
        "Production Factory (Limited Company)" = list(
          calculation_method = "flat",
          flat_amount = 5500
        ),
        "Recruitment and Training companies" = list(
          calculation_method = "flat",
          flat_amount = 1350
        ),
        "Shipping Company" = list(
          calculation_method = "flat",
          flat_amount = 3900
        ),
        "Suppliers" = list(
          calculation_method = "flat",
          flat_amount = 2500
        ),
        "Transport Company" = list(
          calculation_method = "flat",
          flat_amount = 3900
        ),
        
        # Handle "Other" subcategories - use category defaults
        "Other" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.035
        )
      ),
      
      # Default fallback configuration
      default_subcategory = list(
        calculation_method = "minimum_rate",
        minimum = 0,        
        rate = 0.00,          
        flat_amount = 0
      )
    )
  ) # <- Add this closing parenthesis for wellPanel
}

# Simplified calculate_business_license function
calculate_business_license <- function(business_value, business_area, business_subcategory, tax_config) {
  # Get the business license configuration from tax_config
  business_config <- tax_config$business_license
  
  # Look up the exact subcategory configuration
  if (business_subcategory %in% names(business_config$subcategories)) {
    subcat_config <- business_config$subcategories[[business_subcategory]]
  } else {
    # Use default fallback if subcategory not found
    subcat_config <- business_config$default_subcategory
  }
  
  # Calculate tax based on the method
  if (subcat_config$calculation_method == "minimum_rate") {
    # Method 1: Traditional calculation (minimum tax or percentage of value, whichever is higher)
    tax_amount <- max(business_value * subcat_config$rate, subcat_config$minimum)
    
  } else if (subcat_config$calculation_method == "flat") {
    # Method 2: Flat amount
    tax_amount <- subcat_config$flat_amount
    
  } else if (subcat_config$calculation_method == "flat_value_bands") {
    # Method 3: Flat amount based on business value bands
    tax_amount <- subcat_config$value_bands$band3$tax  # Default to highest band
    
    if (business_value <= subcat_config$value_bands$band1$max) {
      tax_amount <- subcat_config$value_bands$band1$tax
    } else if (business_value <= subcat_config$value_bands$band2$max) {
      tax_amount <- subcat_config$value_bands$band2$tax
    }
    
  } else if (subcat_config$calculation_method == "flat_area_bands") {
    # Method 4: Flat amount based on business area bands
    tax_amount <- subcat_config$area_bands$band3$tax  # Default to highest band
    
    if (business_area <= subcat_config$area_bands$band1$max) {
      tax_amount <- subcat_config$area_bands$band1$tax
    } else if (business_area <= subcat_config$area_bands$band2$max) {
      tax_amount <- subcat_config$area_bands$band2$tax
    }
    
  } else {
    # Default fallback
    tax_amount <- max(business_value * 0.035, 350)
  }
  
  return(list(
    license_amount = tax_amount,
    method_used = subcat_config$calculation_method,
    rate_used = ifelse(is.null(subcat_config$rate), 0, subcat_config$rate),
    minimum_used = ifelse(is.null(subcat_config$minimum), 0, subcat_config$minimum),
    flat_amount_used = ifelse(is.null(subcat_config$flat_amount), 0, subcat_config$flat_amount)
  ))
}

get_subcategory_defaults <- function(subcategory) {
  # Get the default configuration
  defaults <- get_default_tax_config()
  
  # Look up the exact subcategory configuration
  if (subcategory %in% names(defaults$business_license$subcategories)) {
    subcat_config <- defaults$business_license$subcategories[[subcategory]]
    
    return(list(
      minimum = ifelse(is.null(subcat_config$minimum), 0, subcat_config$minimum),
      rate = ifelse(is.null(subcat_config$rate), 0, subcat_config$rate * 100),
      flat_amount = ifelse(is.null(subcat_config$flat_amount), 0, subcat_config$flat_amount),
      calculation_method = subcat_config$calculation_method
    ))
  }
  
  # Use default fallback if subcategory not found
  default_config <- defaults$business_license$default_subcategory
  return(list(
    minimum = default_config$minimum,
    rate = default_config$rate * 100,
    flat_amount = default_config$flat_amount,
    calculation_method = default_config$calculation_method
  ))
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

# Function to calculate property taxes handling duplicates from Module 1
calculate_property_taxes_with_deduplication <- function(data, property_values, property_types, tax_config) {
  # Create a unique identifier for property-type combinations
  property_key <- paste(data$id_property, property_types, sep = "_")
  
  # Create dataframe with all necessary information
  calc_data <- data.frame(
    row_id = 1:length(property_values),
    property_key = property_key,
    id_property = data$id_property,
    property_type = property_types,
    property_value = property_values,
    stringsAsFactors = FALSE
  )
  
  # Find unique property-type combinations and keep track of first occurrence
  unique_indices <- !duplicated(calc_data$property_key)
  unique_data <- calc_data[unique_indices, ]
  
  # Calculate tax for unique properties only
  unique_taxes <- numeric(nrow(unique_data))
  
  for (i in 1:nrow(unique_data)) {
    prop_type <- unique_data$property_type[i]
    prop_value <- unique_data$property_value[i]
    
    if (is.na(prop_value) || prop_value <= 0) {
      unique_taxes[i] <- 0
      next
    }
    
    # Use the existing calculate_property_tax function
    tax_result <- calculate_property_tax(prop_value, prop_type, tax_config)
    unique_taxes[i] <- tax_result$tax_amount
  }
  
  # Create lookup table mapping property_key to tax amount
  tax_lookup <- setNames(unique_taxes, unique_data$property_key)
  
  # Apply the calculated taxes back to all rows (including duplicates)
  final_taxes <- tax_lookup[calc_data$property_key]
  
  return(final_taxes)
}

# Add these helper functions to R/module3_functions.R

# Helper function to create property type configuration UI
create_property_type_ui <- function(ns, prop_type, scenario_suffix) {
  wellPanel(
    h5(stringr::str_to_title(prop_type), "Properties"),
    checkboxInput(ns(paste0("use_slots_", prop_type, "_", scenario_suffix)),
                  "Use value-based logic slots",
                  value = FALSE),
    
    conditionalPanel(
      condition = paste0("!input['", ns(paste0("use_slots_", prop_type, "_", scenario_suffix)), "']"),
      # Simple configuration
      fluidRow(
        column(6,
               numericInput(ns(paste0(prop_type, "_min_", scenario_suffix)),
                           "Minimum Tax:",
                           value = get_default_min(prop_type),
                           min = 0)),
        column(6,
               numericInput(ns(paste0(prop_type, "_rate_", scenario_suffix)),
                           "Tax Rate (%):",
                           value = get_default_rate(prop_type),
                           min = 0,
                           step = 0.1))
      )
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns(paste0("use_slots_", prop_type, "_", scenario_suffix)), "']"),
      # Logic slots configuration
      h6("Logic Slot Ranges:"),
      create_slot_ranges_ui(ns, prop_type, scenario_suffix),
      hr(),
      h6("Tax Configuration per Slot:"),
      create_slot_tax_config_ui(ns, prop_type, scenario_suffix)
    )
  )
}

# Helper function to create slot ranges UI
create_slot_ranges_ui <- function(ns, prop_type, scenario_suffix) {
  tagList(
    fluidRow(
      column(4, p("Slot 1:", style = "font-weight: bold;")),
      column(4, numericInput(ns(paste0(prop_type, "_slot1_min_", scenario_suffix)), 
                            "Min:", value = 0, min = 0)),
      column(4, numericInput(ns(paste0(prop_type, "_slot1_max_", scenario_suffix)), 
                            "Max:", value = 350, min = 0))
    ),
    fluidRow(
      column(4, p("Slot 2:", style = "font-weight: bold;")),
      column(4, numericInput(ns(paste0(prop_type, "_slot2_min_", scenario_suffix)), 
                            "Min:", value = 350, min = 0)),
      column(4, numericInput(ns(paste0(prop_type, "_slot2_max_", scenario_suffix)), 
                            "Max:", value = 700, min = 0))
    ),
    fluidRow(
      column(4, p("Slot 3:", style = "font-weight: bold;")),
      column(4, numericInput(ns(paste0(prop_type, "_slot3_min_", scenario_suffix)), 
                            "Min:", value = 700, min = 0)),
      column(4, p("Max: No limit", style = "padding-top: 25px;"))
    )
  )
}

# Helper function to create slot tax configuration UI
create_slot_tax_config_ui <- function(ns, prop_type, scenario_suffix) {
  defaults <- get_slot_defaults(prop_type)
  
  tagList(
    # Slot 1
    p("Logic Slot 1:", style = "font-weight: bold;"),
    fluidRow(
      column(6, numericInput(ns(paste0(prop_type, "_slot1_min_tax_", scenario_suffix)), 
                            "Min Tax:", value = defaults$slot1$min_tax, min = 0)),
      column(6, numericInput(ns(paste0(prop_type, "_slot1_rate_", scenario_suffix)), 
                            "Rate (%):", value = defaults$slot1$rate, min = 0, step = 0.1))
    ),
    # Slot 2
    p("Logic Slot 2:", style = "font-weight: bold;"),
    fluidRow(
      column(6, numericInput(ns(paste0(prop_type, "_slot2_min_tax_", scenario_suffix)), 
                            "Min Tax:", value = defaults$slot2$min_tax, min = 0)),
      column(6, numericInput(ns(paste0(prop_type, "_slot2_rate_", scenario_suffix)), 
                            "Rate (%):", value = defaults$slot2$rate, min = 0, step = 0.1))
    ),
    # Slot 3
    p("Logic Slot 3:", style = "font-weight: bold;"),
    fluidRow(
      column(6, numericInput(ns(paste0(prop_type, "_slot3_min_tax_", scenario_suffix)), 
                            "Min Tax:", value = defaults$slot3$min_tax, min = 0)),
      column(6, numericInput(ns(paste0(prop_type, "_slot3_rate_", scenario_suffix)), 
                            "Rate (%):", value = defaults$slot3$rate, min = 0, step = 0.1))
    )
  )
}

# Helper function to create scenario column
create_scenario_column <- function(ns, scenario_name, scenario_suffix) {
  column(4,
    h4(scenario_name),
    
    # Create property type configurations
    create_property_type_ui(ns, "domestic", scenario_suffix),
    create_property_type_ui(ns, "commercial", scenario_suffix),
    create_property_type_ui(ns, "institutional", scenario_suffix)
  )
}

# Helper functions for default values
get_default_min <- function(prop_type) {
  switch(prop_type,
    "domestic" = 200,
    "commercial" = 200,
    "institutional" = 200,
    200
  )
}

get_default_rate <- function(prop_type) {
  switch(prop_type,
    "domestic" = 2.5,
    "commercial" = 4,
    "institutional" = 2.5,
    2.5
  )
}

get_slot_defaults <- function(prop_type) {
  switch(prop_type,
    "domestic" = list(
      slot1 = list(min_tax = 100, rate = 2.0),
      slot2 = list(min_tax = 200, rate = 2.5),
      slot3 = list(min_tax = 300, rate = 3.0)
    ),
    "commercial" = list(
      slot1 = list(min_tax = 150, rate = 3.0),
      slot2 = list(min_tax = 200, rate = 4.0),
      slot3 = list(min_tax = 400, rate = 5.0)
    ),
    "institutional" = list(
      slot1 = list(min_tax = 100, rate = 2.0),
      slot2 = list(min_tax = 200, rate = 2.5),
      slot3 = list(min_tax = 300, rate = 3.0)
    ),
    # Default fallback
    list(
      slot1 = list(min_tax = 100, rate = 2.0),
      slot2 = list(min_tax = 200, rate = 2.5),
      slot3 = list(min_tax = 300, rate = 3.0)
    )
  )
}
# Add these business license helper functions to R/module3_functions.R

# Helper function to create business license scenario column
create_business_license_scenario_column <- function(ns, scenario_name, scenario_suffix) {
  column(4,
    h4(scenario_name),
    h5("Business Subcategories"),
    div(style = "max-height: 600px; overflow-y: auto;",
        uiOutput(ns(paste0("business_subcategories_", scenario_suffix)))
    )
  )
}

# Replace the existing create_business_subcategory_ui function
create_business_subcategory_ui <- function(ns, subcategory, scenario_suffix) {
  subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)
  
  # Get subcategory-specific defaults
  defaults <- get_subcategory_defaults(subcategory)
  
  wellPanel(
    style = "margin-bottom: 10px;",
    h6(subcategory, style = "font-weight: bold; color: #337ab7;"),
    
    # Tax calculation method selection
    selectInput(ns(paste0("bus_subcat_", subcategory_safe, "_method_", scenario_suffix)),
                "Tax Calculation Method:",
                choices = c(
                  "Tax from minimum and rate" = "minimum_rate",
                  "Flat amount (fixed)" = "flat",
                  "Flat amount from business value calculation" = "flat_value_bands",
                  "Flat amount from business area" = "flat_area_bands"
                ),
                selected = defaults$calculation_method), # Use subcategory-specific default
    
    # Method 1: Minimum + Rate configuration
    conditionalPanel(
      condition = paste0("input['", ns(paste0("bus_subcat_", subcategory_safe, "_method_", scenario_suffix)), "'] == 'minimum_rate'"),
      fluidRow(
        column(6,
               numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_min_", scenario_suffix)),
                           "Minimum Tax:",
                           value = defaults$minimum,
                           min = 0)),
        column(6,
               numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_rate_", scenario_suffix)),
                           "Rate (%):",
                           value = defaults$rate,
                           min = 0,
                           step = 0.1))
      )
    ),
    
    # Method 2: Flat amount (fixed)
    conditionalPanel(
      condition = paste0("input['", ns(paste0("bus_subcat_", subcategory_safe, "_method_", scenario_suffix)), "'] == 'flat'"),
      numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_flat_", scenario_suffix)),
                   "Flat Tax Amount:",
                   value = defaults$flat_amount,
                   min = 0)
    ),
    
    # Method 3: Flat amount from business value (with up to 3 value bands)
    conditionalPanel(
      condition = paste0("input['", ns(paste0("bus_subcat_", subcategory_safe, "_method_", scenario_suffix)), "'] == 'flat_value_bands'"),
      h6("Value-Based Flat Tax (up to 3 bands):"),
      # Band 1
      fluidRow(
        column(4, p("Band 1:", style = "font-weight: bold;")),
        column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_value_band1_max_", scenario_suffix)), 
                              "Max Value:", value = 100000, min = 0)),
        column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_value_band1_tax_", scenario_suffix)), 
                              "Flat Tax:", value = 500, min = 0))
      ),
      # Band 2
      fluidRow(
        column(4, p("Band 2:", style = "font-weight: bold;")),
        column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_value_band2_max_", scenario_suffix)), 
                              "Max Value:", value = 500000, min = 0)),
        column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_value_band2_tax_", scenario_suffix)), 
                              "Flat Tax:", value = 1500, min = 0))
      ),
      # Band 3
      fluidRow(
        column(4, p("Band 3 (above):", style = "font-weight: bold;")),
        column(4, p("No limit", style = "padding-top: 25px;")),
        column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_value_band3_tax_", scenario_suffix)), 
                              "Flat Tax:", value = 3000, min = 0))
      )
    ),
    
    # Method 4: Flat amount from business area (with up to 3 area bands)
    conditionalPanel(
      condition = paste0("input['", ns(paste0("bus_subcat_", subcategory_safe, "_method_", scenario_suffix)), "'] == 'flat_area_bands'"),
      h6("Area-Based Flat Tax (up to 3 bands):"),
      # Band 1
      fluidRow(
        column(4, p("Band 1:", style = "font-weight: bold;")),
        column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_area_band1_max_", scenario_suffix)), 
                              "Max Area:", value = 100, min = 0)),
        column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_area_band1_tax_", scenario_suffix)), 
                              "Flat Tax:", value = 300, min = 0))
      ),
      # Band 2
      fluidRow(
        column(4, p("Band 2:", style = "font-weight: bold;")),
        column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_area_band2_max_", scenario_suffix)), 
                              "Max Area:", value = 500, min = 0)),
        column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_area_band2_tax_", scenario_suffix)), 
                              "Flat Tax:", value = 800, min = 0))
      ),
      # Band 3
      fluidRow(
        column(4, p("Band 3 (above):", style = "font-weight: bold;")),
        column(4, p("No limit", style = "padding-top: 25px;")),
        column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_area_band3_tax_", scenario_suffix)), 
                              "Flat Tax:", value = 1200, min = 0))
      )
    )
  ) # <- This closes the wellPanel
}

# Helper function to create business license slot ranges UI
create_business_slot_ranges_ui <- function(ns, subcategory_safe, scenario_suffix) {
  tagList(
    fluidRow(
      column(4, p("Slot 1:", style = "font-weight: bold;")),
      column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_slot1_min_", scenario_suffix)), 
                            "Min:", value = 0, min = 0)),
      column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_slot1_max_", scenario_suffix)), 
                            "Max:", value = 100000, min = 0))
    ),
    fluidRow(
      column(4, p("Slot 2:", style = "font-weight: bold;")),
      column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_slot2_min_", scenario_suffix)), 
                            "Min:", value = 100000, min = 0)),
      column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_slot2_max_", scenario_suffix)), 
                            "Max:", value = 500000, min = 0))
    ),
    fluidRow(
      column(4, p("Slot 3:", style = "font-weight: bold;")),
      column(4, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_slot3_min_", scenario_suffix)), 
                            "Min:", value = 500000, min = 0)),
      column(4, p("Max: No limit", style = "padding-top: 25px;"))
    )
  )
}

# Helper function to create business license slot tax configuration UI
create_business_slot_tax_config_ui <- function(ns, subcategory_safe, scenario_suffix) {
  defaults <- get_business_slot_defaults(subcategory_safe)
  
  tagList(
    # Slot 1
    p("Logic Slot 1:", style = "font-weight: bold;"),
    fluidRow(
      column(6, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_slot1_min_tax_", scenario_suffix)), 
                            "Min Tax:", value = defaults$slot1$min_tax, min = 0)),
      column(6, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_slot1_rate_", scenario_suffix)), 
                            "Rate (%):", value = defaults$slot1$rate, min = 0, step = 0.1))
    ),
    # Slot 2
    p("Logic Slot 2:", style = "font-weight: bold;"),
    fluidRow(
      column(6, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_slot2_min_tax_", scenario_suffix)), 
                            "Min Tax:", value = defaults$slot2$min_tax, min = 0)),
      column(6, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_slot2_rate_", scenario_suffix)), 
                            "Rate (%):", value = defaults$slot2$rate, min = 0, step = 0.1))
    ),
    # Slot 3
    p("Logic Slot 3:", style = "font-weight: bold;"),
    fluidRow(
      column(6, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_slot3_min_tax_", scenario_suffix)), 
                            "Min Tax:", value = defaults$slot3$min_tax, min = 0)),
      column(6, numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_slot3_rate_", scenario_suffix)), 
                            "Rate (%):", value = defaults$slot3$rate, min = 0, step = 0.1))
    )
  )
}

# Helper functions for business license default values
get_default_business_min <- function(subcategory) {
  # You can customize these defaults based on subcategory if needed
  switch(subcategory,
    "Communication services" = 500,
    "Consumer discretionary" = 300,
    "Consumer staples" = 250,
    "Energy" = 800,
    "Financials" = 1000,
    "Materials Manufacturing/Industrials/Services" = 400,
    300 # default fallback
  )
}

get_default_business_rate <- function(subcategory) {
  # You can customize these defaults based on subcategory if needed
  switch(subcategory,
    "Communication services" = 1.5,
    "Consumer discretionary" = 1.0,
    "Consumer staples" = 0.8,
    "Energy" = 2.0,
    "Financials" = 2.5,
    "Materials Manufacturing/Industrials/Services" = 1.2,
    1.0 # default fallback
  )
}

get_default_business_flat <- function(subcategory) {
  # You can customize these defaults based on subcategory if needed
  switch(subcategory,
    "Communication services" = 1000,
    "Consumer discretionary" = 750,
    "Consumer staples" = 600,
    "Energy" = 1500,
    "Financials" = 2000,
    "Materials Manufacturing/Industrials/Services" = 800,
    750 # default fallback
  )
}

get_business_slot_defaults <- function(subcategory_safe) {
  # You can customize these defaults based on subcategory if needed
  list(
    slot1 = list(min_tax = 200, rate = 0.5),
    slot2 = list(min_tax = 500, rate = 1.0),
    slot3 = list(min_tax = 1000, rate = 1.5)
  )
}

# Updated function to get business subcategories from actual data
get_business_subcategories <- function() {
  # This will be populated by the server using actual data
  # Return empty vector as fallback
  return(c())
}

# New function to get business subcategories from processed data
get_business_subcategories_from_data <- function(processed_data) {
  if (is.null(processed_data) || !"business_sub_category" %in% names(processed_data)) {
    return(c())
  }
  
  # Get unique business subcategories from the actual data
  subcategories <- unique(processed_data$business_sub_category[!is.na(processed_data$business_sub_category)])
  
  # Sort them for consistent display
  return(sort(subcategories))
}

# Also get categories if they exist
get_business_categories_from_data <- function(processed_data) {
  if (is.null(processed_data) || !"business_category" %in% names(processed_data)) {
    return(c())
  }
  
  # Get unique business categories from the actual data
  categories <- unique(processed_data$business_category[!is.na(processed_data$business_category)])
  
  # Sort them for consistent display
  return(sort(categories))
}
