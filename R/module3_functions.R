# R/module3_functions.R

# Function to get default tax configurations
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
    
    # Business license defaults - restructured for categories and subcategories
    business_license = list(
      calculation_method = "minimum_rate",  # Global default
      
      # Category-specific configurations
      categories = list(
        "Communication services" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.035  # 3.5%
        ),
        "Consumer discretionary" = list(
          calculation_method = "minimum_rate",
          minimum = 500,
          rate = 0.03   # 3%
        ),
        "Consumer staples" = list(
          calculation_method = "minimum_rate",
          minimum = 450,
          rate = 0.02   # 2%
        ),
        "Energy" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.065  # 6.5%
        ),
        "Financials" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.055  # 5.5%
        ),
        "Materials Manufacturing/Industrials/Services" = list(
          calculation_method = "minimum_rate",
          minimum = 350,
          rate = 0.05   # 5%
        ),
        "Portfolio" = list(
          calculation_method = "subcategory_specific",  # Use subcategory configs
          # Subcategory-specific configurations
          subcategories = list(
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
            "Consumer Discretionary (certificate)" = list(
              calculation_method = "flat",
              flat_amount = 10000
            ),
            "Consumer Staples (certificate)" = list(
              calculation_method = "flat",
              flat_amount = 500
            ),
            "Cool Room" = list(
              calculation_method = "flat",
              flat_amount = 5500
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
            "Orange & Africell Money Agents" = list(
              calculation_method = "flat",
              flat_amount = 500
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
            )
          )
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
  # Check if this subcategory exists in our tax config
  if (business_subcategory %in% names(tax_config)) {
    subcat_config <- tax_config[[business_subcategory]]
  } else {
    # Use default configuration if subcategory not found
    subcat_config <- list(
      calculation_method = "minimum_rate",
      minimum = 350,
      rate = 0.035
    )
  }
  
  if (subcat_config$calculation_method == "minimum_rate") {
    # Method 1: Traditional calculation
    tax_amount <- max(business_value * subcat_config$rate, subcat_config$minimum)
    
  } else if (subcat_config$calculation_method == "flat_value_bands") {
    # Method 2: Flat amount based on business value bands
    tax_amount <- subcat_config$value_bands$band3$tax  # Default to highest band
    
    if (business_value <= subcat_config$value_bands$band1$max) {
      tax_amount <- subcat_config$value_bands$band1$tax
    } else if (business_value <= subcat_config$value_bands$band2$max) {
      tax_amount <- subcat_config$value_bands$band2$tax
    }
    
  } else if (subcat_config$calculation_method == "flat_area_bands") {
    # Method 3: Flat amount based on business area bands
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
    method_used = subcat_config$calculation_method
  ))
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

# Add this function to R/module3_functions.R
get_subcategory_defaults <- function(subcategory) {
  # Get the default configuration
  defaults <- get_default_tax_config()
  
  # Check if this subcategory has specific Portfolio configuration
  portfolio_subcats <- defaults$business_license$categories$Portfolio$subcategories
  
  if (subcategory %in% names(portfolio_subcats)) {
    # Portfolio subcategory with flat amount
    subcat_config <- portfolio_subcats[[subcategory]]
    return(list(
      minimum = subcat_config$flat_amount,
      rate = 0, # Portfolio items typically don't use rates
      flat_amount = subcat_config$flat_amount,
      calculation_method = subcat_config$calculation_method
    ))
  }
  
  # Check if this subcategory matches a category pattern
  for (category_name in names(defaults$business_license$categories)) {
    if (category_name == "Portfolio") next # Already handled above
    
    category_config <- defaults$business_license$categories[[category_name]]
    
    # Simple pattern matching - you can make this more sophisticated
    if (grepl(tolower(gsub(" ", "", category_name)), tolower(gsub(" ", "", subcategory)), fixed = TRUE)) {
      return(list(
        minimum = category_config$minimum,
        rate = category_config$rate * 100, # Convert to percentage for display
        flat_amount = category_config$minimum,
        calculation_method = category_config$calculation_method
      ))
    }
  }
  
  # Use default fallback
  default_config <- defaults$business_license$default_subcategory
  return(list(
    minimum = default_config$minimum,
    rate = default_config$rate * 100, # Convert to percentage for display
    flat_amount = default_config$flat_amount,
    calculation_method = default_config$calculation_method
  ))
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
                  "Tax from minimum and rate" = "min_rate",
                  "Flat amount from business value calculation" = "flat_value", 
                  "Flat amount from business area" = "flat_area"
                ),
                selected = "min_rate"),
    
    # Method 1: Minimum + Rate configuration
    conditionalPanel(
      condition = paste0("input['", ns(paste0("bus_subcat_", subcategory_safe, "_method_", scenario_suffix)), "'] == 'min_rate'"),
      fluidRow(
        column(6,
               numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_min_", scenario_suffix)),
                           "Minimum Tax:",
                           value = defaults$minimum,  # Use subcategory-specific default
                           min = 0)),
        column(6,
               numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_rate_", scenario_suffix)),
                           "Rate (%):",
                           value = defaults$rate,     # Use subcategory-specific default
                           min = 0,
                           step = 0.1))
      )
    ),
    
    # Method 2: Flat amount from business value (with up to 3 value bands)
    conditionalPanel(
      condition = paste0("input['", ns(paste0("bus_subcat_", subcategory_safe, "_method_", scenario_suffix)), "'] == 'flat_value'"),
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
    
    # Method 3: Flat amount from business area (with up to 3 area bands)
    conditionalPanel(
      condition = paste0("input['", ns(paste0("bus_subcat_", subcategory_safe, "_method_", scenario_suffix)), "'] == 'flat_area'"),
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
  )
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
