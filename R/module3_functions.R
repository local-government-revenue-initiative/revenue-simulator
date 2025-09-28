# R/module3_functions.R

# Function to get default tax configurations
get_default_tax_config <- function() {
  list(
    # Property tax defaults (unchanged)
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
        minimum = 350,        
        rate = 0.05,          
        flat_amount = 350
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

# Helper function to create individual business subcategory UI
create_business_subcategory_ui <- function(ns, subcategory, scenario_suffix) {
  subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)
  
  wellPanel(
    h6(subcategory, style = "font-weight: bold;"),
    
    # Tax method selection
    selectInput(ns(paste0("bus_subcat_", subcategory_safe, "_method_", scenario_suffix)),
                "Tax Method:",
                choices = c("Minimum + Rate" = "min_rate",
                           "Flat Tax" = "flat"),
                selected = "min_rate"),
    
    # Conditional panels for different tax methods
    conditionalPanel(
      condition = paste0("input['", ns(paste0("bus_subcat_", subcategory_safe, "_method_", scenario_suffix)), "'] == 'min_rate'"),
      
      # Use logic slots option
      checkboxInput(ns(paste0("bus_subcat_", subcategory_safe, "_use_slots_", scenario_suffix)),
                    "Use value/area-based logic slots",
                    value = FALSE),
      
      # Simple min + rate configuration
      conditionalPanel(
        condition = paste0("!input['", ns(paste0("bus_subcat_", subcategory_safe, "_use_slots_", scenario_suffix)), "']"),
        fluidRow(
          column(6,
                 numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_min_", scenario_suffix)),
                             "Minimum Tax:",
                             value = get_default_business_min(subcategory),
                             min = 0)),
          column(6,
                 numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_rate_", scenario_suffix)),
                             "Rate (%):",
                             value = get_default_business_rate(subcategory),
                             min = 0,
                             step = 0.1))
        )
      ),
      
      # Logic slots configuration
      conditionalPanel(
        condition = paste0("input['", ns(paste0("bus_subcat_", subcategory_safe, "_use_slots_", scenario_suffix)), "']"),
        
        # Slot basis selection
        radioButtons(ns(paste0("bus_subcat_", subcategory_safe, "_slot_basis_", scenario_suffix)),
                     "Base slots on:",
                     choices = c("Business Value" = "value", "Business Area" = "area"),
                     selected = "value",
                     inline = TRUE),
        
        # Slot ranges
        h6("Logic Slot Ranges:"),
        create_business_slot_ranges_ui(ns, subcategory_safe, scenario_suffix),
        
        hr(),
        
        # Tax configuration per slot
        h6("Tax Configuration per Slot:"),
        create_business_slot_tax_config_ui(ns, subcategory_safe, scenario_suffix)
      )
    ),
    
    # Flat tax configuration
    conditionalPanel(
      condition = paste0("input['", ns(paste0("bus_subcat_", subcategory_safe, "_method_", scenario_suffix)), "'] == 'flat'"),
      numericInput(ns(paste0("bus_subcat_", subcategory_safe, "_flat_", scenario_suffix)),
                   "Flat Tax Amount:",
                   value = get_default_business_flat(subcategory),
                   min = 0)
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

# Helper function to get business subcategories (you may need to adjust this based on your data)
get_business_subcategories <- function() {
  c("Communication services", 
    "Consumer discretionary", 
    "Consumer staples", 
    "Energy", 
    "Financials", 
    "Materials Manufacturing/Industrials/Services")
}