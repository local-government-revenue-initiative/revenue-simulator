# ==============================================================================
# MODULE 3: Configuration Save/Load Functions
# ==============================================================================

# Function to collect all Module 3 configuration from inputs
collect_module3_config <- function(
  input,
  scenario_suffix,
  business_subcategories
) {
  config <- list(
    module = "module3",
    scenario = scenario_suffix,
    timestamp = Sys.time(),

    # Property tax settings
    property_tax = list(
      domestic = list(),
      commercial = list(),
      institutional = list()
    ),

    # Business license settings
    business_license = list()
  )

  # Collect property tax settings for each type
  for (prop_type in c("domestic", "commercial", "institutional")) {
    # Basic settings
    config$property_tax[[prop_type]]$min_tax <- input[[paste0(
      prop_type,
      "_min_",
      scenario_suffix
    )]]
    config$property_tax[[prop_type]]$rate <- input[[paste0(
      prop_type,
      "_rate_",
      scenario_suffix
    )]]

    # Check if slots are used
    use_slots_id <- paste0("use_slots_", prop_type, "_", scenario_suffix)
    config$property_tax[[prop_type]]$use_slots <- input[[use_slots_id]]

    # If slots are used, collect slot settings
    if (!is.null(input[[use_slots_id]]) && input[[use_slots_id]]) {
      config$property_tax[[prop_type]]$slots <- list()

      for (slot in 1:3) {
        slot_config <- list(
          min = input[[paste0(
            prop_type,
            "_slot",
            slot,
            "_min_",
            scenario_suffix
          )]],
          max = input[[paste0(
            prop_type,
            "_slot",
            slot,
            "_max_",
            scenario_suffix
          )]],
          min_tax = input[[paste0(
            prop_type,
            "_slot",
            slot,
            "_min_tax_",
            scenario_suffix
          )]],
          rate = input[[paste0(
            prop_type,
            "_slot",
            slot,
            "_rate_",
            scenario_suffix
          )]]
        )
        config$property_tax[[prop_type]]$slots[[paste0(
          "slot",
          slot
        )]] <- slot_config
      }
    }
  }

  # Collect business license settings
  if (!is.null(business_subcategories)) {
    for (subcategory in business_subcategories) {
      subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)

      # Get method selection
      method_id <- paste0(
        "bus_subcat_",
        subcategory_safe,
        "_method_",
        scenario_suffix
      )
      method <- input[[method_id]]

      subcat_config <- list(
        method = method
      )

      # Collect all possible input values (the UI will show/hide based on method)
      # Updated to match UI naming convention
      input_suffixes <- c(
        "_min_",
        "_rate_",
        "_flat_",
        "_vb1_max_",
        "_vb1_tax_",
        "_vb2_max_",
        "_vb2_tax_",
        "_vb3_tax_",
        "_ab1_max_",
        "_ab1_tax_",
        "_ab2_max_",
        "_ab2_tax_",
        "_ab3_tax_"
      )

      for (suffix in input_suffixes) {
        input_id <- paste0(
          "bus_subcat_",
          subcategory_safe,
          suffix,
          scenario_suffix
        )
        value <- input[[input_id]]

        if (!is.null(value)) {
          # Clean up the suffix name for storage
          param_name <- gsub("^_|_$", "", suffix)
          subcat_config[[param_name]] <- value
        }
      }

      config$business_license[[subcategory]] <- subcat_config
    }
  }

  return(config)
}

# Function to save configuration to JSON file
save_module3_config <- function(config, scenario_name = NULL) {
  if (is.null(scenario_name)) {
    scenario_name <- config$scenario
  }

  # Convert to JSON with pretty formatting
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE, pretty = TRUE)

  # Create filename with timestamp
  filename <- paste0(
    "module3_",
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
load_module3_config <- function(filepath) {
  tryCatch(
    {
      # Read JSON from file
      config_json <- readLines(filepath, warn = FALSE)
      config_json <- paste(config_json, collapse = "\n")

      # Parse JSON back to list
      config <- jsonlite::fromJSON(config_json, simplifyVector = FALSE)

      # Validate it's a Module 3 config
      if (is.null(config$module) || config$module != "module3") {
        stop("This is not a valid Module 3 configuration file")
      }

      return(config)
    },
    error = function(e) {
      stop(paste("Error loading configuration:", e$message))
    }
  )
}

# Function to apply loaded configuration to UI inputs
apply_module3_config <- function(
  session,
  config,
  scenario_suffix,
  business_subcategories
) {
  # Apply property tax settings for each type
  for (prop_type in c("domestic", "commercial", "institutional")) {
    if (!is.null(config$property_tax[[prop_type]])) {
      prop_config <- config$property_tax[[prop_type]]

      # Update basic settings
      if (!is.null(prop_config$min_tax)) {
        updateNumericInput(
          session,
          paste0(prop_type, "_min_", scenario_suffix),
          value = prop_config$min_tax
        )
      }
      if (!is.null(prop_config$rate)) {
        updateNumericInput(
          session,
          paste0(prop_type, "_rate_", scenario_suffix),
          value = prop_config$rate
        )
      }

      # Update use_slots checkbox
      if (!is.null(prop_config$use_slots)) {
        updateCheckboxInput(
          session,
          paste0("use_slots_", prop_type, "_", scenario_suffix),
          value = prop_config$use_slots
        )
      }

      # Update slot settings if they exist
      if (!is.null(prop_config$slots)) {
        for (slot in 1:3) {
          slot_name <- paste0("slot", slot)
          if (!is.null(prop_config$slots[[slot_name]])) {
            slot_config <- prop_config$slots[[slot_name]]

            if (!is.null(slot_config$min)) {
              updateNumericInput(
                session,
                paste0(prop_type, "_slot", slot, "_min_", scenario_suffix),
                value = slot_config$min
              )
            }
            if (!is.null(slot_config$max) && slot < 3) {
              # No max for slot 3
              updateNumericInput(
                session,
                paste0(prop_type, "_slot", slot, "_max_", scenario_suffix),
                value = slot_config$max
              )
            }
            if (!is.null(slot_config$min_tax)) {
              updateNumericInput(
                session,
                paste0(prop_type, "_slot", slot, "_min_tax_", scenario_suffix),
                value = slot_config$min_tax
              )
            }
            if (!is.null(slot_config$rate)) {
              updateNumericInput(
                session,
                paste0(prop_type, "_slot", slot, "_rate_", scenario_suffix),
                value = slot_config$rate
              )
            }
          }
        }
      }
    }
  }

  # Apply business license settings
  if (!is.null(config$business_license) && !is.null(business_subcategories)) {
    for (subcategory in business_subcategories) {
      subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)

      if (!is.null(config$business_license[[subcategory]])) {
        subcat_config <- config$business_license[[subcategory]]

        # Update method selection
        if (!is.null(subcat_config$method)) {
          method_id <- paste0(
            "bus_subcat_",
            subcategory_safe,
            "_method_",
            scenario_suffix
          )
          updateSelectInput(session, method_id, selected = subcat_config$method)
        }

        # Update all input values - matches UI naming convention
        param_to_suffix <- list(
          "min" = "_min_",
          "rate" = "_rate_",
          "flat" = "_flat_",
          "vb1_max" = "_vb1_max_",
          "vb1_tax" = "_vb1_tax_",
          "vb2_max" = "_vb2_max_",
          "vb2_tax" = "_vb2_tax_",
          "vb3_tax" = "_vb3_tax_",
          "ab1_max" = "_ab1_max_",
          "ab1_tax" = "_ab1_tax_",
          "ab2_max" = "_ab2_max_",
          "ab2_tax" = "_ab2_tax_",
          "ab3_tax" = "_ab3_tax_"
        )

        for (param in names(param_to_suffix)) {
          if (!is.null(subcat_config[[param]])) {
            input_id <- paste0(
              "bus_subcat_",
              subcategory_safe,
              param_to_suffix[[param]],
              scenario_suffix
            )
            updateNumericInput(
              session,
              input_id,
              value = subcat_config[[param]]
            )
          }
        }
      }
    }
  }
}
