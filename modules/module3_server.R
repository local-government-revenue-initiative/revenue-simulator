# modules/module3_server.R
# Module 3: Tax Parameters - Updated to use parameter tables

module3_server <- function(
  id,
  processed_data,
  property_configs,
  calculated_property_values,
  param_tax_min_rate = reactive(NULL),
  param_license = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ==========================================================================
    # REACTIVE VALUES
    # ==========================================================================
    values <- reactiveValues(
      # Default configurations from parameters
      default_property_tax = NULL,
      default_license = NULL,

      # Subcategories found in data
      business_subcategories = NULL,
      business_categories = NULL,

      # Preview data
      preview_data = NULL,
      business_preview_data = NULL
    )

    # ==========================================================================
    # INITIALIZATION - Build defaults from parameter tables
    # ==========================================================================

    # Build property tax defaults when param_tax_min_rate is available
    observe({
      if (!is.null(param_tax_min_rate())) {
        values$default_property_tax <- build_property_tax_defaults(param_tax_min_rate())
        cat("Property tax defaults loaded from parameters\n")
      } else {
        values$default_property_tax <- build_property_tax_defaults(NULL)
      }
    })

    # Build business license defaults when param_license is available
    observe({
      if (!is.null(param_license())) {
        values$default_license <- build_business_license_defaults(param_license())
        cat(
          "Business license defaults loaded from parameters:",
          length(values$default_license),
          "subcategories\n"
        )
      } else {
        values$default_license <- build_business_license_defaults(NULL)
      }
    })

    # Extract business subcategories from param_license
    observe({
      req(param_license())
      values$business_subcategories <- get_license_subcategories(param_license())
      cat(
        "Business subcategories from param_license:",
        length(values$business_subcategories),
        "\n"
      )
      
      # Also extract business categories from param_license
      if ("business_category" %in% names(param_license())) {
        values$business_categories <- unique(param_license()$business_category)
      }
    })

    # ==========================================================================
    # HELPER - Get property tax default for a type
    # ==========================================================================

    get_property_tax_default <- function(property_type) {
      prop_type <- tolower(property_type)
      if (
        !is.null(values$default_property_tax) &&
          prop_type %in% names(values$default_property_tax)
      ) {
        return(values$default_property_tax[[prop_type]])
      }
      # Fallback defaults
      list(use_slots = FALSE, minimum = 200, rate = 0.025)
    }

    # ==========================================================================
    # DYNAMIC UI - Property Tax Configuration
    # ==========================================================================

    generate_property_tax_ui <- function(scenario_suffix) {
      tagList(
        lapply(
          c("domestic", "commercial", "institutional"),
          function(prop_type) {
            defaults <- get_property_tax_default(prop_type)
            prop_type_title <- tools::toTitleCase(prop_type)

            box(
              title = paste(prop_type_title, "Property Tax"),
              width = 12,
              status = if (prop_type == "commercial") "warning" else "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,

              # Use slots toggle
              checkboxInput(
                ns(paste0("use_slots_", prop_type, "_", scenario_suffix)),
                "Use value-based slots",
                value = defaults$use_slots %||% FALSE
              ),

              # Simple configuration (when not using slots)
              conditionalPanel(
                condition = paste0(
                  "!input['",
                  ns(paste0("use_slots_", prop_type, "_", scenario_suffix)),
                  "']"
                ),
                fluidRow(
                  column(
                    6,
                    numericInput(
                      ns(paste0(prop_type, "_min_", scenario_suffix)),
                      "Minimum Tax:",
                      value = defaults$minimum %||% 200,
                      min = 0
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      ns(paste0(prop_type, "_rate_", scenario_suffix)),
                      "Tax Rate (%):",
                      value = (defaults$rate %||% 0.025) * 100,
                      min = 0,
                      max = 100,
                      step = 0.1
                    )
                  )
                )
              ),

              # Slot configuration (when using slots)
              conditionalPanel(
                condition = paste0(
                  "input['",
                  ns(paste0("use_slots_", prop_type, "_", scenario_suffix)),
                  "']"
                ),
                h5("Value Slot Configuration"),
                # Slot 1
                fluidRow(
                  column(3, h6("Slot 1")),
                  column(
                    3,
                    numericInput(
                      ns(paste0(prop_type, "_slot1_max_", scenario_suffix)),
                      "Max Value:",
                      value = 10000,
                      min = 0
                    )
                  ),
                  column(
                    3,
                    numericInput(
                      ns(paste0(prop_type, "_slot1_min_", scenario_suffix)),
                      "Min Tax:",
                      value = 100,
                      min = 0
                    )
                  ),
                  column(
                    3,
                    numericInput(
                      ns(paste0(prop_type, "_slot1_rate_", scenario_suffix)),
                      "Rate (%):",
                      value = 2,
                      min = 0,
                      step = 0.1
                    )
                  )
                ),
                # Slot 2
                fluidRow(
                  column(3, h6("Slot 2")),
                  column(
                    3,
                    numericInput(
                      ns(paste0(prop_type, "_slot2_max_", scenario_suffix)),
                      "Max Value:",
                      value = 50000,
                      min = 0
                    )
                  ),
                  column(
                    3,
                    numericInput(
                      ns(paste0(prop_type, "_slot2_min_", scenario_suffix)),
                      "Min Tax:",
                      value = 200,
                      min = 0
                    )
                  ),
                  column(
                    3,
                    numericInput(
                      ns(paste0(prop_type, "_slot2_rate_", scenario_suffix)),
                      "Rate (%):",
                      value = 3,
                      min = 0,
                      step = 0.1
                    )
                  )
                ),
                # Slot 3
                fluidRow(
                  column(3, h6("Slot 3")),
                  column(3, p("Above Slot 2")),
                  column(
                    3,
                    numericInput(
                      ns(paste0(prop_type, "_slot3_min_", scenario_suffix)),
                      "Min Tax:",
                      value = 400,
                      min = 0
                    )
                  ),
                  column(
                    3,
                    numericInput(
                      ns(paste0(prop_type, "_slot3_rate_", scenario_suffix)),
                      "Rate (%):",
                      value = 4,
                      min = 0,
                      step = 0.1
                    )
                  )
                )
              )
            )
          }
        )
      )
    }

    # Render property tax UIs
    output$property_tax_ui_existing <- renderUI({
      req(values$default_property_tax)
      generate_property_tax_ui("existing")
    })

    output$property_tax_ui_scenario_a <- renderUI({
      req(values$default_property_tax)
      generate_property_tax_ui("scenario_a")
    })

    output$property_tax_ui_scenario_b <- renderUI({
      req(values$default_property_tax)
      generate_property_tax_ui("scenario_b")
    })

    # ==========================================================================
    # DYNAMIC UI - Business License Configuration
    # ==========================================================================

    generate_business_license_ui <- function(scenario_suffix) {
      req(values$business_subcategories, values$default_license)
      
      if (length(values$business_subcategories) == 0) {
        return(p("No business subcategories found in data."))
      }
      
      # Group subcategories by category
      param_data <- param_license()
      if (is.null(param_data)) {
        return(p("License parameters not loaded."))
      }
      
      categories <- unique(param_data$business_category)
      
      tagList(
        lapply(categories, function(cat) {
          # Get subcategories for this category
          subcats <- param_data$business_sub_category[
            param_data$business_category == cat
          ]
          
          box(
            title = paste0(cat, " (", length(subcats), " subcategories)"),
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,  # Start collapsed!
            status = "info",
            solidHeader = FALSE,
            
            lapply(subcats, function(subcategory) {
              # Get defaults for this subcategory
              defaults <- get_subcategory_defaults(
                subcategory,
                values$default_license
              )
              generate_business_license_ui_element(
                ns,
                subcategory,
                scenario_suffix,
                defaults
              )
            })
          )
        })
      )
    }

    # Helper to generate single subcategory UI element
    generate_business_license_ui_element <- function(
      ns,
      subcategory,
      scenario_suffix,
      defaults
    ) {
      subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)

      wellPanel(
        style = "margin-bottom: 10px;",
        h6(subcategory, style = "font-weight: bold; color: #337ab7;"),

        selectInput(
          ns(paste0(
            "bus_subcat_",
            subcategory_safe,
            "_method_",
            scenario_suffix
          )),
          "Calculation Method:",
          choices = c(
            "Minimum and rate" = "minimum_rate",
            "Flat amount" = "flat",
            "Value bands" = "flat_value_bands",
            "Area bands" = "flat_area_bands"
          ),
          selected = defaults$calculation_method
        ),

        # Minimum + Rate
        conditionalPanel(
          condition = paste0(
            "input['",
            ns(paste0(
              "bus_subcat_",
              subcategory_safe,
              "_method_",
              scenario_suffix
            )),
            "'] == 'minimum_rate'"
          ),
          fluidRow(
            column(
              6,
              numericInput(
                ns(paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_min_",
                  scenario_suffix
                )),
                "Minimum:",
                value = defaults$minimum,
                min = 0
              )
            ),
            column(
              6,
              numericInput(
                ns(paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_rate_",
                  scenario_suffix
                )),
                "Rate (%):",
                value = defaults$rate,
                min = 0,
                step = 0.1
              )
            )
          )
        ),

        # Flat amount
        conditionalPanel(
          condition = paste0(
            "input['",
            ns(paste0(
              "bus_subcat_",
              subcategory_safe,
              "_method_",
              scenario_suffix
            )),
            "'] == 'flat'"
          ),
          numericInput(
            ns(paste0(
              "bus_subcat_",
              subcategory_safe,
              "_flat_",
              scenario_suffix
            )),
            "Flat Amount:",
            value = defaults$flat_amount,
            min = 0
          )
        ),

        # Value bands
        conditionalPanel(
          condition = paste0(
            "input['",
            ns(paste0(
              "bus_subcat_",
              subcategory_safe,
              "_method_",
              scenario_suffix
            )),
            "'] == 'flat_value_bands'"
          ),
          fluidRow(
            column(
              6,
              numericInput(
                ns(paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_vb1_max_",
                  scenario_suffix
                )),
                "Band 1 Max:",
                value = 10000,
                min = 0
              )
            ),
            column(
              6,
              numericInput(
                ns(paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_vb1_tax_",
                  scenario_suffix
                )),
                "Band 1 Tax:",
                value = 500,
                min = 0
              )
            )
          ),
          fluidRow(
            column(
              6,
              numericInput(
                ns(paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_vb2_max_",
                  scenario_suffix
                )),
                "Band 2 Max:",
                value = 50000,
                min = 0
              )
            ),
            column(
              6,
              numericInput(
                ns(paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_vb2_tax_",
                  scenario_suffix
                )),
                "Band 2 Tax:",
                value = 1000,
                min = 0
              )
            )
          ),
          numericInput(
            ns(paste0(
              "bus_subcat_",
              subcategory_safe,
              "_vb3_tax_",
              scenario_suffix
            )),
            "Band 3 Tax (above Band 2):",
            value = 2000,
            min = 0
          )
        ),

        # Area bands
        conditionalPanel(
          condition = paste0(
            "input['",
            ns(paste0(
              "bus_subcat_",
              subcategory_safe,
              "_method_",
              scenario_suffix
            )),
            "'] == 'flat_area_bands'"
          ),
          fluidRow(
            column(
              6,
              numericInput(
                ns(paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_ab1_max_",
                  scenario_suffix
                )),
                "Band 1 Max Area:",
                value = 50,
                min = 0
              )
            ),
            column(
              6,
              numericInput(
                ns(paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_ab1_tax_",
                  scenario_suffix
                )),
                "Band 1 Tax:",
                value = 500,
                min = 0
              )
            )
          ),
          fluidRow(
            column(
              6,
              numericInput(
                ns(paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_ab2_max_",
                  scenario_suffix
                )),
                "Band 2 Max Area:",
                value = 200,
                min = 0
              )
            ),
            column(
              6,
              numericInput(
                ns(paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_ab2_tax_",
                  scenario_suffix
                )),
                "Band 2 Tax:",
                value = 1000,
                min = 0
              )
            )
          ),
          numericInput(
            ns(paste0(
              "bus_subcat_",
              subcategory_safe,
              "_ab3_tax_",
              scenario_suffix
            )),
            "Band 3 Tax (above Band 2):",
            value = 2000,
            min = 0
          )
        )
      )
    }

    # Render business license UIs
    output$business_license_ui_existing <- renderUI({
      req(values$business_subcategories, values$default_license)
      div(
        style = "max-height: 500px; overflow-y: auto;",
        generate_business_license_ui("existing")
      )
    })

    output$business_license_ui_scenario_a <- renderUI({
      req(values$business_subcategories, values$default_license)
      div(
        style = "max-height: 500px; overflow-y: auto;",
        generate_business_license_ui("scenario_a")
      )
    })

    output$business_license_ui_scenario_b <- renderUI({
      req(values$business_subcategories, values$default_license)
      div(
        style = "max-height: 500px; overflow-y: auto;",
        generate_business_license_ui("scenario_b")
      )
    })

    # ==========================================================================
    # CONFIGURATION COLLECTION HELPERS
    # ==========================================================================

    # Collect property tax configuration for a scenario
    collect_property_tax_config <- function(scenario) {
      config <- list()

      for (prop_type in c("domestic", "commercial", "institutional")) {
        use_slots <- input[[paste0("use_slots_", prop_type, "_", scenario)]]

        if (is.null(use_slots) || !use_slots) {
          # Simple configuration
          min_val <- input[[paste0(prop_type, "_min_", scenario)]]
          rate_val <- input[[paste0(prop_type, "_rate_", scenario)]]

          config[[prop_type]] <- list(
            use_slots = FALSE,
            minimum = min_val %||% 200,
            rate = (rate_val %||% 2.5) / 100 # Convert from percentage
          )
        } else {
          # Slot-based configuration
          config[[prop_type]] <- list(
            use_slots = TRUE,
            slots = list(
              slot1 = list(
                min = 0,
                max = input[[paste0(prop_type, "_slot1_max_", scenario)]] %||%
                  10000,
                minimum = input[[paste0(
                  prop_type,
                  "_slot1_min_",
                  scenario
                )]] %||%
                  100,
                rate = (input[[paste0(
                  prop_type,
                  "_slot1_rate_",
                  scenario
                )]] %||%
                  2) /
                  100
              ),
              slot2 = list(
                min = input[[paste0(prop_type, "_slot1_max_", scenario)]] %||%
                  10000,
                max = input[[paste0(prop_type, "_slot2_max_", scenario)]] %||%
                  50000,
                minimum = input[[paste0(
                  prop_type,
                  "_slot2_min_",
                  scenario
                )]] %||%
                  200,
                rate = (input[[paste0(
                  prop_type,
                  "_slot2_rate_",
                  scenario
                )]] %||%
                  3) /
                  100
              ),
              slot3 = list(
                min = input[[paste0(prop_type, "_slot2_max_", scenario)]] %||%
                  50000,
                max = Inf,
                minimum = input[[paste0(
                  prop_type,
                  "_slot3_min_",
                  scenario
                )]] %||%
                  400,
                rate = (input[[paste0(
                  prop_type,
                  "_slot3_rate_",
                  scenario
                )]] %||%
                  4) /
                  100
              )
            )
          )
        }
      }

      return(config)
    }

    # Collect business license configuration for a scenario
    collect_business_license_config <- function(scenario) {
      config <- list()

      if (is.null(values$business_subcategories)) {
        return(config)
      }

      for (subcategory in values$business_subcategories) {
        subcategory_safe <- gsub("[^A-Za-z0-9_]", "_", subcategory)

        method <- input[[paste0(
          "bus_subcat_",
          subcategory_safe,
          "_method_",
          scenario
        )]]
        if (is.null(method)) {
          method <- "minimum_rate"
        }

        if (method == "minimum_rate") {
          min_val <- input[[paste0(
            "bus_subcat_",
            subcategory_safe,
            "_min_",
            scenario
          )]]
          rate_val <- input[[paste0(
            "bus_subcat_",
            subcategory_safe,
            "_rate_",
            scenario
          )]]

          config[[subcategory]] <- list(
            calculation_method = "minimum_rate",
            minimum = min_val %||% 350,
            rate = (rate_val %||% 3.5) / 100
          )
        } else if (method == "flat") {
          flat_val <- input[[paste0(
            "bus_subcat_",
            subcategory_safe,
            "_flat_",
            scenario
          )]]

          config[[subcategory]] <- list(
            calculation_method = "flat",
            flat_amount = flat_val %||% 1000
          )
        } else if (method == "flat_value_bands") {
          config[[subcategory]] <- list(
            calculation_method = "flat_value_bands",
            value_bands = list(
              band1 = list(
                max = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_vb1_max_",
                  scenario
                )]] %||%
                  10000,
                tax = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_vb1_tax_",
                  scenario
                )]] %||%
                  500
              ),
              band2 = list(
                max = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_vb2_max_",
                  scenario
                )]] %||%
                  50000,
                tax = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_vb2_tax_",
                  scenario
                )]] %||%
                  1000
              ),
              band3 = list(
                tax = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_vb3_tax_",
                  scenario
                )]] %||%
                  2000
              )
            )
          )
        } else if (method == "flat_area_bands") {
          config[[subcategory]] <- list(
            calculation_method = "flat_area_bands",
            area_bands = list(
              band1 = list(
                max = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_ab1_max_",
                  scenario
                )]] %||%
                  50,
                tax = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_ab1_tax_",
                  scenario
                )]] %||%
                  500
              ),
              band2 = list(
                max = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_ab2_max_",
                  scenario
                )]] %||%
                  200,
                tax = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_ab2_tax_",
                  scenario
                )]] %||%
                  1000
              ),
              band3 = list(
                tax = input[[paste0(
                  "bus_subcat_",
                  subcategory_safe,
                  "_ab3_tax_",
                  scenario
                )]] %||%
                  2000
              )
            )
          )
        }
      }

      return(config)
    }

    # ==========================================================================
    # PREVIEW CALCULATIONS
    # ==========================================================================

    # Get precalculated property values from Module 2
    get_precalculated_values <- function(scenario, data, n_rows) {
      calc_values <- calculated_property_values()

      if (is.null(calc_values) || is.null(calc_values[[scenario]])) {
        # No precalculated values - return NAs
        return(list(
          property_values = rep(NA, n_rows),
          business_values = rep(NA, n_rows)
        ))
      }

      scenario_values <- calc_values[[scenario]]

      # Match by id_property
      matched_indices <- match(data$id_property, scenario_values$id_property)

      property_values <- scenario_values$property_value[matched_indices]
      business_values <- scenario_values$business_value[matched_indices]

      # Handle NAs
      property_values[is.na(property_values)] <- 0
      business_values[is.na(business_values)] <- 0

      list(
        property_values = property_values,
        business_values = business_values
      )
    }

    # Property tax preview calculation
    observeEvent(input$calculate_property_preview, {
      req(processed_data())

      data <- processed_data()
      scenario <- input$property_preview_scenario %||% "existing"
      n_rows <- min(input$property_preview_rows %||% 100, nrow(data))

      withProgress(message = "Calculating property tax preview...", value = 0, {
        preview_data <- data[1:n_rows, ]

        incProgress(0.3, detail = "Getting property values...")

        values_result <- get_precalculated_values(
          scenario,
          preview_data,
          n_rows
        )
        property_values <- values_result$property_values

        incProgress(0.3, detail = "Calculating taxes...")

        # Get property types
        property_types <- if ("property_type" %in% names(preview_data)) {
          tolower(preview_data$property_type)
        } else {
          rep("domestic", n_rows)
        }

        # Get tax configuration
        tax_config <- collect_property_tax_config(scenario)

        # Calculate taxes
        property_taxes <- numeric(n_rows)
        tax_rates <- numeric(n_rows)

        for (i in 1:n_rows) {
          if (is.na(property_values[i]) || property_values[i] <= 0) {
            property_taxes[i] <- 0
            tax_rates[i] <- 0
          } else {
            result <- calculate_property_tax(
              property_values[i],
              property_types[i],
              tax_config
            )
            property_taxes[i] <- result$tax_amount
            tax_rates[i] <- result$rate_used * 100
          }
        }

        incProgress(0.3, detail = "Creating preview table...")

        values$preview_data <- data.frame(
          id_property = preview_data$id_property,
          property_type = property_types,
          property_value = round(property_values, 2),
          tax_rate = round(tax_rates, 2),
          property_tax = round(property_taxes, 2),
          stringsAsFactors = FALSE
        )

        incProgress(0.1, detail = "Done!")
      })

      showNotification("Property tax preview calculated", type = "message")
    })

    # Business license preview calculation
    observeEvent(input$calculate_business_preview, {
      req(processed_data())

      data <- processed_data()
      scenario <- input$business_preview_scenario %||% "existing"
      n_rows <- min(input$business_preview_rows %||% 100, nrow(data))

      withProgress(
        message = "Calculating business license preview...",
        value = 0,
        {
          preview_data <- data[1:n_rows, ]

          incProgress(0.3, detail = "Getting business values...")

          values_result <- get_precalculated_values(
            scenario,
            preview_data,
            n_rows
          )
          business_values <- values_result$business_values

          incProgress(0.3, detail = "Calculating licenses...")

          # Get business info
          business_subcategories <- if (
            "business_sub_category" %in% names(preview_data)
          ) {
            preview_data$business_sub_category
          } else {
            rep(NA, n_rows)
          }

          business_areas <- if ("business_area" %in% names(preview_data)) {
            preview_data$business_area
          } else {
            rep(0, n_rows)
          }

          # Get license configuration
          license_config <- collect_business_license_config(scenario)

          # Calculate licenses
          business_licenses <- numeric(n_rows)

          for (i in 1:n_rows) {
            if (
              is.na(business_subcategories[i]) ||
                is.na(business_values[i]) ||
                business_values[i] <= 0
            ) {
              business_licenses[i] <- 0
            } else {
              subcat_config <- license_config[[business_subcategories[i]]]

              if (!is.null(subcat_config)) {
                if (subcat_config$calculation_method == "minimum_rate") {
                  business_licenses[i] <- max(
                    business_values[i] * subcat_config$rate,
                    subcat_config$minimum
                  )
                } else if (subcat_config$calculation_method == "flat") {
                  business_licenses[i] <- subcat_config$flat_amount
                } else if (
                  subcat_config$calculation_method == "flat_value_bands"
                ) {
                  bands <- subcat_config$value_bands
                  if (business_values[i] <= bands$band1$max) {
                    business_licenses[i] <- bands$band1$tax
                  } else if (business_values[i] <= bands$band2$max) {
                    business_licenses[i] <- bands$band2$tax
                  } else {
                    business_licenses[i] <- bands$band3$tax
                  }
                } else if (
                  subcat_config$calculation_method == "flat_area_bands"
                ) {
                  bands <- subcat_config$area_bands
                  if (business_areas[i] <= bands$band1$max) {
                    business_licenses[i] <- bands$band1$tax
                  } else if (business_areas[i] <= bands$band2$max) {
                    business_licenses[i] <- bands$band2$tax
                  } else {
                    business_licenses[i] <- bands$band3$tax
                  }
                }
              }
            }
          }

          incProgress(0.3, detail = "Creating preview table...")

          values$business_preview_data <- data.frame(
            id_property = preview_data$id_property,
            business_sub_category = business_subcategories,
            business_area = round(business_areas, 2),
            business_value = round(business_values, 2),
            business_license = round(business_licenses, 2),
            stringsAsFactors = FALSE
          )

          incProgress(0.1, detail = "Done!")
        }
      )

      showNotification("Business license preview calculated", type = "message")
    })

    # Preview table outputs
    output$property_preview_table <- DT::renderDataTable({
      req(values$preview_data)
      DT::datatable(
        values$preview_data,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      )
    })

    output$business_preview_table <- DT::renderDataTable({
      req(values$business_preview_data)
      DT::datatable(
        values$business_preview_data,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      )
    })

    # ==========================================================================
    # RETURN - Configurations for downstream modules
    # ==========================================================================

    get_all_tax_configs <- reactive({
      configs <- list()

      for (scenario in c("existing", "scenario_a", "scenario_b")) {
        configs[[scenario]] <- list(
          property_tax = collect_property_tax_config(scenario),
          business_license = collect_business_license_config(scenario)
        )
      }

      configs
    })

    return(get_all_tax_configs)
  })
}
