# modules/module1_server.R

module1_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store data
    values <- reactiveValues(
      property_data = NULL,
      payment_data = NULL,
      business_data = NULL,
      property_mapping = list(),
      payment_mapping = list(),
      business_mapping = list(),
      processed_data = NULL,
      mappings_validated = FALSE
    )
    
    # File upload handlers
    observeEvent(input$property_file, {
      req(input$property_file)
      result <- read_csv_safe(input$property_file$datapath)
      if (result$success) {
        values$property_data <- result$data
        output$property_status <- renderText("✓ File uploaded successfully")
      } else {
        output$property_status <- renderText(paste("✗ Error:", result$error))
      }
    })
    
    observeEvent(input$payment_file, {
      req(input$payment_file)
      result <- read_csv_safe(input$payment_file$datapath)
      if (result$success) {
        values$payment_data <- result$data
        output$payment_status <- renderText("✓ File uploaded successfully")
      } else {
        output$payment_status <- renderText(paste("✗ Error:", result$error))
      }
    })
    
    observeEvent(input$business_file, {
      req(input$business_file)
      result <- read_csv_safe(input$business_file$datapath)
      if (result$success) {
        values$business_data <- result$data
        output$business_status <- renderText("✓ File uploaded successfully")
      } else {
        output$business_status <- renderText(paste("✗ Error:", result$error))
      }
    })
    
    # Check if all files are uploaded
    output$files_uploaded <- reactive({
      !is.null(values$property_data) && 
        !is.null(values$payment_data) && 
        !is.null(values$business_data)
    })
    outputOptions(output, "files_uploaded", suspendWhenHidden = FALSE)
    
    # Generate column mapping UIs
    output$property_mapping_ui <- renderUI({
      req(values$property_data)
      
      property_columns <- c("id_property", "coordinate_lat", "coordinate_lng", 
                            "property_area", "property_type", "commercial_type",
                            "institutional_type", "street_access", "drainage",
                            "potential_to_build", "wall_material", "wall_condition",
                            "has_veranda", "roof_material", "roof_condition",
                            "window_material", "aid_conditioning", "has_security",
                            "has_pool", "has_outbuilding", "street_quality",
                            "domestic_use_of_groundfloor", "street_lanes",
                            "tourist_area", "environmental_hazard",
                            "main_road_high_visibility", "informal_settlement",
                            "commercial_corridor", "has_water", "ward")  # Added ward here
      
      suggestions <- suggest_column_mapping(values$property_data, property_columns)
      
      tagList(
        h5("Map your CSV columns to standard property fields:"),
        p("Required fields are marked with *"),
        br(),
        lapply(property_columns, function(col) {
          is_required <- col == "id_property"
          label <- if(is_required) paste0(col, " *") else col
          
          selectInput(
            ns(paste0("map_prop_", col)),
            label = label,
            choices = c("(none)" = "", names(values$property_data)),  # Fixed here
            selected = suggestions[[col]]
          )
        })
      )
    })
    
    output$payment_mapping_ui <- renderUI({
      req(values$payment_data)
      
      payment_columns <- c("id_property", "made_payment")
      suggestions <- suggest_column_mapping(values$payment_data, payment_columns)
      
      tagList(
        h5("Map your CSV columns to standard payment fields:"),
        p("All fields are required *"),
        br(),
        lapply(payment_columns, function(col) {
          selectInput(
            ns(paste0("map_pay_", col)),
            label = paste0(col, " *"),
            choices = c("(none)" = "", names(values$payment_data)),  # Fixed here
            selected = suggestions[[col]]
          )
        })
      )
    })
    
    output$business_mapping_ui <- renderUI({
      req(values$business_data)
      
      business_columns <- c("id_property", "business_name", "business_area", 
                            "business_category", "business_sub_category")  # Added business_sub_category
      suggestions <- suggest_column_mapping(values$business_data, business_columns)
      
      tagList(
        h5("Map your CSV columns to standard business fields:"),
        p("Required fields are marked with *"),
        br(),
        lapply(business_columns, function(col) {
          is_required <- col %in% c("id_property", "business_area", "business_category")
          label <- if(is_required) paste0(col, " *") else col
          
          selectInput(
            ns(paste0("map_bus_", col)),
            label = label,
            choices = c("(none)" = "", names(values$business_data)),
            selected = suggestions[[col]]
          )
        })
      )
    })
    
    # Validation handlers
    observeEvent(input$validate_property, {
      # Collect mappings
      property_columns <- c("id_property", "coordinate_lat", "coordinate_lng", 
                            "property_area", "property_type", "commercial_type",
                            "institutional_type", "street_access", "drainage",
                            "potential_to_build", "wall_material", "wall_condition",
                            "has_veranda", "roof_material", "roof_condition",
                            "window_material", "aid_conditioning", "has_security",
                            "has_pool", "has_outbuilding", "street_quality",
                            "domestic_use_of_groundfloor", "street_lanes",
                            "tourist_area", "environmental_hazard",
                            "main_road_high_visibility", "informal_settlement",
                            "commercial_corridor", "has_water", "ward")
      
      mapping <- list()
      for (col in property_columns) {
        val <- input[[paste0("map_prop_", col)]]
        if (!is.null(val) && val != "") {
          mapping[[col]] <- val
        }
      }
      
      # Validate
      errors <- validate_column_mapping(values$property_data, mapping, c("id_property"))
      
      if (length(errors) == 0) {
        values$property_mapping <- mapping
        showNotification("Property mapping validated successfully!", 
                         type = "message",  # Changed from "success" to "message"
                         duration = 5)
      } else {
        showNotification(paste("Validation errors:", paste(errors, collapse = ", ")), 
                         type = "error", 
                         duration = 10)
      }
    })
    
    observeEvent(input$validate_payment, {
      mapping <- list(
        id_property = input$map_pay_id_property,
        made_payment = input$map_pay_made_payment
      )
      
      errors <- validate_column_mapping(values$payment_data, mapping, 
                                        c("id_property", "made_payment"))
      
      if (length(errors) == 0) {
        values$payment_mapping <- mapping
        showNotification("Payment mapping validated successfully!", 
                         type = "message",  # Changed from "success" to "message"
                         duration = 5)
      } else {
        showNotification(paste("Validation errors:", paste(errors, collapse = ", ")), 
                         type = "error", 
                         duration = 10)
      }
    })
    
    observeEvent(input$validate_business, {
      mapping <- list(
        id_property = input$map_bus_id_property,
        business_name = input$map_bus_business_name,
        business_area = input$map_bus_business_area,
        business_category = input$map_bus_business_category,
        business_sub_category = input$map_bus_business_sub_category  # Added this line
      )
      
      errors <- validate_column_mapping(values$business_data, mapping, 
                                        c("id_property", "business_area", "business_category"))
      
      if (length(errors) == 0) {
        values$business_mapping <- mapping
        showNotification("Business mapping validated successfully!", 
                         type = "message",
                         duration = 5)
      } else {
        showNotification(paste("Validation errors:", paste(errors, collapse = ", ")), 
                         type = "error", 
                         duration = 10)
      }
    })
    
    # Check if all mappings are validated
    observe({
      values$mappings_validated <- length(values$property_mapping) > 0 &&
        length(values$payment_mapping) > 0 &&
        length(values$business_mapping) > 0
    })
    
    output$mappings_validated <- reactive({
      values$mappings_validated
    })
    outputOptions(output, "mappings_validated", suspendWhenHidden = FALSE)
    
    # Process data
    observeEvent(input$process_data, {
      withProgress(message = 'Processing data...', value = 0, {
        
        incProgress(0.2, detail = "Processing property data...")
        
        # Process property data
        processed_property <- process_property_data(values$property_data, 
                                                    values$property_mapping)
        
        incProgress(0.2, detail = "Processing payment data...")
        
        # Process payment data (simpler - just rename)
        processed_payment <- values$payment_data
        for (std_name in names(values$payment_mapping)) {
          if (values$payment_mapping[[std_name]] != "") {
            processed_payment <- processed_payment %>%
              rename(!!std_name := !!values$payment_mapping[[std_name]])
          }
        }
        
        incProgress(0.2, detail = "Processing business data...")
        
        # Process business data
        processed_business <- values$business_data
        for (std_name in names(values$business_mapping)) {
          if (!is.null(values$business_mapping[[std_name]]) && 
              values$business_mapping[[std_name]] != "") {
            processed_business <- processed_business %>%
              rename(!!std_name := !!values$business_mapping[[std_name]])
          }
        }
        
        incProgress(0.2, detail = "Merging datasets...")
        
        # Merge all datasets
        values$processed_data <- merge_datasets(
          processed_property, 
          processed_payment, 
          processed_business,
          "id_property", "id_property", "id_property"
        )
        
        incProgress(0.2, detail = "Complete!")
      })
      
      output$processing_status <- renderText({
        paste("✓ Data processed successfully!",
              "\nTotal properties:", nrow(values$processed_data),
              "\nTotal columns:", ncol(values$processed_data))
      })
    })
    
    # Check if data is processed
    output$data_processed <- reactive({
      !is.null(values$processed_data)
    })
    outputOptions(output, "data_processed", suspendWhenHidden = FALSE)
    
    # Preview processed data
    output$processed_preview <- DT::renderDataTable({
      req(values$processed_data)
      DT::datatable(
        values$processed_data,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'Bfrtip'
        )
      )
    })
    
    # Data summary
    output$data_summary <- renderPrint({
      req(values$processed_data)
      
      cat("=== Data Summary ===\n\n")
      cat("Total Property Rows:", nrow(values$processed_data), "\n")
      cat("Unique Properties:", length(unique(values$processed_data$id_property)), "\n")
      cat("Total Features:", ncol(values$processed_data), "\n")
      
      # Show property type breakdown
      if ("property_type" %in% names(values$property_mapping)) {
        orig_col <- values$property_mapping[["property_type"]]
        if (!is.null(orig_col) && orig_col != "" && orig_col %in% names(values$property_data)) {
          cat("\nProperty Type Distribution:\n")
          print(table(values$property_data[[orig_col]], useNA = "ifany"))
        }
      }
      
      cat("\n\nPayment Status:\n")
      if ("made_payment" %in% names(values$processed_data)) {
        print(table(values$processed_data$made_payment, useNA = "ifany"))
      }
      
      cat("\n\nBusiness Properties:\n")
      cat("Rows with businesses:", 
          sum(!is.na(values$processed_data$business_category)), "\n")
      
      # Business categories
      if ("business_category" %in% names(values$processed_data)) {
        cat("\nBusiness Categories:\n")
        print(table(values$processed_data$business_category, useNA = "ifany"))
      }
      
      # Business sub-categories
      if ("business_sub_category" %in% names(values$processed_data)) {
        cat("\nBusiness Sub-Categories:\n")
        # Show top 10 sub-categories to avoid too long output
        sub_cat_table <- table(values$processed_data$business_sub_category, useNA = "ifany")
        sub_cat_sorted <- sort(sub_cat_table, decreasing = TRUE)
        print(head(sub_cat_sorted, 10))
        if (length(sub_cat_sorted) > 10) {
          cat("... and", length(sub_cat_sorted) - 10, "more sub-categories\n")
        }
      }
      
      # Add ward distribution if available
      original_ward_col <- values$property_mapping[["ward"]]
      if (!is.null(original_ward_col) && original_ward_col != "") {
        cat("\n\nWard Distribution:\n")
        if (original_ward_col %in% names(values$property_data)) {
          print(table(values$property_data[[original_ward_col]], useNA = "ifany"))
        }
      }
      
      cat("\n\nMissing Values Summary:\n")
      missing_summary <- colSums(is.na(values$processed_data))
      missing_summary <- missing_summary[missing_summary > 0]
      if (length(missing_summary) > 0) {
        print(head(missing_summary, 20))
      } else {
        cat("No missing values found!\n")
      }
    })
    
    # Return processed data for use in other modules
    return(reactive({
      values$processed_data
    }))
  })
}

