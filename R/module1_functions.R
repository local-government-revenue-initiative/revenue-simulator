# R/module1_functions.R

# Function to read and validate CSV files
read_csv_safe <- function(file_path) {
  tryCatch({
    data <- read_csv(file_path, show_col_types = FALSE)
    return(list(success = TRUE, data = data, error = NULL))
  }, error = function(e) {
    return(list(success = FALSE, data = NULL, error = as.character(e)))
  })
}

# Function to get column mapping suggestions based on column names
suggest_column_mapping <- function(df, expected_columns) {
  col_names <- names(df)
  suggestions <- list()
  
  for (expected in expected_columns) {
    # Find best match using string similarity
    matches <- agrep(expected, col_names, ignore.case = TRUE, value = TRUE)
    suggestions[[expected]] <- if(length(matches) > 0) matches[1] else ""
  }
  
  return(suggestions)
}

# Function to validate column mappings
validate_column_mapping <- function(df, mapping, required_columns) {
  errors <- character()
  
  # Check required columns
  for (col in required_columns) {
    if (is.null(mapping[[col]]) || mapping[[col]] == "" || !mapping[[col]] %in% names(df)) {
      errors <- c(errors, paste("Required column", col, "not mapped or invalid"))
    }
  }
  
  # Check for duplicate mappings
  mapped_columns <- unlist(mapping)
  mapped_columns <- mapped_columns[mapped_columns != ""]  # Remove empty mappings
  
  if (length(mapped_columns) != length(unique(mapped_columns))) {
    duplicates <- mapped_columns[duplicated(mapped_columns)]
    errors <- c(errors, paste("Duplicate column mappings found:", 
                            paste(unique(duplicates), collapse = ", ")))
  }
  
  return(errors)
}

# Function to create dummy variables for categorical columns
create_dummy_variables <- function(df, categorical_columns) {
  # Remove NA values and convert to factors
  for (col in categorical_columns) {
    if (col %in% names(df)) {
      df[[col]] <- as.factor(df[[col]])
    }
  }
  
  # Create dummy variables
  df_dummy <- dummy_cols(df, 
                         select_columns = categorical_columns,
                         remove_first_dummy = FALSE,
                         remove_selected_columns = TRUE)
  
  return(df_dummy)
}

# Function to process property data
process_property_data <- function(df, column_mapping) {
  # First, remove any empty mappings and check for duplicates
  clean_mapping <- column_mapping[column_mapping != ""]
  
  # Check for duplicate values in mapping
  if (any(duplicated(clean_mapping))) {
    stop("Duplicate column mappings detected. Each CSV column can only be mapped once.")
  }
  
  # Rename columns based on mapping
  df_renamed <- df
  for (standard_name in names(clean_mapping)) {
    old_name <- clean_mapping[[standard_name]]
    if (!is.null(old_name) && old_name != "" && old_name %in% names(df_renamed)) {
      # Only rename if the column exists and hasn't been renamed already
      if (old_name %in% names(df_renamed)) {
        df_renamed <- df_renamed %>%
          rename(!!standard_name := !!old_name)
      }
    }
  }
  
  # Identify categorical columns for dummy variable creation
  categorical_cols <- c("property_type", "commercial_type", "institutional_type",
                        "street_access", "drainage", "potential_to_build",
                        "wall_material", "wall_condition", "has_veranda",
                        "roof_material", "roof_condition", "window_material",
                        "aid_conditioning", "has_security", "has_pool",
                        "has_outbuilding", "street_quality", "domestic_use_of_groundfloor",
                        "street_lanes", "tourist_area", "environmental_hazard",
                        "main_road_high_visibility", "informal_settlement",
                        "commercial_corridor", "has_water", "ward")  
  
  # Filter to only categorical columns that exist
  categorical_cols <- categorical_cols[categorical_cols %in% names(df_renamed)]
  
  # Create dummy variables
  df_processed <- create_dummy_variables(df_renamed, categorical_cols)
  
  return(df_processed)
}

# Updated merge_datasets function to handle many-to-many relationships properly
merge_datasets <- function(property_data, payment_data, business_data,
                           property_id_col, payment_id_col, business_id_col) {
  # Ensure ID columns are character type
  property_data[[property_id_col]] <- as.character(property_data[[property_id_col]])
  payment_data[[payment_id_col]] <- as.character(payment_data[[payment_id_col]])
  business_data[[business_id_col]] <- as.character(business_data[[business_id_col]])
  
  # Add unique business ID to prevent unwanted duplication
  if (nrow(business_data) > 0) {
    business_data <- business_data %>%
      mutate(business_unique_id = row_number())
  }
  
  # Issue 1: Deduplicate payment data (in case of duplicates)
  payment_data_unique <- payment_data %>%
    distinct(!!sym(payment_id_col), .keep_all = TRUE)
  
  # Join property and payment data
  merged_data <- property_data %>%
    left_join(payment_data_unique, 
              by = setNames(payment_id_col, property_id_col))
  
  # Issue 2: Handle business data join more carefully
  if (nrow(business_data) > 0) {
    if ("property_type" %in% names(merged_data)) {
      # Complex case: Properties have types, need smart matching
      
      # Create a priority mapping for property types
      type_priority_map <- data.frame(
        property_type = c("Commercial", "commercial", "COMMERCIAL",
                          "Domestic", "domestic", "DOMESTIC",
                          "Institutional", "institutional", "INSTITUTIONAL"),
        type_priority = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
        stringsAsFactors = FALSE
      )
      
      # Add priority to merged data
      merged_with_priority <- merged_data %>%
        mutate(row_id = row_number()) %>%
        left_join(type_priority_map, by = "property_type") %>%
        mutate(type_priority = ifelse(is.na(type_priority), 99, type_priority))
      
      # For each property_id, identify the best row to attach businesses to
      best_rows <- merged_with_priority %>%
        group_by(!!sym(property_id_col)) %>%
        slice_min(type_priority, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        select(!!sym(property_id_col), best_row_id = row_id)
      
      # Create business assignments
      business_assignments <- business_data %>%
        inner_join(best_rows, by = setNames(property_id_col, business_id_col)) %>%
        select(-all_of(business_id_col))
      
      # Join businesses to the appropriate rows
      final_merged <- merged_with_priority %>%
        left_join(
          business_assignments,
          by = c("row_id" = "best_row_id"),
          relationship = "one-to-many"  # One property row can have multiple businesses
        ) %>%
        select(-row_id, -type_priority, -business_unique_id)
      
      merged_data <- final_merged
      
    } else {
      # Simple case: No property types, just join businesses
      merged_data <- merged_data %>%
        left_join(
          business_data %>% select(-business_unique_id), 
          by = setNames(business_id_col, property_id_col))
    }
  }
  
  return(merged_data)
}

