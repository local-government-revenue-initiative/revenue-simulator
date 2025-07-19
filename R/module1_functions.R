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
  
  for (col in required_columns) {
    if (is.null(mapping[[col]]) || mapping[[col]] == "" || !mapping[[col]] %in% names(df)) {
      errors <- c(errors, paste("Required column", col, "not mapped or invalid"))
    }
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
  # Rename columns based on mapping
  df_renamed <- df
  for (standard_name in names(column_mapping)) {
    if (column_mapping[[standard_name]] != "") {
      df_renamed <- df_renamed %>%
        rename(!!standard_name := !!column_mapping[[standard_name]])
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
                        "commercial_corridor", "has_water")
  
  # Filter to only categorical columns that exist
  categorical_cols <- categorical_cols[categorical_cols %in% names(df_renamed)]
  
  # Create dummy variables
  df_processed <- create_dummy_variables(df_renamed, categorical_cols)
  
  return(df_processed)
}

# Function to merge all datasets
merge_datasets <- function(property_data, payment_data, business_data,
                           property_id_col, payment_id_col, business_id_col) {
  # Ensure ID columns are character type for merging
  property_data[[property_id_col]] <- as.character(property_data[[property_id_col]])
  payment_data[[payment_id_col]] <- as.character(payment_data[[payment_id_col]])
  business_data[[business_id_col]] <- as.character(business_data[[business_id_col]])
  
  # Merge property and payment data
  merged_data <- property_data %>%
    left_join(payment_data, by = setNames(payment_id_col, property_id_col))
  
  # Merge with business data
  merged_data <- merged_data %>%
    left_join(business_data, by = setNames(business_id_col, property_id_col))
  
  return(merged_data)
}