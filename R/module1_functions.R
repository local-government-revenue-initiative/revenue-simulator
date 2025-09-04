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
  
  # Define common variations for problematic columns
  column_variations <- list(
    coordinate_lng = c("coordinate_lng", "coordinate_long", "longitude", "long", "lng", "lon", 
                       "coord_lng", "coord_long", "x", "coords_x"),
    coordinate_lat = c("coordinate_lat", "latitude", "lat", "coord_lat", "y", "coords_y"),
    ward = c("ward", "ward_id", "ward_number", "ward_no", "ward_num", "district", "area", 
             "zone", "ward_code", "admin_ward"),
    id_property = c("id_property", "property_id", "prop_id", "id", "property_code", 
                    "property_number", "property_ref", "reference"),
    property_area = c("property_area", "area", "size", "area_sqm", "property_size", 
                      "total_area", "land_area", "plot_area", "sqm", "square_meters"),
    made_payment = c("made_payment", "payment", "paid", "payment_made", "payment_status", 
                     "has_paid", "payment_2024", "paid_2024"),
    business_name = c("business_name", "business", "company", "company_name", "name", 
                      "business_title", "establishment"),
    business_area = c("business_area", "business_size", "business_sqm", "shop_area", 
                      "commercial_area", "business_space"),
    business_category = c("business_category", "business_cat", "category", "business_type", 
                          "sector", "industry"),
    business_sub_category = c("business_sub_category", "business_subcat", "sub_category", 
                              "subcategory", "business_sub_type", "sub_type")
  )
  
  # For each expected column, try to find a match
  for (expected in expected_columns) {
    # First, try exact match (case-insensitive)
    exact_match <- col_names[tolower(col_names) == tolower(expected)]
    if (length(exact_match) > 0) {
      suggestions[[expected]] <- exact_match[1]
      next
    }
    
    # Second, check if we have known variations for this column
    if (expected %in% names(column_variations)) {
      variations <- column_variations[[expected]]
      for (variant in variations) {
        # Check for exact match with variation (case-insensitive)
        variant_match <- col_names[tolower(col_names) == tolower(variant)]
        if (length(variant_match) > 0) {
          suggestions[[expected]] <- variant_match[1]
          break
        }
        
        # Check for partial match with variation
        partial_match <- col_names[grepl(variant, col_names, ignore.case = TRUE)]
        if (length(partial_match) > 0) {
          suggestions[[expected]] <- partial_match[1]
          break
        }
      }
      
      # If we found a match through variations, continue to next expected column
      if (!is.null(suggestions[[expected]]) && suggestions[[expected]] != "") {
        next
      }
    }
    
    # Third, try fuzzy matching as fallback
    matches <- agrep(expected, col_names, ignore.case = TRUE, value = TRUE, max.distance = 0.3)
    if (length(matches) > 0) {
      suggestions[[expected]] <- matches[1]
    } else {
      # Fourth, try even more lenient fuzzy matching for short column names
      if (nchar(expected) <= 5) {
        matches <- agrep(expected, col_names, ignore.case = TRUE, value = TRUE, max.distance = 2)
        suggestions[[expected]] <- if(length(matches) > 0) matches[1] else ""
      } else {
        suggestions[[expected]] <- ""
      }
    }
  }
  
  # Final check: Make sure we don't have duplicate suggestions
  # (two different expected columns mapped to the same actual column)
  used_columns <- c()
  for (expected in names(suggestions)) {
    suggested <- suggestions[[expected]]
    if (suggested != "" && suggested %in% used_columns) {
      # This column was already suggested for another field
      # Try to find an alternative
      remaining_cols <- setdiff(col_names, used_columns)
      matches <- agrep(expected, remaining_cols, ignore.case = TRUE, value = TRUE)
      suggestions[[expected]] <- if(length(matches) > 0) matches[1] else ""
    } else if (suggested != "") {
      used_columns <- c(used_columns, suggested)
    }
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

# Updated merge_datasets function to handle many-to-many relationships properly v6
merge_datasets <- function(property_data, payment_data, business_data,
                           property_id_col, payment_id_col, business_id_col) {
  # Ensure ID columns are character type
  property_data[[property_id_col]] <- as.character(property_data[[property_id_col]])
  payment_data[[payment_id_col]] <- as.character(payment_data[[payment_id_col]])
  business_data[[business_id_col]] <- as.character(business_data[[business_id_col]])
  
  # Add unique business ID to track each business through the merge
  if (nrow(business_data) > 0) {
    business_data <- business_data %>%
      mutate(business_unique_id = row_number())
  }
  
  # Deduplicate payment data (in case of duplicates)
  payment_data_unique <- payment_data %>%
    distinct(!!sym(payment_id_col), .keep_all = TRUE)
  
  # Join property and payment data
  merged_data <- property_data %>%
    left_join(payment_data_unique, 
              by = setNames(payment_id_col, property_id_col))
  
  # Handle business data join
  if (nrow(business_data) > 0) {
    if ("property_type" %in% names(merged_data)) {
      # Complex case: Properties have types, businesses need smart assignment
      
      # Step 1: Add type priority to the merged data
      merged_with_priority <- merged_data %>%
        mutate(
          type_priority = case_when(
            tolower(property_type) == "commercial" ~ 1,
            tolower(property_type) == "domestic" ~ 2,
            tolower(property_type) == "institutional" ~ 3,
            TRUE ~ 99
          )
        )
      
      # Step 2: Do the many-to-many join (this WILL create duplicates)
      merged_with_businesses <- merged_with_priority %>%
        left_join(business_data, 
                  by = setNames(business_id_col, property_id_col),
                  relationship = "many-to-many")  # Explicitly acknowledge many-to-many
      
      # Step 3: Clean up duplicates
      # For rows with businesses, keep only the ones with the best (lowest) type_priority
      # For rows without businesses, keep all of them
      
      # Separate rows with and without businesses
      rows_with_business <- merged_with_businesses %>%
        filter(!is.na(business_unique_id))
      
      rows_without_business <- merged_with_businesses %>%
        filter(is.na(business_unique_id)) %>%
        select(-business_unique_id, -type_priority)  # Clean up temp columns
      
      # For rows with businesses, keep only those with the best priority per business
      if (nrow(rows_with_business) > 0) {
        rows_with_business_cleaned <- rows_with_business %>%
          group_by(business_unique_id) %>%
          # Keep only the row with the lowest type_priority for each unique business
          slice_min(type_priority, n = 1, with_ties = FALSE) %>%
          ungroup() %>%
          select(-business_unique_id, -type_priority)  # Clean up temp columns
        
        # Combine the cleaned business rows with the non-business rows
        merged_data <- bind_rows(rows_without_business, rows_with_business_cleaned)
      } else {
        merged_data <- rows_without_business
      }
      
    } else {
      # Simple case: No property_type column, just do the join
      merged_data <- merged_data %>%
        left_join(business_data %>% select(-business_unique_id), 
                  by = setNames(business_id_col, property_id_col),
                  relationship = "many-to-many")
    }
  }
  
  return(merged_data)
}