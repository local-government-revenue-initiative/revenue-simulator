# R/module4_functions.R

# Function to aggregate revenue by property type (without averages)
aggregate_revenue_by_type <- function(revenue_data) {
  revenue_data %>%
    dplyr::group_by(property_type) %>%
    dplyr::summarise(
      count = n(),
      total_property_tax = sum(property_tax, na.rm = TRUE),
      total_business_license = sum(business_license, na.rm = TRUE),
      total_revenue = sum(total_tax, na.rm = TRUE),
      .groups = 'drop'
    )
}

# Function to aggregate revenue by business category (without averages)
aggregate_revenue_by_business <- function(revenue_data) {
  business_data <- revenue_data %>%
    dplyr::filter(!is.na(business_category) & business_category != "")
  
  if (nrow(business_data) == 0) {
    return(data.frame(
      business_category = "No business data",
      count = 0,
      total_license_revenue = 0
    ))
  }
  
  business_data %>%
    dplyr::group_by(business_category) %>%
    dplyr::summarise(
      count = n(),
      total_license_revenue = sum(business_license, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::arrange(desc(total_license_revenue))
}

# Function to aggregate revenue by business subcategory (without averages)
aggregate_revenue_by_subcategory <- function(revenue_data, top_n = 20) {
  business_data <- revenue_data %>%
    dplyr::filter(!is.na(business_sub_category) & business_sub_category != "")
  
  if (nrow(business_data) == 0) {
    return(data.frame(
      business_sub_category = "No business data",
      count = 0,
      total_license_revenue = 0
    ))
  }
  
  business_data %>%
    dplyr::group_by(business_sub_category) %>%
    dplyr::summarise(
      count = n(),
      total_license_revenue = sum(business_license, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::arrange(desc(total_license_revenue)) %>%
    head(top_n)
}

# Function to calculate effective tax rates
calculate_effective_tax_rates <- function(revenue_data) {
  revenue_data %>%
    dplyr::filter(property_value > 0) %>%
    dplyr::mutate(effective_rate = (total_tax / property_value) * 100) %>%
    dplyr::group_by(property_type) %>%
    dplyr::summarise(
      count = n(),
      avg_effective_rate = mean(effective_rate, na.rm = TRUE),
      median_effective_rate = median(effective_rate, na.rm = TRUE),
      min_effective_rate = min(effective_rate, na.rm = TRUE),
      max_effective_rate = max(effective_rate, na.rm = TRUE),
      .groups = 'drop'
    )
}

# Function to create property value quantiles
create_value_quantiles <- function(revenue_data, n_quantiles = 5) {
  # Remove properties with zero or NA values
  valid_data <- revenue_data %>%
    dplyr::filter(!is.na(property_value) & property_value > 0)
  
  if (nrow(valid_data) == 0) {
    return(revenue_data %>% dplyr::mutate(value_quantile = NA))
  }
  
  # Create quantile labels
  quantile_labels <- paste0("Q", 1:n_quantiles, " (", 
                            seq(0, 100-100/n_quantiles, 100/n_quantiles), "-",
                            seq(100/n_quantiles, 100, 100/n_quantiles), "%)")
  
  # Calculate quantile breaks
  breaks <- quantile(valid_data$property_value, 
                     probs = seq(0, 1, 1/n_quantiles),
                     na.rm = TRUE)
  
  # Handle edge cases where all values might be the same
  if (length(unique(breaks)) < 2) {
    valid_data$value_quantile <- quantile_labels[1]
  } else {
    valid_data$value_quantile <- cut(valid_data$property_value,
                                     breaks = breaks,
                                     labels = quantile_labels,
                                     include.lowest = TRUE)
  }
  
  # Merge back with original data
  result <- revenue_data %>%
    dplyr::left_join(valid_data %>% dplyr::select(id_property, value_quantile), 
                     by = "id_property")
  
  return(result)
}

# Function to analyze tax progressivity
analyze_tax_progressivity <- function(revenue_data) {
  # Add quantiles
  data_with_quantiles <- create_value_quantiles(revenue_data)
  
  # Calculate metrics by quantile
  progressivity_metrics <- data_with_quantiles %>%
    dplyr::filter(!is.na(value_quantile)) %>%
    dplyr::group_by(value_quantile) %>%
    dplyr::summarise(
      n_properties = n(),
      total_value = sum(property_value, na.rm = TRUE),
      total_tax = sum(total_tax, na.rm = TRUE),
      effective_rate = (sum(total_tax, na.rm = TRUE) / sum(property_value, na.rm = TRUE)) * 100,
      .groups = 'drop'
    )
  
  # Calculate shares
  total_value <- sum(progressivity_metrics$total_value)
  total_tax <- sum(progressivity_metrics$total_tax)
  
  progressivity_metrics$value_share <- (progressivity_metrics$total_value / total_value) * 100
  progressivity_metrics$tax_share <- (progressivity_metrics$total_tax / total_tax) * 100
  
  # Calculate progressivity index (tax share / value share)
  progressivity_metrics$progressivity_index <- progressivity_metrics$tax_share / progressivity_metrics$value_share
  
  return(progressivity_metrics)
}

# Function to compare scenarios
compare_scenarios <- function(revenue_data_list) {
  comparison_list <- list()
  
  for (scenario_name in names(revenue_data_list)) {
    scenario_data <- revenue_data_list[[scenario_name]]
    
    comparison_list[[scenario_name]] <- data.frame(
      scenario = scenario_name,
      total_revenue = sum(scenario_data$total_tax, na.rm = TRUE),
      property_tax_revenue = sum(scenario_data$property_tax, na.rm = TRUE),
      business_license_revenue = sum(scenario_data$business_license, na.rm = TRUE),
      n_properties = nrow(scenario_data),
      n_businesses = sum(!is.na(scenario_data$business_category))
    )
  }
  
  comparison_df <- do.call(rbind, comparison_list)
  
  # Calculate percentage changes from baseline (first scenario)
  if (nrow(comparison_df) > 1) {
    baseline_revenue <- comparison_df$total_revenue[1]
    comparison_df$revenue_change_pct <- ((comparison_df$total_revenue - baseline_revenue) / baseline_revenue) * 100
    comparison_df$revenue_change_absolute <- comparison_df$total_revenue - baseline_revenue
  }
  
  return(comparison_df)
}

# Function to export revenue summary report
export_revenue_summary <- function(revenue_data_list, file_path) {
  # Check if openxlsx is available
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for Excel export. Please install it using: install.packages('openxlsx')")
  }
  
  # Create workbook
  wb <- openxlsx::createWorkbook()
  
  # Add summary comparison sheet
  openxlsx::addWorksheet(wb, "Summary Comparison")
  comparison_data <- compare_scenarios(revenue_data_list)
  openxlsx::writeData(wb, "Summary Comparison", comparison_data)
  
  # Add individual scenario sheets
  for (scenario_name in names(revenue_data_list)) {
    openxlsx::addWorksheet(wb, scenario_name)
    openxlsx::writeData(wb, scenario_name, revenue_data_list[[scenario_name]])
  }
  
  # Save workbook
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
}