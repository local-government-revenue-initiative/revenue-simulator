# R/module5_functions.R

# Function to aggregate properties by id_property (handling multi-type properties)
aggregate_properties <- function(data) {
  data %>%
    dplyr::group_by(id_property) %>%
    dplyr::summarise(
      # Sum values across all property types for same id
      total_property_value = sum(property_value, na.rm = TRUE),
      total_property_tax = sum(property_tax, na.rm = TRUE),
      total_business_license = sum(business_license, na.rm = TRUE),
      total_tax = sum(total_tax, na.rm = TRUE),
      
      # Property characteristics
      has_business = any(!is.na(business_category)),
      n_property_types = n_distinct(property_type),
      property_types = paste(unique(property_type), collapse = "+"),
      business_categories = paste(unique(business_category[!is.na(business_category)]), collapse = "+"),
      # ADD THIS LINE:
      business_subcategories = paste(unique(business_sub_category[!is.na(business_sub_category)]), collapse = "+"),
      
      # Area information
      total_property_area = sum(property_area, na.rm = TRUE),
      total_business_area = sum(business_area, na.rm = TRUE),
      
      # Payment status
      made_payment = any(made_payment == TRUE, na.rm = TRUE),
      
      .groups = 'drop'
    ) %>%
    dplyr::filter(total_property_value > 0)  # Remove zero-value properties
}

# Function to create property value quantiles
create_value_quantiles <- function(aggregated_data, n_quantiles = 5) {
  aggregated_data %>%
    dplyr::mutate(
      value_quantile = cut(
        total_property_value,
        breaks = quantile(total_property_value, 
                          probs = seq(0, 1, 1/n_quantiles),
                          na.rm = TRUE),
        labels = paste0("Q", 1:n_quantiles),
        include.lowest = TRUE
      ),
      # Add numeric quantile for easier plotting
      quantile_num = as.numeric(value_quantile)
    )
}

# Function to calculate progressivity metrics by quantile - FIXED VERSION
calculate_progressivity <- function(data_with_quantiles) {
  
  # First, ensure we have the required columns
  required_cols <- c("value_quantile", "total_property_value", "total_tax", 
                     "total_property_tax", "total_business_license", "has_business")
  missing_cols <- setdiff(required_cols, names(data_with_quantiles))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Calculate metrics by quantile with explicit average calculations
  progressivity_results <- data_with_quantiles %>%
    dplyr::group_by(value_quantile) %>%
    dplyr::summarise(
      # Count
      n_properties = n(),
      
      # VALUE METRICS
      total_value = sum(total_property_value, na.rm = TRUE),
      avg_property_value = sum(total_property_value, na.rm = TRUE) / n(),  # Explicit calculation
      median_property_value = median(total_property_value, na.rm = TRUE),
      
      # TAX METRICS - EXPLICIT CALCULATION TO FIX ISSUE
      total_tax = sum(total_tax, na.rm = TRUE),
      avg_tax = sum(total_tax, na.rm = TRUE) / n(),  # EXPLICIT: total divided by count
      median_tax = median(total_tax, na.rm = TRUE),
      
      # Property tax only
      total_property_tax_only = sum(total_property_tax, na.rm = TRUE),
      
      # Business license only  
      total_business_license_only = sum(total_business_license, na.rm = TRUE),
      
      # Effective rates
      avg_effective_rate = (sum(total_tax, na.rm = TRUE) / 
                              sum(total_property_value, na.rm = TRUE)) * 100,
      
      # Business properties
      n_with_business = sum(has_business, na.rm = TRUE),
      pct_with_business = (sum(has_business, na.rm = TRUE) / n()) * 100,
      
      .groups = 'drop'
    )
  
  # Verify calculations worked correctly
  cat("=== PROGRESSIVITY CALCULATION VERIFICATION ===\n")
  cat("Checking if avg_tax ≠ total_tax:\n")
  for (i in 1:nrow(progressivity_results)) {
    cat(sprintf("  %s: total_tax = %.2f, avg_tax = %.2f, n = %d\n",
                progressivity_results$value_quantile[i],
                progressivity_results$total_tax[i],
                progressivity_results$avg_tax[i],
                progressivity_results$n_properties[i]))
  }
  
  # Add share calculations and progressivity index
  progressivity_results <- progressivity_results %>%
    dplyr::mutate(
      # Calculate shares
      value_share = (total_value / sum(total_value)) * 100,
      tax_share = (total_tax / sum(total_tax)) * 100,
      
      # Progressivity index
      progressivity_index = ifelse(value_share > 0, tax_share / value_share, NA),
      
      # Cumulative shares for Lorenz curve
      cumulative_value_share = cumsum(value_share),
      cumulative_tax_share = cumsum(tax_share)
    )
  
  return(progressivity_results)
}

# Function to identify overtaxed properties
identify_overtaxed <- function(data_with_quantiles, threshold_pct = 5) {
  # Check for has_business column
  if(!"has_business" %in% names(data_with_quantiles)) {
    data_with_quantiles$has_business <- FALSE
  }
  
  # Check for business_categories column  
  if(!"business_categories" %in% names(data_with_quantiles)) {
    data_with_quantiles$business_categories <- NA
  }
  
  data_with_quantiles %>%
    dplyr::mutate(
      effective_rate = (total_tax / total_property_value) * 100,
      is_overtaxed = effective_rate > threshold_pct,
      is_low_value = value_quantile %in% c("Q1", "Q2"),
      risk_category = dplyr::case_when(
        is_overtaxed & is_low_value & has_business ~ "High Risk - Low Value with Business",
        is_overtaxed & is_low_value ~ "High Risk - Low Value",
        is_overtaxed & has_business ~ "Medium Risk - Has Business",
        is_overtaxed ~ "Medium Risk",
        TRUE ~ "No Risk"
      )
    ) %>%
    dplyr::filter(is_overtaxed) %>%
    dplyr::arrange(desc(effective_rate)) %>%
    dplyr::select(
      id_property,
      value_quantile,
      property_types,
      total_property_value,
      total_property_tax,
      total_business_license,
      total_tax,
      effective_rate,
      risk_category,
      has_business,
      business_categories,
      business_subcategories  # ADD THIS LINE
    )
}

# Function to calculate Gini coefficient
calculate_gini <- function(aggregated_data) {
  # Sort by property value
  sorted_data <- aggregated_data %>%
    dplyr::arrange(total_property_value) %>%
    dplyr::mutate(
      # Calculate cumulative proportions
      cum_prop_pop = row_number() / n(),
      cum_prop_value = cumsum(total_property_value) / sum(total_property_value),
      cum_prop_tax = cumsum(total_tax) / sum(total_tax)
    )
  
  # Calculate area under Lorenz curve using trapezoidal rule
  area_under_lorenz <- sum(diff(sorted_data$cum_prop_pop) * 
                             (sorted_data$cum_prop_tax[-1] + sorted_data$cum_prop_tax[-nrow(sorted_data)]) / 2)
  
  # Gini coefficient
  gini <- 2 * (0.5 - area_under_lorenz)
  
  return(list(
    gini_coefficient = gini,
    lorenz_data = sorted_data %>%
      dplyr::select(cum_prop_pop, cum_prop_value, cum_prop_tax)
  ))
}

# Function to analyze tax concentration
calculate_tax_concentration <- function(aggregated_data) {
  sorted_data <- aggregated_data %>%
    dplyr::arrange(desc(total_tax))
  
  n_total <- nrow(sorted_data)
  total_tax <- sum(sorted_data$total_tax)
  
  list(
    top_1pct = sum(sorted_data$total_tax[1:ceiling(n_total * 0.01)]) / total_tax * 100,
    top_5pct = sum(sorted_data$total_tax[1:ceiling(n_total * 0.05)]) / total_tax * 100,
    top_10pct = sum(sorted_data$total_tax[1:ceiling(n_total * 0.10)]) / total_tax * 100,
    top_20pct = sum(sorted_data$total_tax[1:ceiling(n_total * 0.20)]) / total_tax * 100,
    n_total = n_total,
    total_tax_revenue = total_tax
  )
}

# Function to compare scenarios
compare_scenarios <- function(existing_data, scenario_data, scenario_name) {
  # Aggregate both datasets
  existing_agg <- aggregate_properties(existing_data) %>%
    dplyr::mutate(scenario = "Existing")
  
  scenario_agg <- aggregate_properties(scenario_data) %>%
    dplyr::mutate(scenario = scenario_name)
  
  # Join on id_property to compare
  comparison <- existing_agg %>%
    dplyr::select(id_property, 
                  existing_value = total_property_value,
                  existing_tax = total_tax) %>%
    dplyr::inner_join(
      scenario_agg %>%
        dplyr::select(id_property, 
                      scenario_value = total_property_value,
                      scenario_tax = total_tax),
      by = "id_property"
    ) %>%
    dplyr::mutate(
      tax_change = scenario_tax - existing_tax,
      tax_change_pct = (tax_change / existing_tax) * 100,
      winner_loser = dplyr::case_when(
        tax_change < -100 ~ "Big Winner (>$100 decrease)",
        tax_change < 0 ~ "Winner",
        tax_change == 0 ~ "No Change", 
        tax_change < 100 ~ "Loser",
        TRUE ~ "Big Loser (>$100 increase)"
      )
    )
  
  # Add quantiles
  comparison_with_quantiles <- comparison %>%
    dplyr::mutate(
      value_quantile = cut(
        existing_value,
        breaks = quantile(existing_value, probs = seq(0, 1, 0.2)),
        labels = paste0("Q", 1:5),
        include.lowest = TRUE
      )
    )
  
  return(comparison_with_quantiles)
}

# Function to analyze multi-type properties
analyze_multi_type_properties <- function(data) {
  multi_type <- data %>%
    dplyr::group_by(id_property) %>%
    dplyr::filter(n_distinct(property_type) > 1) %>%
    dplyr::ungroup()
  
  if(nrow(multi_type) == 0) {
    return(data.frame(
      message = "No multi-type properties found"
    ))
  }
  
  multi_type %>%
    dplyr::group_by(id_property) %>%
    dplyr::summarise(
      property_types = paste(unique(property_type), collapse = "+"),
      n_types = n_distinct(property_type),
      total_value = sum(property_value, na.rm = TRUE),
      total_property_tax = sum(property_tax, na.rm = TRUE),
      total_business_license = sum(business_license, na.rm = TRUE),
      total_tax = sum(total_tax, na.rm = TRUE),
      effective_rate = (total_tax / total_value) * 100,
      has_business = any(!is.na(business_category)),
      .groups = 'drop'
    ) %>%
    dplyr::arrange(desc(total_tax))
}

# Function to analyze business property burden
analyze_business_burden <- function(data_with_quantiles) {
  data_with_quantiles %>%
    dplyr::mutate(
      property_group = ifelse(has_business, "With Business", "Without Business"),
      effective_rate = (total_tax / total_property_value) * 100
    ) %>%
    dplyr::group_by(value_quantile, property_group) %>%
    dplyr::summarise(
      n_properties = n(),
      avg_effective_rate = mean(effective_rate, na.rm = TRUE),
      median_effective_rate = median(effective_rate, na.rm = TRUE),
      total_tax = sum(total_tax, na.rm = TRUE),
      avg_property_tax = mean(total_property_tax, na.rm = TRUE),
      avg_business_license = mean(total_business_license, na.rm = TRUE),
      .groups = 'drop'
    )
}

# Function to create summary statistics
create_summary_stats <- function(data_with_quantiles) {
  list(
    total_properties = nrow(data_with_quantiles),
    total_value = sum(data_with_quantiles$total_property_value),
    total_tax_revenue = sum(data_with_quantiles$total_tax),
    overall_effective_rate = (sum(data_with_quantiles$total_tax) / 
                                sum(data_with_quantiles$total_property_value)) * 100,
    properties_with_business = sum(data_with_quantiles$has_business),
    pct_with_business = mean(data_with_quantiles$has_business) * 100,
    median_property_value = median(data_with_quantiles$total_property_value),
    median_tax = median(data_with_quantiles$total_tax),
    properties_paying = sum(data_with_quantiles$made_payment),
    compliance_rate = mean(data_with_quantiles$made_payment) * 100
  )
}

# Function to identify winners and losers by quantile
analyze_winners_losers <- function(comparison_data) {
  comparison_data %>%
    dplyr::group_by(value_quantile, winner_loser) %>%
    dplyr::summarise(
      n_properties = n(),
      total_tax_change = sum(tax_change, na.rm = TRUE),
      avg_tax_change = mean(tax_change, na.rm = TRUE),
      avg_tax_change_pct = mean(tax_change_pct, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    tidyr::pivot_wider(
      names_from = winner_loser,
      values_from = c(n_properties, total_tax_change, avg_tax_change),
      values_fill = 0
    )
}

# Function to compare a specific property across scenarios
compare_property_across_scenarios <- function(revenue_data, property_id) {
  
  # Get aggregated data from all scenarios
  existing_agg <- aggregate_properties(revenue_data$existing)
  scenario_a_agg <- aggregate_properties(revenue_data$scenario_a)
  scenario_b_agg <- aggregate_properties(revenue_data$scenario_b)
  
  # Filter to the specific property
  existing_prop <- existing_agg %>%
    dplyr::filter(id_property == property_id)
  
  scenario_a_prop <- scenario_a_agg %>%
    dplyr::filter(id_property == property_id)
  
  scenario_b_prop <- scenario_b_agg %>%
    dplyr::filter(id_property == property_id)
  
  # Check if property exists in any scenario
  if (nrow(existing_prop) == 0 && nrow(scenario_a_prop) == 0 && nrow(scenario_b_prop) == 0) {
    return(NULL)  # Property not found
  }
  
  # Create comparison dataframe
  comparison <- data.frame(
    Metric = c("Total Property Value", 
               "Total Property Tax", 
               "Total Business License", 
               "Total Tax",
               "Property Types",
               "Has Business",
               "Business Categories"),
    
    Existing = c(
      if(nrow(existing_prop) > 0) existing_prop$total_property_value else NA,
      if(nrow(existing_prop) > 0) existing_prop$total_property_tax else NA,
      if(nrow(existing_prop) > 0) existing_prop$total_business_license else NA,
      if(nrow(existing_prop) > 0) existing_prop$total_tax else NA,
      if(nrow(existing_prop) > 0) existing_prop$property_types else "N/A",
      if(nrow(existing_prop) > 0) as.character(existing_prop$has_business) else "N/A",
      if(nrow(existing_prop) > 0) existing_prop$business_categories else "N/A"
    ),
    
    Scenario_A = c(
      if(nrow(scenario_a_prop) > 0) scenario_a_prop$total_property_value else NA,
      if(nrow(scenario_a_prop) > 0) scenario_a_prop$total_property_tax else NA,
      if(nrow(scenario_a_prop) > 0) scenario_a_prop$total_business_license else NA,
      if(nrow(scenario_a_prop) > 0) scenario_a_prop$total_tax else NA,
      if(nrow(scenario_a_prop) > 0) scenario_a_prop$property_types else "N/A",
      if(nrow(scenario_a_prop) > 0) as.character(scenario_a_prop$has_business) else "N/A",
      if(nrow(scenario_a_prop) > 0) scenario_a_prop$business_categories else "N/A"
    ),
    
    Scenario_B = c(
      if(nrow(scenario_b_prop) > 0) scenario_b_prop$total_property_value else NA,
      if(nrow(scenario_b_prop) > 0) scenario_b_prop$total_property_tax else NA,
      if(nrow(scenario_b_prop) > 0) scenario_b_prop$total_business_license else NA,
      if(nrow(scenario_b_prop) > 0) scenario_b_prop$total_tax else NA,
      if(nrow(scenario_b_prop) > 0) scenario_b_prop$property_types else "N/A",
      if(nrow(scenario_b_prop) > 0) as.character(scenario_b_prop$has_business) else "N/A",
      if(nrow(scenario_b_prop) > 0) scenario_b_prop$business_categories else "N/A"
    ),
    
    stringsAsFactors = FALSE
  )
  
  # Calculate changes vs Existing
  comparison <- comparison %>%
    dplyr::mutate(
      Change_A_vs_Existing = dplyr::case_when(
        Metric %in% c("Total Property Value", "Total Property Tax", 
                      "Total Business License", "Total Tax") ~ 
          as.numeric(Scenario_A) - as.numeric(Existing),
        TRUE ~ NA_real_
      ),
      Change_B_vs_Existing = dplyr::case_when(
        Metric %in% c("Total Property Value", "Total Property Tax", 
                      "Total Business License", "Total Tax") ~ 
          as.numeric(Scenario_B) - as.numeric(Existing),
        TRUE ~ NA_real_
      ),
      Pct_Change_A = dplyr::case_when(
        Metric %in% c("Total Property Value", "Total Property Tax", 
                      "Total Business License", "Total Tax") & 
          as.numeric(Existing) > 0 ~ 
          (Change_A_vs_Existing / as.numeric(Existing)) * 100,
        TRUE ~ NA_real_
      ),
      Pct_Change_B = dplyr::case_when(
        Metric %in% c("Total Property Value", "Total Property Tax", 
                      "Total Business License", "Total Tax") & 
          as.numeric(Existing) > 0 ~ 
          (Change_B_vs_Existing / as.numeric(Existing)) * 100,
        TRUE ~ NA_real_
      )
    )
  
  return(comparison)
}