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
      business_categories = paste(
        unique(business_category[!is.na(business_category)]),
        collapse = "+"
      ),
      # ADD THIS LINE:
      business_subcategories = paste(
        unique(business_sub_category[!is.na(business_sub_category)]),
        collapse = "+"
      ),

      # Area information
      total_property_area = sum(property_area, na.rm = TRUE),
      total_business_area = sum(business_area, na.rm = TRUE),

      # Payment status
      made_payment = any(made_payment == TRUE, na.rm = TRUE),

      .groups = 'drop'
    ) %>%
    dplyr::filter(total_property_value > 0) # Remove zero-value properties
}

# Function to create property value quantiles
create_value_quantiles <- function(aggregated_data, n_quantiles = 5) {
  aggregated_data %>%
    dplyr::mutate(
      value_quantile = cut(
        total_property_value,
        breaks = quantile(
          total_property_value,
          probs = seq(0, 1, 1 / n_quantiles),
          na.rm = TRUE
        ),
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
  required_cols <- c(
    "value_quantile",
    "total_property_value",
    "total_tax",
    "total_property_tax",
    "total_business_license",
    "has_business"
  )
  missing_cols <- setdiff(required_cols, names(data_with_quantiles))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Calculate metrics by quantile with explicit average calculations
  progressivity_results <- data_with_quantiles %>%
    dplyr::group_by(value_quantile) %>%
    dplyr::summarise(
      # Count
      n_properties = n(),

      # VALUE METRICS
      total_value = sum(total_property_value, na.rm = TRUE),
      avg_property_value = sum(total_property_value, na.rm = TRUE) / n(), # Explicit calculation
      median_property_value = median(total_property_value, na.rm = TRUE),

      # TAX METRICS - EXPLICIT CALCULATION TO FIX ISSUE
      total_tax = sum(total_tax, na.rm = TRUE),
      avg_tax = sum(total_tax, na.rm = TRUE) / n(), # EXPLICIT: total divided by count
      median_tax = median(total_tax, na.rm = TRUE),

      # Property tax only
      total_property_tax_only = sum(total_property_tax, na.rm = TRUE),

      # Business license only
      total_business_license_only = sum(total_business_license, na.rm = TRUE),

      # Effective rates
      avg_effective_rate = (sum(total_tax, na.rm = TRUE) /
        sum(total_property_value, na.rm = TRUE)) *
        100,

      # Business properties
      n_with_business = sum(has_business, na.rm = TRUE),
      pct_with_business = (sum(has_business, na.rm = TRUE) / n()) * 100,

      .groups = 'drop'
    )

  # Verify calculations worked correctly
  cat("=== PROGRESSIVITY CALCULATION VERIFICATION ===\n")
  cat("Checking if avg_tax ≠ total_tax:\n")
  for (i in 1:nrow(progressivity_results)) {
    cat(sprintf(
      "  %s: total_tax = %.2f, avg_tax = %.2f, n = %d\n",
      progressivity_results$value_quantile[i],
      progressivity_results$total_tax[i],
      progressivity_results$avg_tax[i],
      progressivity_results$n_properties[i]
    ))
  }

  # Add share calculations and progressivity index
  progressivity_results <- progressivity_results %>%
    dplyr::mutate(
      # Calculate shares
      value_share = (total_value / sum(total_value)) * 100,
      tax_share = (total_tax / sum(total_tax)) * 100,

      # Progressivity index
      progressivity_index = ifelse(
        value_share > 0,
        tax_share / value_share,
        NA
      ),

      # Cumulative shares for Lorenz curve
      cumulative_value_share = cumsum(value_share),
      cumulative_tax_share = cumsum(tax_share)
    )

  return(progressivity_results)
}

# Function to identify overtaxed properties
identify_overtaxed <- function(data_with_quantiles, threshold_pct = 5) {
  # Check for has_business column
  if (!"has_business" %in% names(data_with_quantiles)) {
    data_with_quantiles$has_business <- FALSE
  }

  # Check for business_categories column
  if (!"business_categories" %in% names(data_with_quantiles)) {
    data_with_quantiles$business_categories <- NA
  }

  data_with_quantiles %>%
    dplyr::mutate(
      effective_rate = (total_tax / total_property_value) * 100,
      is_overtaxed = effective_rate > threshold_pct,
      is_low_value = value_quantile %in% c("Q1", "Q2"),
      risk_category = dplyr::case_when(
        is_overtaxed &
          is_low_value &
          has_business ~ "High Risk - Low Value with Business",
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
      business_subcategories # ADD THIS LINE
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
  area_under_lorenz <- sum(
    diff(sorted_data$cum_prop_pop) *
      (sorted_data$cum_prop_tax[-1] +
        sorted_data$cum_prop_tax[-nrow(sorted_data)]) /
      2
  )

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
    top_1pct = sum(sorted_data$total_tax[1:ceiling(n_total * 0.01)]) /
      total_tax *
      100,
    top_5pct = sum(sorted_data$total_tax[1:ceiling(n_total * 0.05)]) /
      total_tax *
      100,
    top_10pct = sum(sorted_data$total_tax[1:ceiling(n_total * 0.10)]) /
      total_tax *
      100,
    top_20pct = sum(sorted_data$total_tax[1:ceiling(n_total * 0.20)]) /
      total_tax *
      100,
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
    dplyr::select(
      id_property,
      existing_value = total_property_value,
      existing_tax = total_tax
    ) %>%
    dplyr::inner_join(
      scenario_agg %>%
        dplyr::select(
          id_property,
          scenario_value = total_property_value,
          scenario_tax = total_tax
        ),
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

  if (nrow(multi_type) == 0) {
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
      property_group = ifelse(
        has_business,
        "With Business",
        "Without Business"
      ),
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
      sum(data_with_quantiles$total_property_value)) *
      100,
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
  if (
    nrow(existing_prop) == 0 &&
      nrow(scenario_a_prop) == 0 &&
      nrow(scenario_b_prop) == 0
  ) {
    return(NULL) # Property not found
  }

  # Get property characteristics from first available scenario
  prop_data <- if (nrow(existing_prop) > 0) {
    existing_prop
  } else if (nrow(scenario_a_prop) > 0) {
    scenario_a_prop
  } else {
    scenario_b_prop
  }

  # Helper to get numeric value or NA
  get_num <- function(prop_df, field) {
    if (nrow(prop_df) > 0 && field %in% names(prop_df)) {
      return(prop_df[[field]])
    }
    return(NA_real_)
  }

  # Extract numeric values
  ex_pv <- get_num(existing_prop, "total_property_value")
  ex_pt <- get_num(existing_prop, "total_property_tax")
  ex_bl <- get_num(existing_prop, "total_business_license")
  ex_tt <- get_num(existing_prop, "total_tax")

  sa_pv <- get_num(scenario_a_prop, "total_property_value")
  sa_pt <- get_num(scenario_a_prop, "total_property_tax")
  sa_bl <- get_num(scenario_a_prop, "total_business_license")
  sa_tt <- get_num(scenario_a_prop, "total_tax")

  sb_pv <- get_num(scenario_b_prop, "total_property_value")
  sb_pt <- get_num(scenario_b_prop, "total_property_tax")
  sb_bl <- get_num(scenario_b_prop, "total_business_license")
  sb_tt <- get_num(scenario_b_prop, "total_tax")

  # Build comparison table (NUMERIC ONLY)
  comparison <- data.frame(
    Metric = c(
      "Total Property Value",
      "Total Property Tax",
      "Total Business License",
      "Total Tax"
    ),
    Existing = c(ex_pv, ex_pt, ex_bl, ex_tt),
    Scenario_A = c(sa_pv, sa_pt, sa_bl, sa_tt),
    Scenario_B = c(sb_pv, sb_pt, sb_bl, sb_tt),
    stringsAsFactors = FALSE
  )

  # Calculate changes using vectorized operations
  comparison$Change_A_vs_Existing <- ifelse(
    is.na(comparison$Scenario_A) | is.na(comparison$Existing),
    NA_real_,
    comparison$Scenario_A - comparison$Existing
  )

  comparison$Change_B_vs_Existing <- ifelse(
    is.na(comparison$Scenario_B) | is.na(comparison$Existing),
    NA_real_,
    comparison$Scenario_B - comparison$Existing
  )

  comparison$Pct_Change_A <- ifelse(
    is.na(comparison$Change_A_vs_Existing) |
      is.na(comparison$Existing) |
      comparison$Existing == 0,
    NA_real_,
    (comparison$Change_A_vs_Existing / comparison$Existing) * 100
  )

  comparison$Pct_Change_B <- ifelse(
    is.na(comparison$Change_B_vs_Existing) |
      is.na(comparison$Existing) |
      comparison$Existing == 0,
    NA_real_,
    (comparison$Change_B_vs_Existing / comparison$Existing) * 100
  )

  # Get property characteristics (same across scenarios)
  get_safe <- function(df, field) {
    if (nrow(df) > 0 && field %in% names(df)) {
      val <- df[[field]]
      if (is.na(val) || val == "") {
        return("None")
      }
      return(as.character(val))
    }
    return("None")
  }

  get_bool <- function(df, field) {
    if (nrow(df) > 0 && field %in% names(df)) {
      val <- df[[field]]
      if (is.logical(val) && !is.na(val)) {
        return(ifelse(val, "Yes", "No"))
      }
    }
    return("Unknown")
  }

  characteristics <- list(
    property_types = get_safe(prop_data, "property_types"),
    has_business = get_bool(prop_data, "has_business"),
    business_categories = get_safe(prop_data, "business_categories"),
    business_subcategories = get_safe(prop_data, "business_subcategories")
  )

  # Return both the comparison table and characteristics
  return(list(
    comparison = comparison,
    characteristics = characteristics
  ))
}

#' Find representative properties by quantile with filtering
#'
#' This function finds the median property (by value) from each quantile
#' and compares how taxes change across scenarios
#'
#' @param revenue_data List containing existing, scenario_a, and scenario_b data
#' @param n_quantiles Number of quantiles to use
#' @param filter_structure_types Vector of structure types to include (or "All")
#' @param filter_property_types Vector of property types to include (or "All")
#' @param filter_license_categories Vector of license categories to include (or "All")
#' @param filter_license_subcategories Vector of license subcategories to include (or "All")
#' @return Data frame with representative properties and their tax impacts
find_quantile_representatives <- function(
  revenue_data,
  n_quantiles = 5,
  filter_structure_types = "All",
  filter_property_types = "All",
  filter_license_categories = "All",
  filter_license_subcategories = "All"
) {
  # Aggregate all three scenarios
  existing_agg <- aggregate_properties(revenue_data$existing)
  scenario_a_agg <- aggregate_properties(revenue_data$scenario_a)
  scenario_b_agg <- aggregate_properties(revenue_data$scenario_b)

  # Apply filters to existing scenario
  filtered_data <- existing_agg

  # Structure type filter
  if (
    !is.null(filter_structure_types) &&
      length(filter_structure_types) > 0 &&
      !"All" %in% filter_structure_types
  ) {
    filtered_data <- filtered_data %>%
      dplyr::filter(structure_type %in% filter_structure_types)
  }

  # Property type filter
  if (
    !is.null(filter_property_types) &&
      length(filter_property_types) > 0 &&
      !"All" %in% filter_property_types
  ) {
    filtered_data <- filtered_data %>%
      dplyr::filter(property_type %in% filter_property_types)
  }

  # License category filter
  if (
    !is.null(filter_license_categories) &&
      length(filter_license_categories) > 0 &&
      !"All" %in% filter_license_categories
  ) {
    filtered_data <- filtered_data %>%
      dplyr::filter(
        business_category %in%
          filter_license_categories |
          is.na(business_category)
      )
  }

  # License subcategory filter
  if (
    !is.null(filter_license_subcategories) &&
      length(filter_license_subcategories) > 0 &&
      !"All" %in% filter_license_subcategories
  ) {
    filtered_data <- filtered_data %>%
      dplyr::filter(
        business_sub_category %in%
          filter_license_subcategories |
          is.na(business_sub_category)
      )
  }

  # Check if we have data after filtering
  if (nrow(filtered_data) == 0) {
    return(data.frame(
      message = "No properties match the selected filters"
    ))
  }

  # Add quantiles to filtered data
  valid_data <- filtered_data %>%
    dplyr::filter(!is.na(total_property_value) & total_property_value > 0)

  if (nrow(valid_data) == 0) {
    return(data.frame(
      message = "No valid properties with positive values after filtering"
    ))
  }

  # Create quantile labels
  quantile_labels <- paste0("Q", 1:n_quantiles)

  # Calculate quantile breaks
  breaks <- quantile(
    valid_data$total_property_value,
    probs = seq(0, 1, 1 / n_quantiles),
    na.rm = TRUE
  )

  # Assign quantiles
  valid_data$value_quantile <- cut(
    valid_data$total_property_value,
    breaks = breaks,
    labels = quantile_labels,
    include.lowest = TRUE
  )

  # Find median property in each quantile
  representative_props <- valid_data %>%
    dplyr::group_by(value_quantile) %>%
    dplyr::arrange(abs(
      total_property_value - median(total_property_value, na.rm = TRUE)
    )) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      id_property,
      value_quantile,
      property_type,
      structure_type,
      business_category,
      business_sub_category,
      existing_value = total_property_value,
      existing_property_tax = total_property_tax,
      existing_business_license = total_business_license,
      existing_total_tax = total_tax
    )

  # Join with Scenario A data
  representative_props <- representative_props %>%
    dplyr::left_join(
      scenario_a_agg %>%
        dplyr::select(
          id_property,
          scenario_a_value = total_property_value,
          scenario_a_property_tax = total_property_tax,
          scenario_a_business_license = total_business_license,
          scenario_a_total_tax = total_tax
        ),
      by = "id_property"
    )

  # Join with Scenario B data
  representative_props <- representative_props %>%
    dplyr::left_join(
      scenario_b_agg %>%
        dplyr::select(
          id_property,
          scenario_b_value = total_property_value,
          scenario_b_property_tax = total_property_tax,
          scenario_b_business_license = total_business_license,
          scenario_b_total_tax = total_tax
        ),
      by = "id_property"
    )

  # Calculate changes
  representative_props <- representative_props %>%
    dplyr::mutate(
      # Scenario A changes
      scenario_a_tax_change = scenario_a_total_tax - existing_total_tax,
      scenario_a_tax_change_pct = (scenario_a_tax_change / existing_total_tax) *
        100,
      scenario_a_value_change = scenario_a_value - existing_value,
      scenario_a_value_change_pct = (scenario_a_value_change / existing_value) *
        100,

      # Scenario B changes
      scenario_b_tax_change = scenario_b_total_tax - existing_total_tax,
      scenario_b_tax_change_pct = (scenario_b_tax_change / existing_total_tax) *
        100,
      scenario_b_value_change = scenario_b_value - existing_value,
      scenario_b_value_change_pct = (scenario_b_value_change / existing_value) *
        100,

      # Effective rates
      existing_effective_rate = (existing_total_tax / existing_value) * 100,
      scenario_a_effective_rate = (scenario_a_total_tax / scenario_a_value) *
        100,
      scenario_b_effective_rate = (scenario_b_total_tax / scenario_b_value) *
        100
    )

  return(representative_props)
}


#' Create a summary table for quantile representatives
#'
#' @param representative_props Output from find_quantile_representatives
#' @return Formatted data frame for display
format_quantile_representatives_table <- function(representative_props) {
  if ("message" %in% names(representative_props)) {
    return(representative_props)
  }

  representative_props %>%
    dplyr::select(
      Quantile = value_quantile,
      `Property ID` = id_property,
      `Property Type` = property_type,
      `Structure Type` = structure_type,
      `Business Category` = business_category,
      `Existing Value` = existing_value,
      `Existing Tax` = existing_total_tax,
      `Existing Rate (%)` = existing_effective_rate,
      `Scenario A Tax` = scenario_a_total_tax,
      `Scenario A Change` = scenario_a_tax_change,
      `Scenario A Change (%)` = scenario_a_tax_change_pct,
      `Scenario B Tax` = scenario_b_total_tax,
      `Scenario B Change` = scenario_b_tax_change,
      `Scenario B Change (%)` = scenario_b_tax_change_pct
    ) %>%
    dplyr::mutate(
      `Business Category` = ifelse(
        is.na(`Business Category`),
        "None",
        `Business Category`
      )
    )
}


#' Create visualization of tax impacts by quantile
#'
#' @param representative_props Output from find_quantile_representatives
#' @return ggplot object
plot_quantile_tax_impacts <- function(representative_props) {
  if ("message" %in% names(representative_props)) {
    return(NULL)
  }

  # Reshape data for plotting
  plot_data <- representative_props %>%
    dplyr::select(
      value_quantile,
      Existing = existing_total_tax,
      `Scenario A` = scenario_a_total_tax,
      `Scenario B` = scenario_b_total_tax
    ) %>%
    tidyr::pivot_longer(
      cols = c(Existing, `Scenario A`, `Scenario B`),
      names_to = "Scenario",
      values_to = "Tax"
    )

  # Order scenarios
  plot_data$Scenario <- factor(
    plot_data$Scenario,
    levels = c("Existing", "Scenario A", "Scenario B")
  )

  ggplot(plot_data, aes(x = value_quantile, y = Tax, fill = Scenario)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(
      values = c(
        "Existing" = "#95a5a6",
        "Scenario A" = "#3498db",
        "Scenario B" = "#e74c3c"
      )
    ) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Representative Property Tax by Quantile",
      subtitle = "Comparing median property from each value quantile",
      x = "Property Value Quantile",
      y = "Total Tax",
      fill = "Scenario"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
}


#' Create visualization of tax changes by quantile
#'
#' @param representative_props Output from find_quantile_representatives
#' @return ggplot object
plot_quantile_tax_changes <- function(representative_props) {
  if ("message" %in% names(representative_props)) {
    return(NULL)
  }

  # Reshape data for plotting
  plot_data <- representative_props %>%
    dplyr::select(
      value_quantile,
      `Scenario A` = scenario_a_tax_change,
      `Scenario B` = scenario_b_tax_change
    ) %>%
    tidyr::pivot_longer(
      cols = c(`Scenario A`, `Scenario B`),
      names_to = "Scenario",
      values_to = "Tax_Change"
    )

  ggplot(plot_data, aes(x = value_quantile, y = Tax_Change, fill = Scenario)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
    scale_fill_manual(
      values = c("Scenario A" = "#3498db", "Scenario B" = "#e74c3c")
    ) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Tax Change from Existing Scenario",
      subtitle = "Impact on representative properties by quantile",
      x = "Property Value Quantile",
      y = "Tax Change (Positive = Increase, Negative = Decrease)",
      fill = "Scenario"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
}
