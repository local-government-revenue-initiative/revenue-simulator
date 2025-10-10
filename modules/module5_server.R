# modules/module5_server.R

module5_server <- function(id, revenue_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store analysis results
    values <- reactiveValues(
      aggregated_data = NULL,
      data_with_quantiles = NULL,
      progressivity_results = NULL,
      overtaxed_properties = NULL,
      comparison_results = NULL,
      gini_results = NULL,
      concentration_results = NULL,
      analysis_complete = FALSE
    )
    
    # Run main analysis when button clicked
    observeEvent(input$run_analysis, {
      req(revenue_data())
      
      withProgress(message = 'Running tax burden analysis...', value = 0, {
        
        # Get the selected scenario data
        all_revenue_data <- revenue_data()
        scenario_data <- all_revenue_data[[input$analysis_scenario]]
        existing_data <- all_revenue_data[["existing"]]
        
        # Apply complier filter if selected
        if (input$filter_compliers) {
          scenario_data <- scenario_data %>%
            dplyr::filter(made_payment == TRUE)
          existing_data <- existing_data %>%
            dplyr::filter(made_payment == TRUE)
        }
        
        incProgress(0.2, detail = "Aggregating properties...")
        
        # Aggregate properties by id_property
        values$aggregated_data <- aggregate_properties(scenario_data)
        
        incProgress(0.2, detail = "Creating quantiles...")
        
        # Create quantiles
        values$data_with_quantiles <- create_value_quantiles(
          values$aggregated_data, 
          n_quantiles = input$n_quantiles
        )
        
        incProgress(0.2, detail = "Calculating progressivity...")
        
        # Calculate progressivity metrics
        values$progressivity_results <- calculate_progressivity(values$data_with_quantiles)
        
        # Calculate Gini coefficient
        values$gini_results <- calculate_gini(values$aggregated_data)
        
        # Calculate concentration metrics
        values$concentration_results <- calculate_tax_concentration(values$aggregated_data)
        
        incProgress(0.2, detail = "Identifying overtaxed properties...")
        
        # Identify overtaxed properties
        values$overtaxed_properties <- identify_overtaxed(
          values$data_with_quantiles,
          threshold_pct = input$overtax_threshold
        )
        
        incProgress(0.1, detail = "Comparing scenarios...")
        
        # Compare with existing scenario
        values$comparison_results <- compare_scenarios(
          existing_data,
          scenario_data,
          input$analysis_scenario
        )
        
        incProgress(0.1, detail = "Complete!")
        
        values$analysis_complete <- TRUE
      })
      
      showNotification("Analysis complete!", type = "message", duration = 5)
    })
    
    # Value boxes
    output$total_properties_box <- renderValueBox({
      valueBox(
        value = ifelse(values$analysis_complete,
                       format(nrow(values$aggregated_data), big.mark = ","),
                       "Not calculated"),
        subtitle = "Total Properties",
        icon = icon("home"),
        color = if(values$analysis_complete) "blue" else "black"
      )
    })
    
    output$effective_rate_box <- renderValueBox({
      if (!values$analysis_complete) {
        valueBox(value = "Not calculated", subtitle = "Avg Effective Rate", 
                 icon = icon("percentage"), color = "black")
      } else {
        rate <- (sum(values$aggregated_data$total_tax) / 
                   sum(values$aggregated_data$total_property_value)) * 100
        valueBox(
          value = paste0(round(rate, 2), "%"),
          subtitle = "Overall Effective Tax Rate",
          icon = icon("percentage"),
          color = if (rate > 3) "red" else if (rate > 2) "yellow" else "green"
        )
      }
    })
    
    output$gini_coefficient_box <- renderValueBox({
      if (!values$analysis_complete) {
        valueBox(value = "Not calculated", subtitle = "Gini Coefficient", 
                 icon = icon("chart-line"), color = "black")
      } else {
        gini <- values$gini_results$gini_coefficient
        valueBox(
          value = round(gini, 3),
          subtitle = "Gini Coefficient (0=equal, 1=unequal)",
          icon = icon("chart-line"),
          color = if (gini > 0.6) "red" else if (gini > 0.4) "yellow" else "green"
        )
      }
    })
    
    output$overtaxed_count_box <- renderValueBox({
      if (!values$analysis_complete) {
        valueBox(value = "Not calculated", subtitle = "Overtaxed Properties", 
                 icon = icon("exclamation-triangle"), color = "black")
      } else {
        count <- nrow(values$overtaxed_properties)
        pct <- (count / nrow(values$data_with_quantiles)) * 100
        valueBox(
          value = paste0(count, " (", round(pct, 1), "%)"),
          subtitle = paste("Properties Above", input$overtax_threshold, "% Rate"),
          icon = icon("exclamation-triangle"),
          color = if (pct > 10) "red" else if (pct > 5) "yellow" else "green"
        )
      }
    })
    
    # Tab 1: Progressivity Analysis
    output$quantile_burden_table <- DT::renderDataTable({
      req(values$progressivity_results)
      
      values$progressivity_results %>%
        dplyr::select(
          Quantile = value_quantile,
          `Properties` = n_properties,
          `Total Value` = total_value,
          `Total Tax` = total_tax,
          `Avg Value` = avg_property_value,
          `Avg Tax` = avg_tax,
          `Eff. Rate %` = avg_effective_rate,
          `Value Share %` = value_share,
          `Tax Share %` = tax_share,
          `Progressivity Index` = progressivity_index
        ) %>%
        DT::datatable(
          options = list(
            pageLength = 10,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel')
          ),
          extensions = 'Buttons'
        ) %>%
        DT::formatCurrency(columns = c('Total Value', 'Total Tax', 'Avg Value', 'Avg Tax'),
                           currency = "", interval = 3, mark = ",") %>%
        DT::formatRound(columns = c('Eff. Rate %', 'Value Share %', 'Tax Share %', 
                                    'Progressivity Index'), digits = 2) %>%
        DT::formatStyle(
          'Progressivity Index',
          backgroundColor = DT::styleInterval(c(0.9, 1.1), 
                                              c('lightcoral', 'lightgray', 'lightgreen'))
        )
    })
    
    # output$lorenz_curve_plot <- renderPlot({
    #   req(values$gini_results)
      
    #   lorenz_data <- values$gini_results$lorenz_data
      
    #   # Add perfect equality line points
    #   equality_data <- data.frame(
    #     cum_prop_pop = c(0, 1),
    #     cum_prop_tax = c(0, 1)
    #   )
      
    #   ggplot() +
    #     # Perfect equality line
    #     geom_line(data = equality_data,
    #               aes(x = cum_prop_pop, y = cum_prop_tax),
    #               color = "gray50", linetype = "dashed", linewidth = 1) +
    #     # Lorenz curve
    #     geom_line(data = lorenz_data,
    #               aes(x = cum_prop_pop, y = cum_prop_tax),
    #               color = "darkblue", linewidth = 1.5) +
    #     geom_point(data = lorenz_data %>%
    #                  dplyr::filter(row_number() %% (n()/10) == 0),
    #                aes(x = cum_prop_pop, y = cum_prop_tax),
    #                color = "darkblue", size = 3) +
    #     # Fill area between curves
    #     geom_ribbon(data = lorenz_data,
    #                 aes(x = cum_prop_pop, ymin = cum_prop_pop, ymax = cum_prop_tax),
    #                 alpha = 0.2, fill = "blue") +
    #     scale_x_continuous(labels = scales::percent) +
    #     scale_y_continuous(labels = scales::percent) +
    #     labs(title = paste0("Lorenz Curve (Gini = ", 
    #                         round(values$gini_results$gini_coefficient, 3), ")"),
    #          x = "Cumulative % of Properties (by value)",
    #          y = "Cumulative % of Tax Revenue",
    #          caption = "Deviation from diagonal indicates inequality") +
    #     theme_minimal() +
    #     theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    # })
    
    output$progressivity_index_plot <- renderPlot({
      req(values$progressivity_results)
      
      ggplot(values$progressivity_results, 
             aes(x = value_quantile, y = progressivity_index, fill = progressivity_index)) +
        geom_bar(stat = "identity") +
        geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1) +
        scale_fill_gradient2(low = "red", mid = "gray90", high = "darkgreen", 
                             midpoint = 1, limits = c(0, 2)) +
        scale_y_continuous(breaks = seq(0, 2, 0.25)) +
        labs(title = "Progressivity Index by Quantile",
             subtitle = "Values > 1 indicate progressive taxation",
             x = "Property Value Quantile",
             y = "Progressivity Index",
             fill = "Index") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              legend.position = "none")
    })
    
    output$tax_value_share_plot <- renderPlot({
      req(values$progressivity_results)
      
      share_data <- values$progressivity_results %>%
        dplyr::select(value_quantile, value_share, tax_share) %>%
        tidyr::pivot_longer(cols = c(value_share, tax_share),
                            names_to = "share_type",
                            values_to = "percentage")
      
      ggplot(share_data, aes(x = value_quantile, y = percentage, 
                             fill = share_type, group = share_type)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        scale_fill_manual(values = c("value_share" = "#3498db", "tax_share" = "#e74c3c"),
                          labels = c("Value Share", "Tax Share")) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +
        labs(title = "Distribution of Property Value vs Tax Burden",
             x = "Property Value Quantile",
             y = "Share (%)",
             fill = "") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              legend.position = "bottom")
    })
    
    output$progressivity_metrics <- renderPrint({
      req(values$analysis_complete)
      
      cat("=== PROGRESSIVITY ANALYSIS SUMMARY ===\n\n")
      
      # Overall metrics
      stats <- create_summary_stats(values$data_with_quantiles)
      cat("Total Properties:", format(stats$total_properties, big.mark = ","), "\n")
      cat("Total Property Value:", format(round(stats$total_value), big.mark = ","), "\n")
      cat("Total Tax Revenue:", format(round(stats$total_tax_revenue), big.mark = ","), "\n")
      cat("Overall Effective Rate:", round(stats$overall_effective_rate, 2), "%\n")
      cat("Gini Coefficient:", round(values$gini_results$gini_coefficient, 3), "\n\n")
      
      # Progressivity assessment
      prog_index_q1 <- values$progressivity_results$progressivity_index[1]
      prog_index_q5 <- values$progressivity_results$progressivity_index[input$n_quantiles]
      
      cat("Lowest Quantile Progressivity Index:", round(prog_index_q1, 2), "\n")
      cat("Highest Quantile Progressivity Index:", round(prog_index_q5, 2), "\n\n")
      
      if (prog_index_q1 < 0.8 && prog_index_q5 > 1.2) {
        cat("Assessment: Tax system appears PROGRESSIVE\n")
      } else if (prog_index_q1 > 1.1) {
        cat("Assessment: Tax system appears REGRESSIVE (low-value properties overtaxed)\n")
      } else {
        cat("Assessment: Tax system appears PROPORTIONAL\n")
      }
    })
    
    # Tab 2: Effective Tax Rates
    output$effective_rate_scatter <- renderPlot({
      req(values$data_with_quantiles)
      
      ggplot(values$data_with_quantiles, 
             aes(x = total_property_value, 
                 y = (total_tax / total_property_value) * 100,
                 color = value_quantile)) +
        geom_point(aes(shape = has_business), alpha = 0.6, size = 2) +
        geom_hline(yintercept = input$overtax_threshold, 
                   linetype = "dashed", color = "red", linewidth = 1) +
        geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.8) +
        scale_x_log10(labels = scales::comma) +
        scale_color_brewer(palette = "RdYlBu", direction = -1) +
        scale_shape_manual(values = c(16, 17),
                           labels = c("No Business", "Has Business")) +
        labs(title = "Effective Tax Rate vs Property Value",
             subtitle = paste("Red line indicates overtaxation threshold of", 
                              input$overtax_threshold, "%"),
             x = "Property Value (log scale)",
             y = "Effective Tax Rate (%)",
             color = "Quantile",
             shape = "Business") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    })
    
    output$effective_rate_boxplot <- renderPlot({
      req(values$data_with_quantiles)
      
      ggplot(values$data_with_quantiles,
             aes(x = value_quantile, 
                 y = (total_tax / total_property_value) * 100,
                 fill = value_quantile)) +
        geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
        geom_hline(yintercept = input$overtax_threshold,
                   linetype = "dashed", color = "red", linewidth = 1) +
        scale_fill_brewer(palette = "RdYlBu", direction = -1) +
        labs(title = "Distribution of Effective Tax Rates by Quantile",
             x = "Property Value Quantile",
             y = "Effective Tax Rate (%)",
             fill = "Quantile") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              legend.position = "none")
    })
    
    output$effective_rate_by_type <- renderPlot({
      req(values$data_with_quantiles)
      
      # Parse property types
      type_data <- values$data_with_quantiles %>%
        dplyr::mutate(
          main_type = dplyr::case_when(
            grepl("domestic", property_types, ignore.case = TRUE) ~ "Domestic",
            grepl("commercial", property_types, ignore.case = TRUE) ~ "Commercial",
            grepl("institutional", property_types, ignore.case = TRUE) ~ "Institutional",
            TRUE ~ "Other"
          ),
          effective_rate = (total_tax / total_property_value) * 100
        ) %>%
        dplyr::group_by(main_type) %>%
        dplyr::summarise(
          avg_rate = mean(effective_rate, na.rm = TRUE),
          median_rate = median(effective_rate, na.rm = TRUE),
          n = n(),
          .groups = 'drop'
        )
      
      ggplot(type_data, aes(x = reorder(main_type, avg_rate), y = avg_rate, fill = main_type)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        geom_text(aes(label = paste0(round(avg_rate, 2), "%\n(n=", n, ")")),
                  vjust = -0.5, size = 3.5) +
        scale_fill_manual(values = c("Domestic" = "#3498db",
                                     "Commercial" = "#e74c3c",
                                     "Institutional" = "#95a5a6",
                                     "Other" = "#f39c12")) +
        labs(title = "Average Effective Tax Rate by Property Type",
             x = "Property Type",
             y = "Average Effective Tax Rate (%)",
             fill = "Type") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              legend.position = "none")
    })
    
    # Tab 3: Overtaxation Analysis
    output$overtax_table <- DT::renderDataTable({
      req(values$overtaxed_properties)
      
      values$overtaxed_properties %>%
        DT::datatable(
          options = list(
            pageLength = 25,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel'),
            scrollX = TRUE
          ),
          extensions = 'Buttons'
        ) %>%
        DT::formatCurrency(columns = c('total_property_value', 'total_property_tax',
                                       'total_business_license', 'total_tax'),
                           currency = "", interval = 3, mark = ",") %>%
        DT::formatRound(columns = 'effective_rate', digits = 2) %>%
        DT::formatStyle(
          'risk_category',
          backgroundColor = DT::styleEqual(
            c("High Risk - Low Value with Business", "High Risk - Low Value", "Medium Risk"),
            c('#ffcccc', '#ffe6cc', '#fff9cc')
          )
        )
    })
    
    output$overtax_by_quantile <- renderPlot({
      req(values$data_with_quantiles)
      
      overtax_summary <- values$data_with_quantiles %>%
        dplyr::mutate(
          effective_rate = (total_tax / total_property_value) * 100,
          is_overtaxed = effective_rate > input$overtax_threshold
        ) %>%
        dplyr::group_by(value_quantile) %>%
        dplyr::summarise(
          n_total = n(),
          n_overtaxed = sum(is_overtaxed),
          pct_overtaxed = (n_overtaxed / n_total) * 100,
          .groups = 'drop'
        )
      
      ggplot(overtax_summary, aes(x = value_quantile, y = pct_overtaxed, fill = pct_overtaxed)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        geom_text(aes(label = paste0(round(pct_overtaxed, 1), "%\n(",
                                     n_overtaxed, "/", n_total, ")")),
                  vjust = -0.5, size = 3.5) +
        scale_fill_gradient(low = "lightgreen", high = "darkred") +
        labs(title = paste("Properties with Effective Tax Rate >", input$overtax_threshold, "%"),
             x = "Property Value Quantile",
             y = "Percentage Overtaxed",
             fill = "% Overtaxed") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              legend.position = "none")
    })
    
    output$combined_burden_plot <- renderPlot({
      req(values$data_with_quantiles)
      
      burden_data <- values$data_with_quantiles %>%
        dplyr::group_by(value_quantile, has_business) %>%
        dplyr::summarise(
          avg_property_tax = mean(total_property_tax, na.rm = TRUE),
          avg_business_license = mean(total_business_license, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        tidyr::pivot_longer(cols = c(avg_property_tax, avg_business_license),
                            names_to = "tax_type",
                            values_to = "amount")
      
      ggplot(burden_data, aes(x = value_quantile, y = amount, fill = tax_type)) +
        geom_bar(stat = "identity", position = "stack") +
        facet_wrap(~ has_business, 
                   labeller = as_labeller(c(`TRUE` = "With Business", 
                                            `FALSE` = "Without Business"))) +
        scale_fill_manual(values = c("avg_property_tax" = "#3498db",
                                     "avg_business_license" = "#e74c3c"),
                          labels = c("Property Tax", "Business License")) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Average Combined Tax Burden by Quantile and Business Status",
             x = "Property Value Quantile",
             y = "Average Tax Amount",
             fill = "Tax Type") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              legend.position = "bottom")
    })
    
    # Tab 4: Business Impact
    output$business_burden_table <- DT::renderDataTable({
      req(values$data_with_quantiles)
      
      business_burden <- analyze_business_burden(values$data_with_quantiles)
      
      DT::datatable(
        business_burden,
        options = list(
          pageLength = 15,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons'
      ) %>%
        DT::formatRound(columns = c('avg_effective_rate', 'median_effective_rate'), 
                        digits = 2) %>%
        DT::formatCurrency(columns = c('total_tax', 'avg_property_tax', 'avg_business_license'),
                           currency = "", interval = 3, mark = ",")
    })
    
    # Tab 5: Scenario Comparison
    output$winners_losers_table <- DT::renderDataTable({
      req(values$comparison_results)
      
      winners_losers <- analyze_winners_losers(values$comparison_results)
      
      DT::datatable(
        winners_losers,
        options = list(
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons'
      ) %>%
        DT::formatCurrency(columns = names(winners_losers)[grepl("total_tax", names(winners_losers))],
                           currency = "", interval = 3, mark = ",") %>%
        DT::formatRound(columns = names(winners_losers)[grepl("avg_", names(winners_losers))],
                        digits = 2)
    })

    # ============================================
    # Property Lookup Feature (Scenario Comparison Tab)
    # ============================================

    # Reactive value to store property search results
    values$property_comparison <- NULL
    values$property_found <- FALSE

    # Handle property search button
    observeEvent(input$search_property, {
      req(input$property_id_search)
      req(revenue_data())
      
      property_id <- trimws(input$property_id_search)
      
      if (property_id == "") {
        showNotification("Please enter a property ID", type = "warning")
        values$property_found <- FALSE
        return()
      }
      
      withProgress(message = 'Searching for property...', value = 0.5, {
        result <- compare_property_across_scenarios(revenue_data(), property_id)
        
        if (is.null(result)) {
          showNotification(
            paste("Property ID", property_id, "not found in any scenario"),
            type = "error",
            duration = 5
          )
          values$property_found <- FALSE
          values$property_comparison <- NULL
          values$property_characteristics <- NULL
        } else {
          values$property_comparison <- result$comparison
          values$property_characteristics <- result$characteristics
          values$property_found <- TRUE
          showNotification(
            paste("Property", property_id, "found!"),
            type = "message",
            duration = 3
          )
        }
      })
    })

    # Output: property found flag
    output$property_found <- reactive({
      values$property_found
    })
    outputOptions(output, "property_found", suspendWhenHidden = FALSE)

    # Render property characteristics
    output$property_info <- renderUI({
      req(values$property_characteristics)
      char <- values$property_characteristics
      
      tagList(
        div(class = "row",
          div(class = "col-md-3",
              div(class = "info-box bg-aqua",
                  div(class = "info-box-content",
                      span(class = "info-box-text", "Property Types"),
                      span(class = "info-box-number", char$property_types)
                  )
              )
          ),
          div(class = "col-md-3",
              div(class = "info-box bg-green",
                  div(class = "info-box-content",
                      span(class = "info-box-text", "Has Business"),
                      span(class = "info-box-number", char$has_business)
                  )
              )
          ),
          div(class = "col-md-3",
              div(class = "info-box bg-yellow",
                  div(class = "info-box-content",
                      span(class = "info-box-text", "Business Categories"),
                      span(class = "info-box-number", char$business_categories)
                  )
              )
          ),
          div(class = "col-md-3",
              div(class = "info-box bg-red",
                  div(class = "info-box-content",
                      span(class = "info-box-text", "Business Subcategories"),
                      span(class = "info-box-number", char$business_subcategories)
                  )
              )
          )
        )
      )
    })

    # Render the comparison table (UNCHANGED)
    output$property_comparison_table <- DT::renderDataTable({
      req(values$property_comparison)
      
      values$property_comparison %>%
        DT::datatable(
          options = list(
            pageLength = 10,
            dom = 't',
            scrollX = TRUE
          ),
          rownames = FALSE
        ) %>%
        DT::formatCurrency(
          columns = c('Existing', 'Scenario_A', 'Scenario_B', 
                      'Change_A_vs_Existing', 'Change_B_vs_Existing'),
          currency = "",
          interval = 3,
          mark = ","
        ) %>%
        DT::formatRound(
          columns = c('Pct_Change_A', 'Pct_Change_B'),
          digits = 1
        ) %>%
        DT::formatStyle(
          'Change_A_vs_Existing',
          backgroundColor = DT::styleInterval(
            cuts = c(-0.01, 0.01),
            values = c('#ffcccc', '#ffffff', '#ccffcc')
          )
        ) %>%
        DT::formatStyle(
          'Change_B_vs_Existing',
          backgroundColor = DT::styleInterval(
            cuts = c(-0.01, 0.01),
            values = c('#ffcccc', '#ffffff', '#ccffcc')
          )
        )
    })

    # Plot: Property value comparison
    output$property_value_comparison <- renderPlot({
      req(values$property_comparison)
      
      # Extract value data
      value_data <- values$property_comparison %>%
        dplyr::filter(Metric == "Total Property Value") %>%
        tidyr::pivot_longer(
          cols = c(Existing, Scenario_A, Scenario_B),
          names_to = "Scenario",
          values_to = "Value"
        ) %>%
        dplyr::mutate(
          Value = as.numeric(Value),
          Scenario = factor(Scenario, levels = c("Existing", "Scenario_A", "Scenario_B"))
        )
      
      ggplot(value_data, aes(x = Scenario, y = Value, fill = Scenario)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        geom_text(aes(label = scales::comma(Value)), 
                  vjust = -0.5, size = 4) +
        scale_fill_manual(values = c("Existing" = "#95a5a6",
                                      "Scenario_A" = "#3498db",
                                      "Scenario_B" = "#e74c3c")) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Total Property Value Comparison",
            x = NULL,
            y = "Property Value") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "none"
        )
    })

    # Plot: Tax comparison
    output$property_tax_comparison <- renderPlot({
      req(values$property_comparison)
      
      # Extract tax data
      tax_data <- values$property_comparison %>%
        dplyr::filter(Metric %in% c("Total Property Tax", "Total Business License")) %>%
        tidyr::pivot_longer(
          cols = c(Existing, Scenario_A, Scenario_B),
          names_to = "Scenario",
          values_to = "Value"
        ) %>%
        dplyr::mutate(
          Value = as.numeric(Value),
          Scenario = factor(Scenario, levels = c("Existing", "Scenario_A", "Scenario_B"))
        )
      
      ggplot(tax_data, aes(x = Scenario, y = Value, fill = Metric)) +
        geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
        scale_fill_manual(
          values = c("Total Property Tax" = "#3498db",
                    "Total Business License" = "#e74c3c"),
          labels = c("Property Tax", "Business License")
        ) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Total Tax Comparison (Property + Business)",
            x = NULL,
            y = "Tax Amount",
            fill = "Tax Type") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "bottom"
        )
    })
    
    # Tab 6: Special Cases
    output$multi_type_properties <- DT::renderDataTable({
      req(revenue_data())
      
      multi_type <- analyze_multi_type_properties(revenue_data()[[input$analysis_scenario]])
      
      if(nrow(multi_type) == 1 && "message" %in% names(multi_type)) {
        DT::datatable(multi_type)
      } else {
        DT::datatable(
          multi_type,
          options = list(
            pageLength = 25,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel')
          ),
          extensions = 'Buttons'
        ) %>%
          DT::formatCurrency(columns = c('total_value', 'total_property_tax',
                                         'total_business_license', 'total_tax'),
                             currency = "", interval = 3, mark = ",") %>%
          DT::formatRound(columns = 'effective_rate', digits = 2)
      }
    })
    
    output$concentration_metrics <- renderPrint({
      req(values$concentration_results)
      
      cat("=== TAX CONCENTRATION ANALYSIS ===\n\n")
      cat("Top 1% of properties pay:", round(values$concentration_results$top_1pct, 1), "% of total tax\n")
      cat("Top 5% of properties pay:", round(values$concentration_results$top_5pct, 1), "% of total tax\n")
      cat("Top 10% of properties pay:", round(values$concentration_results$top_10pct, 1), "% of total tax\n")
      cat("Top 20% of properties pay:", round(values$concentration_results$top_20pct, 1), "% of total tax\n")
    })
    
    # Return analysis results for use in other modules
    return(reactive({
      list(
        aggregated_data = values$aggregated_data,
        data_with_quantiles = values$data_with_quantiles,
        progressivity_results = values$progressivity_results,
        overtaxed_properties = values$overtaxed_properties,
        comparison_results = values$comparison_results,
        gini_coefficient = values$gini_results$gini_coefficient,
        concentration_metrics = values$concentration_results
      )
    }))
  })
}