# ============================================================================
# Survival Visualizations
# ============================================================================

create_kaplan_meier_plot <- function(survival_data, time_col, event_col,
                                    group_col = NULL, output_file = NULL) {
  """
  Create Kaplan-Meier survival curves
  
  Args:
    survival_data: Data frame with survival information
    time_col: Column name for survival time
    event_col: Column name for event status
    group_col: Column name for grouping variable
    output_file: Output file path
  """
  log_info("Creating Kaplan-Meier plot")
  
  tryCatch({
    # Create survival object
    if (!is.null(group_col)) {
      formula_str <- sprintf("Surv(%s, %s) ~ %s", time_col, event_col, group_col)
    } else {
      formula_str <- sprintf("Surv(%s, %s) ~ 1", time_col, event_col)
    }
    
    fit <- survfit(as.formula(formula_str), data = survival_data)
    
    # Create plot
    p <- ggsurvplot(
      fit,
      data = survival_data,
      risk.table = TRUE,
      risk.table.col = "strata",
      pval = TRUE,
      pval.method = TRUE,
      conf.int = TRUE,
      xlab = "Time (days)",
      ylab = "Survival Probability",
      palette = "Set1",
      ggtheme = theme_minimal()
    )
    
    # Save if output file specified
    if (!is.null(output_file)) {
      pdf(output_file, width = 10, height = 8)
      print(p)
      dev.off()
      log_info(sprintf("✓ KM plot saved: %s", output_file))
    }
    
    return(p)
    
  }, error = function(e) {
    log_error(sprintf("Failed to create KM plot: %s", e$message))
    return(NULL)
  })
}

create_survival_comparison_plot <- function(survival_data, time_col, event_col,
                                           groups, output_file = NULL) {
  """
  Create comparison of survival curves across multiple groups
  """
  log_info("Creating survival comparison plot")
  
  tryCatch({
    plots <- list()
    
    for (i in seq_along(groups)) {
      group_name <- names(groups)[i]
      group_data <- survival_data %>%
        filter(!!sym(group_col) == groups[[i]])
      
      fit <- survfit(
        as.formula(sprintf("Surv(%s, %s) ~ 1", time_col, event_col)),
        data = group_data
      )
      
      p <- ggsurvplot(
        fit,
        data = group_data,
        title = group_name,
        xlab = "Time (days)",
        ylab = "Survival Probability",
        ggtheme = theme_minimal()
      )
      
      plots[[i]] <- p$plot
    }
    
    # Combine plots
    combined_plot <- do.call(gridExtra::grid.arrange, c(plots, ncol = 2))
    
    if (!is.null(output_file)) {
      pdf(output_file, width = 14, height = 10)
      print(combined_plot)
      dev.off()
      log_info(sprintf("✓ Comparison plot saved: %s", output_file))
    }
    
    return(combined_plot)
    
  }, error = function(e) {
    log_error(sprintf("Failed to create comparison plot: %s", e$message))
    return(NULL)
  })
}