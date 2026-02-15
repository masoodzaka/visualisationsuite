# ============================================================================
# Feature Importance Visualizations
# ============================================================================

create_feature_importance_plot <- function(model, feature_names = NULL,
                                          top_n = 20, output_file = NULL) {
  """
  Create feature importance plot from Random Forest model
  
  Args:
    model: Random Forest model object
    feature_names: Names of features
    top_n: Number of top features to display
    output_file: Output file path
  """
  log_info("Creating feature importance plot")
  
  tryCatch({
    # Extract importance
    importance_df <- data.frame(
      feature = rownames(model$importance),
      importance = model$importance[, 1]
    )
    
    # Sort and select top features
    importance_df <- importance_df %>%
      arrange(desc(importance)) %>%
      head(top_n)
    
    # Create plot
    p <- ggplot(importance_df, aes(x = reorder(feature, importance), y = importance)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      coord_flip() +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_text(size = 10)
      ) +
      labs(
        title = "Top Feature Importance",
        x = "Feature",
        y = "Importance"
      )
    
    if (!is.null(output_file)) {
      ggsave(output_file, p, width = 10, height = 8, dpi = 300)
      log_info(sprintf("✓ Feature importance plot saved: %s", output_file))
    }
    
    return(p)
    
  }, error = function(e) {
    log_error(sprintf("Failed to create feature importance plot: %s", e$message))
    return(NULL)
  })
}

create_coefficient_plot <- function(model_coef, top_n = 20, output_file = NULL) {
  """
  Create plot of model coefficients (for Cox or logistic regression)
  
  Args:
    model_coef: Model coefficients
    top_n: Number of top coefficients to display
    output_file: Output file path
  """
  log_info("Creating coefficient plot")
  
  tryCatch({
    # Convert to data frame
    coef_df <- data.frame(
      feature = names(model_coef),
      coefficient = as.numeric(model_coef)
    )
    
    # Sort and select top features
    coef_df <- coef_df %>%
      arrange(desc(abs(coefficient))) %>%
      head(top_n)
    
    # Create plot
    p <- ggplot(coef_df, aes(x = reorder(feature, coefficient), y = coefficient)) +
      geom_col(aes(fill = coefficient > 0), alpha = 0.8) +
      scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "blue")) +
      coord_flip() +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "none"
      ) +
      labs(
        title = "Model Coefficients",
        x = "Feature",
        y = "Coefficient"
      )
    
    if (!is.null(output_file)) {
      ggsave(output_file, p, width = 10, height = 8, dpi = 300)
      log_info(sprintf("✓ Coefficient plot saved: %s", output_file))
    }
    
    return(p)
    
  }, error = function(e) {
    log_error(sprintf("Failed to create coefficient plot: %s", e$message))
    return(NULL)
  })
}