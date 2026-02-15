
# ============================================================================
# Model Performance Visualizations
# ============================================================================

create_roc_comparison_plot <- function(roc_objects, model_names = NULL,
                                      output_file = NULL) {
  """
  Create comparison of ROC curves from multiple models
  
  Args:
    roc_objects: List of ROC objects
    model_names: Names of models
    output_file: Output file path
  """
  log_info("Creating ROC comparison plot")
  
  tryCatch({
    if (is.null(model_names)) {
      model_names <- paste0("Model_", seq_along(roc_objects))
    }
    
    # Create plot
    pdf(output_file, width = 10, height = 8)
    
    plot(NULL, xlim = c(0, 1), ylim = c(0, 1),
         xlab = "False Positive Rate",
         ylab = "True Positive Rate",
         main = "ROC Curve Comparison")
    
    colors <- rainbow(length(roc_objects))
    
    for (i in seq_along(roc_objects)) {
      lines(roc_objects[[i]], col = colors[i], lwd = 2)
    }
    
    # Add diagonal line
    lines(c(0, 1), c(0, 1), lty = 2, col = "gray")
    
    # Add legend
    legend("bottomright",
           legend = paste0(model_names, " (AUC = ",
                          round(sapply(roc_objects, auc), 3), ")"),
           col = colors,
           lwd = 2)
    
    dev.off()
    
    log_info(sprintf("✓ ROC comparison plot saved: %s", output_file))
    
  }, error = function(e) {
    log_error(sprintf("Failed to create ROC comparison: %s", e$message))
  })
}

create_calibration_plot <- function(predictions, actual, output_file = NULL) {
  """
  Create calibration plot for model predictions
  
  Args:
    predictions: Predicted probabilities
    actual: Actual outcomes
    output_file: Output file path
  """
  log_info("Creating calibration plot")
  
  tryCatch({
    # Create bins
    n_bins <- 10
    bins <- cut(predictions, breaks = seq(0, 1, length.out = n_bins + 1))
    
    # Calculate observed and expected
    calib_data <- data.frame(
      bin = bins,
      pred = predictions,
      actual = actual
    ) %>%
      group_by(bin) %>%
      summarise(
        expected = mean(pred),
        observed = mean(actual),
        n = n(),
        .groups = 'drop'
      )
    
    # Create plot
    p <- ggplot(calib_data, aes(x = expected, y = observed)) +
      geom_point(aes(size = n), alpha = 0.6, color = "steelblue") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      xlim(0, 1) + ylim(0, 1) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
      ) +
      labs(
        title = "Calibration Plot",
        x = "Expected Probability",
        y = "Observed Frequency",
        size = "N"
      )
    
    if (!is.null(output_file)) {
      ggsave(output_file, p, width = 8, height = 8, dpi = 300)
      log_info(sprintf("✓ Calibration plot saved: %s", output_file))
    }
    
    return(p)
    
  }, error = function(e) {
    log_error(sprintf("Failed to create calibration plot: %s", e$message))
    return(NULL)
  })
}