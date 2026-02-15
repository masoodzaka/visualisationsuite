# ============================================================================
# Distribution and Comparison Plots
# ============================================================================

create_expression_distribution_plot <- function(expression_data, metadata,
                                               output_file = NULL) {
  """
  Create distribution plots of expression by sample type
  """
  log_info("Creating expression distribution plot")
  
  tryCatch({
    # Prepare data
    expr_long <- expression_data %>%
      as.data.frame() %>%
      rownames_to_column("transcript") %>%
      pivot_longer(
        cols = -transcript,
        names_to = "sample",
        values_to = "expression"
      ) %>%
      left_join(metadata, by = c("sample" = "sample_id"))
    
    # Create plot
    p <- ggplot(expr_long, aes(x = expression, fill = sample_type)) +
      geom_density(alpha = 0.6) +
      facet_wrap(~indication, scales = "free") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "bottom"
      ) +
      labs(
        title = "Expression Distribution by Sample Type",
        x = "Expression Level",
        y = "Density",
        fill = "Sample Type"
      )
    
    if (!is.null(output_file)) {
      ggsave(output_file, p, width = 14, height = 10, dpi = 300)
      log_info(sprintf("✓ Distribution plot saved: %s", output_file))
    }
    
    return(p)
    
  }, error = function(e) {
    log_error(sprintf("Failed to create distribution plot: %s", e$message))
    return(NULL)
  })
}