library(tidyverse)
library(ggplot2)
library(ComplexHeatmap)
library(circlize)
library(survival)
library(survminer)
library(gridExtra)
library(plotly)
library(logger)

# ============================================================================
# Heatmap Visualizations
# ============================================================================

create_expression_heatmap <- function(expression_data, metadata, 
                                     top_n = 50, output_file = NULL) {
  """
  Create heatmap of top differentially expressed transcripts
  
  Args:
    expression_data: Expression matrix (transcripts × samples)
    metadata: Sample metadata with annotations
    top_n: Number of top transcripts to display
    output_file: Output file path
  """
  log_info("Creating expression heatmap")
  
  tryCatch({
    # Select top transcripts
    if (nrow(expression_data) > top_n) {
      # Calculate variance for each transcript
      transcript_vars <- apply(expression_data, 1, var)
      top_transcripts <- names(sort(transcript_vars, decreasing = TRUE)[1:top_n])
      heatmap_data <- expression_data[top_transcripts, ]
    } else {
      heatmap_data <- expression_data
    }
    
    # Normalize for visualization
    heatmap_data_scaled <- t(scale(t(heatmap_data)))
    
    # Create column annotations
    col_anno <- HeatmapAnnotation(
      sample_type = metadata$sample_type,
      indication = metadata$indication,
      col = list(
        sample_type = c("cancer" = "red", "normal" = "blue"),
        indication = structure(
          rainbow(length(unique(metadata$indication))),
          names = unique(metadata$indication)
        )
      )
    )
    
    # Create heatmap
    h <- Heatmap(
      heatmap_data_scaled,
      name = "Expression\n(scaled)",
      cluster_rows = TRUE,
      cluster_columns = TRUE,
      show_row_names = FALSE,
      show_column_names = FALSE,
      top_annotation = col_anno,
      col = colorRamp2(c(-2, 0, 2), c("blue", "white", "red")),
      heatmap_legend_param = list(
        title = "Z-score",
        direction = "vertical"
      )
    )
    
    # Save if output file specified
    if (!is.null(output_file)) {
      pdf(output_file, width = 12, height = 8)
      print(h)
      dev.off()
      log_info(sprintf("✓ Heatmap saved: %s", output_file))
    }
    
    return(h)
    
  }, error = function(e) {
    log_error(sprintf("Failed to create heatmap: %s", e$message))
    return(NULL)
  })
}

create_sample_clustering_heatmap <- function(expression_data, metadata,
                                            output_file = NULL) {
  """
  Create heatmap showing sample clustering
  """
  log_info("Creating sample clustering heatmap")
  
  tryCatch({
    # Calculate correlation between samples
    sample_cor <- cor(expression_data)
    
    # Create heatmap
    h <- Heatmap(
      sample_cor,
      name = "Correlation",
      cluster_rows = TRUE,
      cluster_columns = TRUE,
      col = colorRamp2(c(-1, 0, 1), c("blue", "white", "red")),
      show_row_names = FALSE,
      show_column_names = FALSE
    )
    
    if (!is.null(output_file)) {
      pdf(output_file, width = 10, height = 10)
      print(h)
      dev.off()
      log_info(sprintf("✓ Clustering heatmap saved: %s", output_file))
    }
    
    return(h)
    
  }, error = function(e) {
    log_error(sprintf("Failed to create clustering heatmap: %s", e$message))
    return(NULL)
  })
}