plot_metrics <- function(metric_scores) {
  max_inverse <- metric_scores$Window_Size[which.max(metric_scores$Inverse)]
  max_mean_inverse <- metric_scores$Window_Size[which.max(metric_scores$Mean_inverse)]
  max_mean_asymptote_ratio <- metric_scores$Window_Size[which.max(metric_scores$Mean_Asymptote_Ratio)]
  max_ratio_exceeded_threshold <- metric_scores$Window_Size[which.max(metric_scores$Ratio_Exceeded_Threshold)]
  
  ggplot(metric_scores, aes(x = Window_Size)) +
    geom_line(aes(y = Inverse * 50, color = "Inverse"), size = 1) +
    geom_line(aes(y = Mean_inverse * 50, color = "Mean_inverse"), size = 1) +
    geom_line(aes(y = Mean_Asymptote_Ratio, color = "Mean_Asymptote_Ratio"), size = 1) +
    geom_line(aes(y = Ratio_Exceeded_Threshold, color = "Ratio_Exceeded_Threshold"), size = 1) +
    geom_vline(xintercept = max_inverse, linetype = "dashed", color = "darkolivegreen2") +
    geom_vline(xintercept = max_mean_inverse, linetype = "dashed", color = "lightsalmon") +
    scale_color_manual(values = c(
      "Inverse" = "darkolivegreen2",
      "Mean_inverse" = "lightsalmon",
      "Mean_Asymptote_Ratio" = "darkgrey",
      "Ratio_Exceeded_Threshold" = "grey"
    )) +
    labs(
      title = "Behavior of Metrics over Different Window Sizes",
      x = "Window Size (days)",
      y = "Metric Score",
      color = "Metric"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "top"
    )
}

# Plot the metrics
plot_metrics(INTERRESULT_metric_scores)