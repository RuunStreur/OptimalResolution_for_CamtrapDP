library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(cowplot)

# Function to create a plot for a specific dataset and window size
create_plot <- function(data, window_size) {
  data$parameter_set <- factor(1:nrow(data))
  
  data_long <- data %>%
    pivot_longer(cols = c(Threshold_Metric_mean, Mean_Metric_mean),
                 names_to = "measure",
                 values_to = "mean_value") %>%
    pivot_longer(cols = c(Threshold_Metric_sd, Mean_Metric_sd),
                 names_to = "measure_sd",
                 values_to = "sd_value") %>%
    filter((measure == "Threshold_Metric_mean" & measure_sd == "Threshold_Metric_sd") |
             (measure == "Mean_Metric_mean" & measure_sd == "Mean_Metric_sd"))
  
  p <- ggplot(data_long, aes(x = factor(num_species), color = measure)) +
    geom_point(aes(y = mean_value), size = 3) +
    geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = 0.2) +
    geom_hline(yintercept = window_size, linetype = "dotted", color = "black") +
    annotate("text", x = Inf, y = window_size / 2, label = "", hjust = 1.1, vjust = -0.5, color = "black", size = 5) +
    labs(subtitle = paste("Target:", window_size),
         x = NULL,
         y = NULL,
         color = "Measure") +
    scale_color_manual(values = c("Threshold_Metric_mean" = "darkolivegreen", "Mean_Metric_mean" = "salmon"),
                       labels = c("Threshold Metric", "Mean Metric")) +
    facet_wrap(~num_observations_per_deployment, scales = "free_x", labeller = as_labeller(function(x) paste("Observations:", x))) +
    theme(plot.subtitle = element_text(size = 18),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          strip.text = element_text(size = 12))
  
  return(p)
}

# Generate plots for each dataset
plot_14 <- create_plot(all_stats_14, 14)
plot_70 <- create_plot(all_stats_70, 70)
plot_140 <- create_plot(all_stats_140, 140)

# Extract legend from one of the plots
legend <- get_legend(plot_70 + theme(legend.position = "right"))

# Remove legends from the individual plots
plot_14 <- plot_14 + theme(legend.position = "none")
plot_70 <- plot_70 + theme(legend.position = "none")
plot_140 <- plot_140 + theme(legend.position = "none")

# Combine plots into a 3x1 grid with a common legend
combined_plot <- grid.arrange(plot_14, plot_70, plot_140, ncol = 1,
                              top = textGrob("Summarized Model Results for Simulated Data",
                                             gp = gpar(fontsize = 24, fontface = "bold"),
                                             hjust = 0,  # Align to the left
                                             x = 0),     # Align to the left
                              left = textGrob("Window Size", rot = 90, gp = gpar(fontsize = 24)),
                              bottom = textGrob("Number of Species", gp = gpar(fontsize = 24)))

# Add the common legend to the combined plot
final_plot <- plot_grid(combined_plot, legend, ncol = 2, rel_widths = c(5, 1))

# Print the combined plot
print(final_plot)
