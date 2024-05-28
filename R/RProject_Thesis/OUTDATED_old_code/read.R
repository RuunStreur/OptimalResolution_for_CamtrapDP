# Load necessary library
library(tidyverse)

library(ggplot2)
library(reshape2)

# Load necessary library
library(dplyr)

# Function to calculate statistics for a given file
calculate_statistics <- function(file_path) {
  # Read the data
  data <- read.csv(file_path)
  
  # Calculate the statistics
  stats <- data %>%
    summarise(
      num_deployments = first(num_deployments),
      num_species = first(num_species),
      num_observations_per_deployment = first(num_observations_per_deployment),
      Window_Size_inverse_mean = mean(Window_Size_inverse, na.rm = TRUE),
      Window_Size_inverse_median = median(Window_Size_inverse, na.rm = TRUE),
      Window_Size_inverse_sd = sd(Window_Size_inverse, na.rm = TRUE),
      Window_Size_mean_mean = mean(Window_Size_mean, na.rm = TRUE),
      Window_Size_mean_median = median(Window_Size_mean, na.rm = TRUE),
      Window_Size_mean_sd = sd(Window_Size_mean, na.rm = TRUE)
    )
  
  return(stats)
}

# List of file paths
file_paths <- c(
  "C:/Users/ruuns/Documents/GitHub/Thesis2024/R/RProject_Thesis/RESULTS_1kObs_5Spec_parallel.csv",
  "C:/Users/ruuns/Documents/GitHub/Thesis2024/R/RProject_Thesis/RESULTS_5kObs_5Spec_parallel.csv",
  "C:/Users/ruuns/Documents/GitHub/Thesis2024/R/RProject_Thesis/RESULTS_10kObs_5Spec_parallel.csv",
  "C:/Users/ruuns/Documents/GitHub/Thesis2024/R/RProject_Thesis/RESULTS_1kObs_30Spec_parallel.csv",
  "C:/Users/ruuns/Documents/GitHub/Thesis2024/R/RProject_Thesis/RESULTS_5kObs_30Spec_parallel.csv",
  "C:/Users/ruuns/Documents/GitHub/Thesis2024/R/RProject_Thesis/RESULTS_10kObs_30Spec_parallel.csv",
  "C:/Users/ruuns/Documents/GitHub/Thesis2024/R/RProject_Thesis/RESULTS_1kObs_50Spec_parallel.csv",
  "C:/Users/ruuns/Documents/GitHub/Thesis2024/R/RProject_Thesis/RESULTS_5kObs_50Spec_parallel.csv",
  "C:/Users/ruuns/Documents/GitHub/Thesis2024/R/RProject_Thesis/RESULTS_10kObs_50Spec_parallel.csv"
)

# Apply the function to each file and combine the results
all_stats <- lapply(file_paths, calculate_statistics) %>%
  bind_rows()

# Save the combined statistics to a CSV file
write.csv(all_stats, "combined_window_size_stats.csv", row.names = FALSE)


all_stats <- all_stats %>%
  select(-num_deployments, -Window_Size_inverse_median, -Window_Size_mean_median)


all_stats <- all_stats %>%
  rename(
    Threshold_Metric_mean = Window_Size_inverse_mean,
    Threshold_Metric_sd = Window_Size_inverse_sd,
    Mean_Metric_mean = Window_Size_mean_mean,
    Mean_Metric_sd = Window_Size_mean_sd
  )





library(ggplot2)
library(dplyr)
library(tidyr)

# Assuming all_stats dataframe is already created and available in the environment

# Add a column for parameter sets as x-axis labels
all_stats$parameter_set <- factor(1:nrow(all_stats))

# Reshape the data for easier plotting with ggplot2
all_stats_long <- all_stats %>%
  pivot_longer(cols = c(Window_Size_inverse_mean, Window_Size_mean_mean),
               names_to = "measure",
               values_to = "mean_value") %>%
  pivot_longer(cols = c(Window_Size_inverse_sd, Window_Size_mean_sd),
               names_to = "measure_sd",
               values_to = "sd_value") %>%
  filter((measure == "Window_Size_inverse_mean" & measure_sd == "Window_Size_inverse_sd") |
           (measure == "Window_Size_mean_mean" & measure_sd == "Window_Size_mean_sd"))

# Create the plot
plot <- ggplot(all_stats_long, aes(x = factor(num_species), color = measure)) +
  geom_point(aes(y = mean_value), size = 3) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = 0.2) +
  geom_hline(yintercept = 70, linetype = "dotted", color = "black") +
  annotate("text", x = Inf, y = 70, label = "", hjust = 1.1, vjust = -0.5, color = "black", size = 5) +
  labs(title = "Optimal Temporal Resolution Output Mean and Standard Deviation",
       subtitle = "Faceted by Number of Observations per Deployment",
       x = "Number of Species",
       y = "Window Size",
       color = "Measure") +
  scale_color_manual(values = c("Window_Size_inverse_mean" = "darkolivegreen", "Window_Size_mean_mean" = "salmon"),
                     labels = c("Threshold Metric", "Mean Metric")) +
  facet_wrap(~num_observations_per_deployment, scales = "free_x", labeller = label_both) +
  theme(legend.position = "right",
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 12))

# Print the plot
print(plot)


# Load necessary library
library(dplyr)

# Assuming you have your dataframes in your environment
# results_pipeline11, results_pipeline12, results_pipeline13, results_pipeline2, results_pipeline21, results_pipeline22, results_pipeline23

# List of dataframes
dataframes <- list(results_pipeline11, results_pipeline12, results_pipeline13, results_pipeline21, results_pipeline22, results_pipeline23)

# Combine all dataframes into a single dataframe
combined_df <- bind_rows(dataframes)

# Save the combined dataframe as a CSV file
write.csv(combined_df, file = "experiment2.csv", row.names = FALSE)

