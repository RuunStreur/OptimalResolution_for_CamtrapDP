library(tidyverse)
library(lubridate)
library(iNEXT)

set.seed(3990)

# Wrapper function to perform all steps multiple times
whole_pipeline <- function(n_runs, num_species, num_observations_per_deployment, sdlog, target_window_size, min_window, max_window, step_size, species_asymptote_threshold, reached_asymptote_ratio_threshold) {
  results_list <- vector("list", n_runs)
  
  for (run in 1:n_runs) {
    
    # Simulate data
    print(paste('Simulating data for run',run,'...'))
    simulation_results <- simulate_multiple_deployments(4, 365, num_observations_per_deployment, target_window_size, sdlog = sdlog)
    observations <- simulation_results$observations
    deployments <- simulation_results$deployments
    
    # Process data
    processed_data <- Preprocess(observations, deployments)
    observations <- processed_data$observations
    deployments <- processed_data$deployments
    
    # Calculate iNEXT estimates
    print(paste('Calculating iNEXT for run', run, '...'))
    iNEXT_results <- Calculate_iNEXT(observations, deployments)
    
    # Run the all windows analysis
    print(paste('Running analysis for run', run, '...'))
    
    final_results <- all_windows_for_all(observations, deployments, iNEXT_results, min_window, max_window, step_size, species_asymptote_threshold, reached_asymptote_ratio_threshold)
    
    # Compute advanced metrics
    optimized_values <- compute_advanced_metrics(final_results)
    
    # Extract the best result based on Simple Inverse
    top_simple_inverse <- optimized_values %>%
      arrange(desc(Simple_Inverse)) %>%
      slice(1) %>%
      select(Window_Size, Ratio_Exceeded_Threshold)
    
    # Store results in a list
    results_list[[run]] <- top_simple_inverse
  }
  
  # Combine all results into a single dataframe
  final_output <- bind_rows(results_list, .id = "Run")
  return(final_output)
}

# Example usage:
# Adjust the parameters according to your specific setup and requirements
optimal_results <- whole_pipeline(n_runs = 2, num_species = 30, num_observations_per_deployment = 1000, sdlog = 2, target_window_size = 69, min_window = 7, max_window = 105, step_size = 7, species_asymptote_threshold = 0.95, reached_asymptote_ratio_threshold = 1)
print(optimal_results)

save(optimal_results,file='results_100_70_1kobs.Rda')
saus<-load("results_100_70_10k.Rda")
