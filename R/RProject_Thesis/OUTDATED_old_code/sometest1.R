
# Assuming observations and deployments data are loaded and prepared as described previously

# Function to find the smallest optimal time window for a single deployment
find_smallest_optimal_time_window <- function(incidence_data, iNEXT_output, min_days_per_window) {
  # Extract the estimated total number of species from iNEXT output
  total_estimated_species <- floor(iNEXT_output$Estimator[1])
  target_species <- floor(total_estimated_species * 0.95)
  
  # Determine the date range for the analysis
  start_date <- as.Date(min(incidence_data$date))
  end_date <- as.Date(max(incidence_data$date))
  initial_days_per_window <- as.numeric(end_date - start_date + 1)
  
  detailed_results <- tibble(days_per_window = integer(), success_ratio = numeric(), days_in_window = integer())
  
  # Explore window sizes from the initial down to the minimum specified
  for (days_per_window in seq(initial_days_per_window, min_days_per_window, by = -1)) {
    num_windows <- as.integer((end_date - start_date + 1) / days_per_window)
    num_target_met <- 0
    
    # Evaluate each window
    for (window_start in seq(from = start_date, by = days_per_window, length.out = num_windows)) {
      window_end <- min(window_start + days_per_window - 1, end_date)
      window_data <- incidence_data %>% filter(date >= window_start & date <= window_end)
      observed_species_count <- n_distinct(window_data$scientificName)
      
      if (observed_species_count >= target_species) {
        num_target_met <- num_target_met + 1
      }
    }
    
    success_ratio <- num_target_met / num_windows
    detailed_results <- detailed_results %>% add_row(days_per_window = days_per_window, success_ratio = success_ratio, days_in_window = days_per_window)
  }
  
  return(list(
    smallest_optimal_days_per_window = min(detailed_results$days_per_window[detailed_results$success_ratio == 1]),
    detailed_results = detailed_results
  ))
}

# Function to evaluate multiple deployments
evaluate_multiple_deployments <- function(deployments_data, observations_data, iNEXT_outputs, min_days_per_window, deployment_asy_ratio) {
  results_list <- list()
  
  # Filter the iNEXT output for species richness data only
  iNEXT_outputs <- iNEXT_outputs %>% filter(Diversity == "Species richness")
  
  for (deployment_id in unique(deployments_data$deploymentID)) {
    current_incidence_data <- observations_data %>% filter(deploymentID == deployment_id)
    current_iNEXT_output <- iNEXT_outputs %>% filter(Assemblage == deployment_id)
    
    optimal_result <- find_smallest_optimal_time_window(current_incidence_data, current_iNEXT_output, min_days_per_window)
    
    results_list[[deployment_id]] <- optimal_result
  }
  
  # Aggregate results to calculate the deployment ratio
  detailed_results <- bind_rows(lapply(results_list, "[[", "detailed_results"), .id = "deploymentID")
  deployment_ratios <- sapply(results_list, function(x) max(x$detailed_results$success_ratio) == 1.0)
  total_deployments <- length(unique(deployments_data$deploymentID))
  
  # Calculate the percentage of deployments meeting the deployment ratio criterion
  percentage_met_ratio <- mean(deployment_ratios)
  if (percentage_met_ratio >= deployment_asy_ratio) {
    valid_windows <- detailed_results %>%
      filter(success_ratio == 1) %>%
      group_by(days_per_window) %>%
      summarize(count = n(), .groups = 'drop') %>%
      filter(count / total_deployments >= deployment_asy_ratio)
    
    if (nrow(valid_windows) > 0) {
      smallest_window_meeting_ratio <- min(valid_windows$days_per_window)
      print(paste("The optimal temporal resolution for which", format(deployment_asy_ratio * 100, nsmall = 2), 
                  "% of deployments reach asymptote =", smallest_window_meeting_ratio, "days"))
    } else {
      print("No window size meets the deployment ratio criterion.")
    }
  } else {
    print("Deployment ratio criterion not met by any window size.")
  }
  
  return(list(
    deployment_ratio = percentage_met_ratio,
    detailed_results = detailed_results
  ))
}

# Example usage
results <- evaluate_multiple_deployments(simulation_results$deployments, simulation_results$observations, iNET_out$AsyEst, 1, 1)