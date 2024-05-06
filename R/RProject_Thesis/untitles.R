Observations_calculator <- function(observations_data, deployments_data, min_window, max_window, step_size, species_asymptote_threshold, deployment_asymptote_threshold) {
  check_asymptote_results_all <<- data.frame()
  
  results <- list()
  summary_results <- data.frame(Window_Size = integer(), Deployments_Reached_Asymptote_Ratio = numeric(), Mean_Asymptote_Ratio = numeric(), Min_Asymptote_Ratio = numeric(), Max_Asymptote_Ratio = numeric())
  print('Calculating iNEXT...')
  AsyEst_info <- Calculate_iNEXT(observations_data, deployments_data)
  print('iNEXT calculated')
  
  print('Analyzing windows...')
  for (window_size in seq(min_window, max_window, by = step_size)) {
    window_results <- list()
    
    for (deployment_id in unique(deployments_data$deploymentID)) {
      deployment_data <- observations_data[observations_data$deploymentID == deployment_id, ]
      if (nrow(deployment_data) == 0) {
        print('nrow == 0, next')
        next
      }
      current_asyest <- AsyEst_info %>%
        filter(Assemblage == deployment_id, Diversity == 'Species richness') %>%
        pull(Estimator)
      
      if (is.na(current_asyest)) {
        next
      }
      analysis_result <- Deployment_window_calculator(deployment_data, window_size, current_asyest, species_asymptote_threshold)
      window_results[[deployment_id]] <- analysis_result
    }
    
    if (length(window_results) > 0) {
      combined_results <- do.call(rbind, window_results)
      results[[as.character(window_size)]] <- combined_results
      mean_ratio <- mean(combined_results$Asymptote_Ratio, na.rm = TRUE)
      min_ratio <- min(combined_results$Asymptote_Ratio, na.rm = TRUE)
      max_ratio <- max(combined_results$Asymptote_Ratio, na.rm = TRUE)
      deployments_reached_ratio <- mean(combined_results$Asymptote_Ratio >= deployment_asymptote_threshold, na.rm = TRUE)
      summary_results <- rbind(summary_results, data.frame(Window_Size = window_size, Deployments_Reached_Asymptote_Ratio = deployments_reached_ratio, Mean_Asymptote_Ratio = mean_ratio, Min_Asymptote_Ratio = min_ratio, Max_Asymptote_Ratio = max_ratio))
    }
  }
  
  all_detailed_results <- bind_rows(results, .id = "Window_Size")
  Observations_calculator_results <<- list(Detailed_Results = all_detailed_results, Summary_Results = summary_results)
  return(Observations_calculator_results)
}

# Run the observations calculator with provided parameters
results <- Observations_calculator(observations, deployments, min_window = 7, max_window = 400, step_size = 14, species_asymptote_threshold = .9, deployment_asymptote_threshold = 1)
print(results$Summary_Results)



