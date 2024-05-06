library(tidyverse)
library(iNEXT)
library(sf)
library(lubridate)

observations <- observations_artis
deployments <- deployments_artis
observations$date <- as.Date(observations$date)


deployments_inc_mats <- list()
for(deployment in unique(deployments$deploymentID)) {
  deploy_start <- deployments$deploymentStart[deployments$deploymentID == deployment]
  deploy_end <- deployments$deploymentEnd[deployments$deploymentID == deployment]
  deploy_obs <- observations[observations$deploymentID == deployment,]
  deploy_spec <- unique(deploy_obs$scientificName)
  deploy_days <- seq.Date(deploy_start, deploy_end, by = "day")
  
  inc_mat <- matrix(0, nrow = length(deploy_spec), ncol = length(deploy_days))
  row.names(inc_mat) <- deploy_spec
  colnames(inc_mat) <- as.character(deploy_days)
  
  for(i in 1:length(deploy_days)) {
    currday <- deploy_days[i]
    curr_spec <- deploy_obs$scientificName[deploy_obs$date == currday]
    present_bool <- deploy_spec %in% curr_spec
    inc_mat[, i] = as.numeric(present_bool)
  }
  
  deployments_inc_mats[[deployment]] <- inc_mat
}


iNET_out <- iNEXT(deployments_inc_mats,
                  q=c(0),
                  datatype="incidence_raw",
                  endpoint=750)

iNET_out$AsyEst

curveplot <- ggiNEXT(iNET_out, type = 1)
curveplot + labs(y='Species richness', x='Deployment days')

# Function to find the smallest optimal time window for a single deployment
find_smallest_optimal_time_window <- function(incidence_data, iNEXT_output, min_days_per_window) {
  # Extracting asymptotic species estimate from iNEXT output
  total_estimated_species <- floor(iNEXT_output$Estimator[1])
  target_species <- floor(total_estimated_species * .95)
  print(target_species)
  
  # Setting up date range for analysis
  start_date <- as.Date(min(incidence_data$date))
  end_date <- as.Date(max(incidence_data$date))
  initial_days_per_window <- as.numeric(end_date - start_date + 1)
  
  # Initialize variables for tracking the optimal window
  last_successful_days_per_window <- NULL
  detailed_results <- tibble(days_per_window = integer(), success_ratio = numeric(), days_in_window = integer())
  
  for (days_per_window in seq(initial_days_per_window, min_days_per_window, by = -1)) {
    num_windows <- as.integer((end_date - start_date + 1) / days_per_window)
    num_target_met <- 0
    
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
    
    if (success_ratio == 1) {
      last_successful_days_per_window <- days_per_window
    }
  }
  
  if (!is.null(last_successful_days_per_window)) {
    return(list(
      smallest_optimal_days_per_window = last_successful_days_per_window,
      detailed_results = detailed_results
    ))
  } else {
    return(list(
      message = "No window size found in which asymptote is reached.",
      detailed_results = detailed_results
    ))
  }
}

# Function to evaluate multiple deployments
evaluate_multiple_deployments <- function(deployments_data, observations_data, iNEXT_outputs, min_days_per_window, deployment_asy_ratio) {
  results_list <- list()
  
  # Filter iNEXT output for species richness only
  iNEXT_outputs <- iNEXT_outputs %>% filter(Diversity == "Species richness")
  
  for (deployment_id in unique(deployments_data$deploymentID)) {
    current_incidence_data <- observations_data %>% filter(deploymentID == deployment_id)
    current_iNEXT_output <- iNEXT_outputs %>% filter(Assemblage == deployment_id)
    
    optimal_result <- find_smallest_optimal_time_window(current_incidence_data, current_iNEXT_output, min_days_per_window)
    
    results_list[[deployment_id]] <- optimal_result
  }
  
  success_rate <- mean(sapply(results_list, function(x) x$detailed_results$success_ratio[1] >= deployment_asy_ratio))
  detailed_results <- bind_rows(lapply(results_list, "[[", "detailed_results"), .id = "deploymentID")
  
  return(list(
    success_rate = success_rate,
    detailed_results = detailed_results
  ))
}

# Example usage with actual data
results <- evaluate_multiple_deployments(deployments, observations, iNET_out$AsyEst, 1, 0.95)
print(results$success_rate)
print(results$detailed_results)


































