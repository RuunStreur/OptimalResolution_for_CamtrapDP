library(tidyverse)
library(lubridate)
library(iNEXT)

# Example deployment and observations data should be defined here

# Define the function to compute deployment_asy_ratio
calculate_deployment_asy_ratio <- function(window_duration, deployments_data, observations_data, iNEXT_outputs, target_species_ratio) {
  results_list <- list()
  
  # Iterate through each deployment
  for (deployment_id in unique(deployments_data$deploymentID)) {
    current_incidence_data <- observations_data %>% filter(deploymentID == deployment_id)
    current_iNEXT_output <- iNEXT_outputs %>% filter(Assemblage == deployment_id, Diversity == "Species richness")
    estimator <- current_iNEXT_output$Estimator[1] * target_species_ratio
    
    # Split data according to window_duration and calculate success
    start_date <- min(current_incidence_data$date)
    end_date <- max(current_incidence_data$date)
    periods <- ceiling(as.numeric(end_date - start_date + 1) / window_duration)
    success_count <- sum(sapply(1:periods, function(p) {
      window_start <- start_date + (p - 1) * window_duration
      window_end <- min(window_start + window_duration - 1, end_date)
      observed_species_count <- current_incidence_data %>% 
        filter(date >= window_start & date <= window_end) %>%
        summarise(n_distinct_species = n_distinct(scientificName)) %>%
        pull(n_distinct_species)
      observed_species_count >= estimator
    }))
    
    results_list[[deployment_id]] <- success_count / periods
  }
  
  # Calculate average deployment_asy_ratio across all deployments
  mean(unlist(results_list))
}

# Define the optimization function
optimization_function <- function(window_duration, deployments_data, observations_data, iNEXT_outputs, target_species_ratio, w1, w2) {
  deployment_asy_ratio <- calculate_deployment_asy_ratio(window_duration, deployments_data, observations_data, iNEXT_outputs, target_species_ratio)
  # Objective: Maximize deployment_asy_ratio and minimize window_duration
  f_x <- w1 * (1 - deployment_asy_ratio) + w2 * window_duration
  return(f_x)
}

# Define parameters
deployments_data <- simulation_results$deployments
observations_data <- simulation_results$observations
iNEXT_outputs <- iNET_out$AsyEst
target_species_ratio <- 0.95
w1 <- 1
w2 <- 0.01  # Adjust weights as necessary to balance the objectives

# Run optimization
optim_result <- optim(par = 30,  # Initial guess for the window duration
                      fn = optimization_function,
                      method = "BFGS",
                      deployments_data = deployments_data,
                      observations_data = observations_data,
                      iNEXT_outputs = iNEXT_outputs,
                      target_species_ratio = target_species_ratio,
                      w1 = w1,
                      w2 = w2)

print(optim_result$par)  # Prints the optimal window duration in days
