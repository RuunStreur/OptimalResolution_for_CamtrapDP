library(dplyr)
library(purrr)
library(tidyverse)
library(iNEXT)

observations = observations_artis
deployments = deployments_artis

#deployment_2 <- observations %>% filter(deploymentID == "Deployment_2")

Calculate_iNEXT <- function(observations, deployments) {
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
  # Validate data before using it in any function
  if(anyNA(inc_mat)) {
    stop("NA values detected in the incidence matrix, which could lead to errors.")
  }
  
  
  
  iNET_out <- iNEXT(deployments_inc_mats,
                    q=c(0),
                    datatype="incidence_raw",
                    endpoint=750)
  
  return(iNET_out$AsyEst)
}

Calculate_iNEXT(observations, deployments)

Check_asymptote <- function(split_data, AsyEst, species_asymptote_threshold) {
  observed_species <- length(unique(split_data$scientificName))
  result <- (list(
    Start_date = min(split_data$date),
    End_date = max(split_data$date),
    N_Days = as.integer(max(split_data$date) - min(split_data$date) + 1),
    AsyEst = AsyEst,
    Target = floor(AsyEst * species_asymptote_threshold),
    Observed = observed_species,
    Reached = observed_species >= floor(AsyEst * species_asymptote_threshold)
  ))
  return(result)
}

#Check_asymptote(observations, 29, .99)

Deployment_window_calculator <- function(deployment_data, window_size, AsyEst, species_asymptote_threshold) {
  # Convert window_size to days if needed (e.g., if window_size is provided in another unit)
  window_size_days <- as.numeric(window_size)
  
  # Ensure the sequence includes the last day by adding a buffer if necessary
  period_end <- max(deployment_data$date)
  period_start <- min(deployment_data$date)
  total_days <- as.integer(period_end - period_start + 1)
  
  # Correctly determine the number of windows that can be fully formed
  n_full_windows <- total_days %/% window_size_days
  dropped_days <- total_days %% window_size_days
  
  # Create date splits using full windows only
  split_dates <- seq(from = period_start, length.out = n_full_windows + 1, by = window_size_days)
  
  results <- list()
  for (i in seq_along(split_dates)[-length(split_dates)]) {
    split_data <- deployment_data[deployment_data$date >= split_dates[i] & deployment_data$date < split_dates[i + 1], ]
    results[[i]] <- Check_asymptote(split_data, AsyEst, species_asymptote_threshold)
  }
  
  # for (result in results){print(result)}
  
  n_windows <- length(results)
  reached <- sum(sapply(results, `[[`, "Reached"))
  result <- list(
    Deployment = unique(deployment_data$deploymentID),
    Window_Size = window_size_days,
    N_Windows = n_windows,
    Dropped_days = dropped_days,
    Asymptote_Ratio = reached / n_windows
  )
  return(result)
}

#Deployment_window_calculator(deployment_2, 60, 29, .95)


Observations_calculator <- function(observations_data, deployments_data, min_window, max_window, step_size, species_asymptote_threshold, deployment_asymptote_threshold) {
  results <- list()
  summary_results <- data.frame(Window_Size = integer(), Asymptote_Ratio = numeric())
  AsyEst_info <- Calculate_iNEXT(observations_data, deployments_data)
  print(AsyEst_info)
  
  for (window_size in seq(min_window, max_window, by = step_size)) {
    window_results <- list()
    
    cat("\nProcessing window size:", window_size, "days\n")
    
    for (deployment_id in unique(deployments_data$deploymentID)) {
      deployment_data <- observations_data[observations_data$deploymentID == deployment_id, ]

      if (nrow(deployment_data) == 0) {
        cat("No data for deployment:", deployment_id, "Skipping...\n")
        next
      }
      
      # Extract the AsyEst specifically for 'Species richness' for the current deployment
      current_asyest <- AsyEst_info %>%
        filter(Assemblage == deployment_id, Diversity == 'Species richness') %>%
        pull(Estimator)
      print(deployment_id)
      print(current_asyest)
      
      if (is.na(current_asyest)) {
        cat("AsyEst is NA for deployment:", deployment_id, ". Check input data.\n")
        next
      }
      
      analysis_result <- Deployment_window_calculator(deployment_data, window_size, current_asyest, species_asymptote_threshold)
      window_results[[deployment_id]] <- analysis_result
      
      cat("Deployment:", deployment_id, "Window Size:", window_size, "Asymptote Ratio:", analysis_result$Asymptote_Ratio, "\n")
    }
    
    if (length(window_results) > 0) {
      # Aggregate results for current window size
      combined_results <- do.call(rbind, window_results)
      results[[as.character(window_size)]] <- combined_results
      reached_ratio <- mean(sapply(window_results, function(x) x$Asymptote_Ratio >= deployment_asymptote_threshold))
      summary_results <- rbind(summary_results, data.frame(Window_Size = window_size, Asymptote_Ratio = reached_ratio))
    } else {
      cat("No valid data processed for window size:", window_size, "\n")
    }
  }
  return(list(Detailed_Results = results, Summary_Results = summary_results))
}


Observations_calculator(observations, deployments, 60, 180, 60, 0.95, 1)

