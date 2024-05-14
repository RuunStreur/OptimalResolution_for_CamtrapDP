library(dplyr)
library(purrr)
library(tidyverse)
library(iNEXT)

observations = simulation_results$observations
deployments = simulation_results$deployments

Preprocess <- function(observations, deployments) {
  
  deployments$deploymentStart <- as.Date(deployments$deploymentStart)
  deployments$deploymentEnd <- as.Date(deployments$deploymentEnd)
  
  observations$date <- as.Date(observations$eventStart)
  deployments$time_active <- as.numeric(deployments$deploymentEnd - deployments$deploymentStart)
  # remove rows without a valid eventStart, scientificName
  observations <- observations %>%
    filter(scientificName != "" & !is.na(scientificName) & !is.na(date))
  
  # remove deployments with less than y observations or less time active than 1 day
  y <- 10
  obs_count <- observations %>%
    group_by(deploymentID) %>%
    summarise(n = n(), .groups = 'drop')
  deployments <- deployments %>%
    inner_join(obs_count, by = "deploymentID") %>%
    filter(n >= y) %>%
    select(-n)
  
  deployments <- deployments[deployments$time_active >= 1, ]
  
  # only include observations that are in the remaining deployments
  observations <- observations[observations$deploymentID %in% deployments$deploymentID, ]
  return(list(observations = observations, deployments = deployments))
}

preprocessed_data <- Preprocess(observations, deployments)
observations <- preprocessed_data$observations
deployments <- preprocessed_data$deployments
deployment_test <- observations %>% filter(deploymentID == "Deployment_2")

###
Calculate_iNEXT <- function(observations, deployments) {
  deployments_inc_mats <- list()
  for (deployment in unique(deployments$deploymentID)) {
    deploy_start <- deployments$deploymentStart[deployments$deploymentID == deployment]
    deploy_end <- deployments$deploymentEnd[deployments$deploymentID == deployment]
    deploy_obs <- observations[observations$deploymentID == deployment,]
    deploy_spec <- unique(deploy_obs$scientificName)
    deploy_days <- seq.Date(deploy_start, deploy_end, by = "day")
    
    inc_mat <- matrix(0, nrow = length(deploy_spec), ncol = length(deploy_days))
    row.names(inc_mat) <- deploy_spec
    colnames(inc_mat) <- as.character(deploy_days)
    
    for (i in 1:length(deploy_days)) {
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
  
  Calculate_iNEXT_results <<- as.data.frame(iNET_out$AsyEst)
  return(Calculate_iNEXT_results)
}



###
Check_asymptote <- function(split_data, AsyEst, species_asymptote_threshold) {
  observed_species <- length(unique(split_data$scientificName))
  deployment_id <- unique(split_data$deploymentID)
  result_df <- data.frame(
    DeploymentID = deployment_id,
    Start_date = min(split_data$date),
    End_date = max(split_data$date),
    N_Days = as.integer(max(split_data$date) - min(split_data$date) + 1),
    AsyEst = AsyEst,
    Target = floor(AsyEst * species_asymptote_threshold),
    Observed = observed_species,
    Reached = observed_species >= floor(AsyEst * species_asymptote_threshold),
    stringsAsFactors = FALSE
  )
  
  # Append results to the global data frame
  INTERRESULT_check_asymptote <<- rbind(INTERRESULT_check_asymptote, result_df)
  
  return(result_df)
}



###
daterange_asymptote_ratio_calculator <- function(observations, deployments, AsyEsts, daterange, species_asymptote_threshold) {
  # Convert date range into start and end dates
  start_date <- as.Date(daterange$start)
  end_date <- as.Date(daterange$end)
  
  results <- list()  # Prepare an empty list to store results
  
  # Iterate over each deployment
  for (deployment_id in unique(deployments$deploymentID)) {
    deployment_data <- observations %>% 
      filter(deploymentID == deployment_id, date >= start_date, date <= end_date)
    
    # Check if deployment data is empty
    if (nrow(deployment_data) == 0) {
      results[[deployment_id]] <- NULL  # Explicitly assign NULL to represent no data
    } else {
      # Get the asymptotic estimate for this deployment
      AsyEst <- AsyEsts %>% 
        filter(Assemblage == deployment_id, Diversity == "Species richness") %>%
        pull(Estimator)
      
      # Check if this window reaches asymptote
      result <- Check_asymptote(deployment_data, AsyEst, species_asymptote_threshold)
      results[[deployment_id]] <- result
    }
  }
  
  # Filter out NULL entries and calculate the ratio
  valid_results <- results[!sapply(results, is.null)]  # Remove NULL entries
  n_deployments <- length(valid_results)  # Count valid deployments
  reached_asymptote_ratio <- mean(sapply(valid_results, `[[`, "Reached"), na.rm = TRUE)  # Calculate mean of 'Reached' values
  
  results_daterange <<- data.frame(
    Daterange = paste(format(start_date, "%d/%m/%y"), "-", format(end_date, "%d/%m/%y")),
    N_Days = as.integer(end_date - start_date + 1),
    N_Deployments = n_deployments,
    Reached_Asymptote_Ratio = reached_asymptote_ratio
  )
  
  return(data.frame(
    Daterange = paste(format(start_date, "%d/%m/%y"), "-", format(end_date, "%d/%m/%y")),
    N_Days = as.integer(end_date - start_date + 1),
    N_Deployments = n_deployments,
    Reached_Asymptote_Ratio = reached_asymptote_ratio
  ))
}

#daterange_asymptote_ratio_calculator(observations, deployments, Calculate_iNEXT_results, daterange_example, .95)

###
all_windows_for_windowsize_calculator <- function(observations, deployments, AsyEsts, window_size, species_asymptote_threshold, reached_asymptote_ratio_threshold) {
  step <- window_size
  date_ranges <- list()
  first_observation_date <- min(observations$date)
  last_observation_date <- max(observations$date)
  
  start_dates <- seq(first_observation_date, last_observation_date - step, by = step)
  end_dates <- start_dates + step - 1
  n_windows <- length(start_dates)  # Calculate number of windows
  
  # Collect results for each window
  results <- mapply(function(start, end) {
    daterange <- list(start = start, end = end)
    daterange_asymptote_ratio_calculator(observations, deployments, AsyEsts, daterange, species_asymptote_threshold)
  }, start_dates, end_dates, SIMPLIFY = FALSE)
  
  results_df <- do.call(rbind, results)
  results_df$Exceeded_threshold <- results_df$Reached_Asymptote_Ratio >= reached_asymptote_ratio_threshold
  results_df$N_windows <- n_windows
  # print(results_df)
  INTERRESULT_all_windows_for_windowsize_calculator <<- rbind(INTERRESULT_all_windows_for_windowsize_calculator, results_df)

  return(results_df)
}

###
all_windows_for_all <- function(observations, deployments, AsyEsts, min_window, max_window, step_size, species_asymptote_threshold, reached_asymptote_ratio_threshold) {
  INTERRESULT_check_asymptote <<- data.frame(
    DeploymentID = character(),
    Start_date = as.Date(character()),
    End_date = as.Date(character()),
    N_Days = integer(),
    AsyEst = numeric(),
    Target = integer(),
    Observed = integer(),
    Reached = logical(),
    stringsAsFactors = FALSE
  )
  
  # Initialize a global dataframe to store results from all_windows_for_windowsize_calculator
  INTERRESULT_all_windows_for_windowsize_calculator <<- data.frame(
    Daterange = character(),
    N_Days = integer(),
    N_Deployments = integer(),
    Reached_Asymptote_Ratio = numeric(),
    Exceeded_threshold = logical(),
    N_windows = integer(),
    stringsAsFactors = FALSE  # Setting this to FALSE to prevent automatic conversion of character vectors to factors
  )
  
  results <- list()
  
  for (window_size in seq(min_window, max_window, by = step_size)) {
    window_results <- all_windows_for_windowsize_calculator(observations, deployments, AsyEsts, window_size, species_asymptote_threshold, reached_asymptote_ratio_threshold)
    results[[as.character(window_size)]] <- window_results
  }
  
  # Combine all results into a single data frame
  final_results <<- do.call(rbind, lapply(seq(min_window, max_window, by = step_size), function(window_size) {
    df <- results[[as.character(window_size)]]
    data.frame(Window_Size = window_size,
               N_windows = unique(df$N_windows),
               Mean_Asymptote_Ratio = mean(df$Reached_Asymptote_Ratio, na.rm = TRUE), 
               Min_Asymptote_Ratio = min(df$Reached_Asymptote_Ratio, na.rm = TRUE), 
               Max_Asymptote_Ratio = max(df$Reached_Asymptote_Ratio, na.rm = TRUE), 
               Ratio_Exceeded_Threshold = mean(df$Exceeded_threshold, na.rm = TRUE)
    )
  }))
  
  return(final_results)
}

Calculate_iNEXT(observations, deployments)
# Run the all windows analysis
all_windows_for_all(observations, deployments, Calculate_iNEXT_results, 7, 105, 7, .95, 1)
