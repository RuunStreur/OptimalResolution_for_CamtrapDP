library(tidyverse)
library(lubridate)
library(iNEXT)
library(dplyr)
library(purrr)

set.seed(114)

Preprocess <- function(observations, deployments) {
  
  deployments$deploymentStart <- as.Date(deployments$deploymentStart)
  deployments$deploymentEnd <- as.Date(deployments$deploymentEnd)
  
  observations$date <- as.Date(observations$eventStart)
  deployments$time_active <- as.numeric(deployments$deploymentEnd - deployments$deploymentStart)
  
  # Remove rows without a valid eventStart, scientificName
  observations <- observations %>%
    filter(scientificName != "" & !is.na(scientificName) & !is.na(date))
  
  # Remove deployments with less than y observations or less time active than 1 day
  y <- 10
  obs_count <- observations %>%
    group_by(deploymentID) %>%
    summarise(obs_count = n(), .groups = 'drop')
  
  deployments <- deployments %>%
    inner_join(obs_count, by = "deploymentID") %>%
    filter(obs_count >= y)
  
  # Remove deployments with less time active than 1 day
  deployments <- deployments[deployments$time_active >= 1, ]
  
  # Only include observations that are in the remaining deployments
  observations <- observations[observations$deploymentID %in% deployments$deploymentID, ]
  
  return(list(observations = observations, deployments = deployments))
}


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
  
  INTERRESULT_iNEXT <<- as.data.frame(iNET_out$AsyEst)
  return(INTERRESULT_iNEXT)
}

Check_asymptote <- function(split_data, AsyEst, species_asymptote_threshold, start_date, end_date) {
  observed_species <- 0
  observed_species <- length(unique(split_data$scientificName))
  deployment_id <- unique(split_data$deploymentID)
  result_df <- data.frame(
    DeploymentID = deployment_id,
    # Start_date = min(split_data$date),
    # End_date = max(split_data$date),
    # N_Days = as.integer(max(split_data$date) - min(split_data$date) + 1),
    Start_date = start_date,
    End_date = end_date,
    N_Days = as.integer(end_date - start_date + 1),
    AsyEst = AsyEst,
    Target = max(floor(AsyEst * species_asymptote_threshold), 1),
    Observed = observed_species,
    Reached = observed_species >= max(floor(AsyEst * species_asymptote_threshold), 1),
    stringsAsFactors = FALSE
  )
  
  INTERRESULT_check_asymptote <<- rbind(INTERRESULT_check_asymptote, result_df)
  
  return(result_df)
}

daterange_asymptote_ratio_calculator <- function(observations, deployments, AsyEsts, daterange, species_asymptote_threshold) {
  start_date <- as.Date(daterange$start)
  end_date <- as.Date(daterange$end)
  
  results <- list()
  
  for (deployment_id in unique(deployments$deploymentID)) {
    observations_for_deployment <- observations %>%
      filter(deploymentID == deployment_id, date >= start_date, date <= end_date)
    
    # Check if deployment data is empty
    if (nrow(observations_for_deployment) == 0) {
      results[[deployment_id]] <- NULL  # Explicitly assign NULL to represent no data
    } else {
      AsyEst <- AsyEsts %>%
        filter(Assemblage == deployment_id, Diversity == "Species richness") %>%
        pull(Estimator)
      
      result <- Check_asymptote(observations_for_deployment, AsyEst, species_asymptote_threshold, start_date, end_date)
      results[[deployment_id]] <- result
    }
  }
  
  # Filter out NULL entries and calculate the ratio
  valid_results <- results[!sapply(results, is.null)]  # Remove NULL entries
  n_deployments <- length(valid_results)  # Count valid deployments
  reached_asymptote_ratio <- mean(sapply(valid_results, `[[`, "Reached"), na.rm = TRUE)  # Calculate mean of 'Reached' values
  
  results_daterange <- data.frame(
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
  INTERRESULT_all_for_all <<- do.call(rbind, lapply(seq(min_window, max_window, by = step_size), function(window_size) {
    df <- results[[as.character(window_size)]]
    data.frame(Window_Size = window_size,
               N_windows = unique(df$N_windows),
               Mean_Asymptote_Ratio = mean(df$Reached_Asymptote_Ratio, na.rm = TRUE),
               Min_Asymptote_Ratio = min(df$Reached_Asymptote_Ratio, na.rm = TRUE),
               Max_Asymptote_Ratio = max(df$Reached_Asymptote_Ratio, na.rm = TRUE),
               Ratio_Exceeded_Threshold = mean(df$Exceeded_threshold, na.rm = TRUE)
    )
  }))

  return(INTERRESULT_all_for_all)
}

check_deployment_asymptote <- function(deployments, observations, species_asymptote_threshold) {
  species_richness_estimates <- Calculate_iNEXT_results %>%
    filter(Diversity == "Species richness") %>%
    select(Assemblage, Estimator)
  
  observed_species_counts <- observations %>%
    group_by(deploymentID) %>%
    summarise(Observed_Species = n_distinct(scientificName), .groups = 'drop')
  
  deployment_checks <- left_join(species_richness_estimates, observed_species_counts, by = c("Assemblage" = "deploymentID"))
  
  deployment_checks <- deployment_checks %>%
    mutate(Target = floor(Estimator * species_asymptote_threshold),
           Reached_Asymptote = Observed_Species >= Target)
  
  return(deployment_checks)
}

compute_advanced_metrics <- function(df) {
  max_window_size <- max(df$Window_Size)  # Get the maximum window size for normalization
  
  df %>%
    mutate(
      Inverse = ( Ratio_Exceeded_Threshold  / Window_Size),
      Mean_inverse = ( Mean_Asymptote_Ratio  / Window_Size),
    )
}

calculate_ratios <- function(observations_data) {
  deployment_ids <- unique(observations_data$deploymentID)
  
  results <- data.frame(
    deploymentID = character(length(deployment_ids)),
    total_species = integer(length(deployment_ids)),
    common_species = integer(length(deployment_ids)),
    rare_species = integer(length(deployment_ids)),
    common_ratio = numeric(length(deployment_ids)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(deployment_ids)) {
    deployment_id <- deployment_ids[i]
    data_filtered <- observations_data %>%
      filter(deploymentID == deployment_id) %>%
      group_by(scientificName) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count))
    
    fit <- fitdist(data_filtered$count, "lnorm")
    meanlog <- fit$estimate["meanlog"]
    sdlog <- fit$estimate["sdlog"]
    threshold <- exp(meanlog + .8 * sdlog)
    
    data_filtered <- data_filtered %>%
      mutate(classification = ifelse(count > threshold, "Common", "Rare"))
    
    common_count <- sum(data_filtered$classification == "Common")
    rare_count <- sum(data_filtered$classification == "Rare")
    total_count <- nrow(data_filtered)
    
    results[i, ] <- c(
      deployment_id,
      total_count,
      common_count,
      rare_count,
      common_count / total_count
    )
  }
  results$common_ratio <- as.numeric(results$common_ratio)
  ratio_results <<- results
  mean_ratio <<- mean(ratio_results$common_ratio)
  # mean_ratio <<- mean(results$common_ratio)
  return(results)
}

whole_pipeline <- function(observations, deployments, min_window, max_window, step_size, species_asymptote_threshold, reached_asymptote_ratio_threshold) {
  results_list <- vector("list", 1)
  
  
  # Simulate data
  processed_data <<- Preprocess(observations, deployments)
  
  # Calculate iNEXT estimates
  print('Calculating iNEXT ...')
  # iNEXT_results <- Calculate_iNEXT(processed_data$observations, processed_data$deployments)
  iNEXT_results <- INTERRESULT_iNEXT
  
  # Run the all windows analysis
  print('Running analysis ...')
  
  all_windows_for_all_results <- all_windows_for_all(processed_data$observations, processed_data$deployments, iNEXT_results, min_window, max_window, step_size, species_asymptote_threshold, reached_asymptote_ratio_threshold)
  
  # Compute advanced metrics
  metric_scores <<- compute_advanced_metrics(all_windows_for_all_results)
  
  
  top_mean_inverse <- metric_scores %>%
    arrange(desc(Mean_inverse)) %>%
    slice(1) %>%
    select(Window_Size, Mean_Asymptote_Ratio) %>%
    rename(Window_Size_Metric1 = Window_Size)
  
  # Extract the best result based on Simple Inverse
  top_inverse <- metric_scores %>%
    arrange(desc(Inverse)) %>%
    slice(1) %>%
    select(Window_Size, Ratio_Exceeded_Threshold) %>%
    rename(Window_Size_Metric2 = Window_Size)
  # Combine results
  result <- data.frame(
    # num_deployments = n_distinct(deployments$deploymentID),
    # num_species = n_distinct(observations$scientificName),
    # num_observations_per_deployment = nrow(observations) / n_distinct(deployments$deploymentID),
    species_asymptote_threshold = species_asymptote_threshold,
    reached_asymptote_ratio_threshold = reached_asymptote_ratio_threshold,
    top_mean_inverse,
    top_inverse
  )
  
  return(result)
}


deployments_artis <- read.csv("../ArtisData/deployments.csv")
observations_artis <- read.csv("../ArtisData/observations.csv")
results_pipeline <- whole_pipeline(observations_winter,
                                   deployments_artis,
                                   min_window = 7,
                                   max_window = 100,
                                   step_size = 7,
                                   species_asymptote_threshold = 0.9,
                                   reached_asymptote_ratio_threshold = .9)

print(results_pipeline)
