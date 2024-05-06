library(dplyr)
library(purrr)
library(tidyverse)
library(iNEXT)

# observations = simulation_results$observations
# deployments = simulation_results$deployments

deployments <- read.csv("../ArtisData/deployments.csv")
observations <- read.csv("../ArtisData/observations.csv")

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
deployment_test <- observations %>% filter(deploymentID == "artis_27_wildlifecamera1")

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

#Calculate_iNEXT(observations, deployments)

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
    Reached = observed_species >= floor(AsyEst * species_asymptote_threshold)
  )
  return(result_df)
}

Deployment_window_calculator <- function(deployment_data, window_size, AsyEst, species_asymptote_threshold) {
  window_size_days <- as.numeric(window_size)
  period_end <- max(deployment_data$date)
  period_start <- min(deployment_data$date)
  total_days <- as.integer(period_end - period_start + 1)
  
  # Determine if window size is larger than the available data
  if (window_size_days >= total_days) {
    # Use the entire range as a single window
    split_starts <- period_start
    split_ends <- period_end
    n_full_windows <- 1
    dropped_days <- 0
  } else {
    # Normal calculation for multiple windows
    n_full_windows <- total_days %/% window_size_days
    dropped_days <- total_days %% window_size_days
    split_starts <- seq(from = period_start, by = window_size_days, length.out = n_full_windows)
    split_ends <- split_starts + window_size_days - 1
  }
  
  results <- list()
  for (i in seq_len(n_full_windows)) {
    split_data <- deployment_data[deployment_data$date >= split_starts[i] & deployment_data$date <= split_ends[i], ]
    
    if (nrow(split_data) > 0) {
      asymp_result <- Check_asymptote(split_data, AsyEst, species_asymptote_threshold)
      asymp_result$N_Days <- as.integer(split_ends[i] - split_starts[i] + 1)
      results[[i]] <- asymp_result
    } else {
      # Append a result with 0 observed when no data is available
      results[[i]] <- data.frame(
        DeploymentID = unique(deployment_data$deploymentID),
        Start_date = split_starts[i],
        End_date = split_ends[i],
        N_Days = as.integer(split_ends[i] - split_starts[i] + 1),
        AsyEst = AsyEst,
        Target = NA,
        Observed = 0,
        Reached = FALSE
      )
    }
    check_asymptote_results_all <<- rbind(check_asymptote_results_all, results[[i]])
  }
  
  n_windows <- length(results)
  reached <- sum(unlist(lapply(results, function(x) x$Reached)), na.rm = TRUE)
  
  result_df <- data.frame(
    Deployment = unique(deployment_data$deploymentID),
    Window_Size = window_size_days,
    N_Windows = n_windows,
    Dropped_Days = dropped_days,
    Asymptote_Ratio = if (n_windows > 0) reached / n_windows else 0
  )
  return(result_df)
}

#Deployment_window_calculator(deployment_test, 600, 14, 1)

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

Observations_calculator(observations, deployments, min_window = 30, max_window = 390, step_size = 7, species_asymptote_threshold = .5, deployment_asymptote_threshold = 1)

find_pareto_front <- function(data) {
  pareto_front <- do.call(rbind, lapply(1:nrow(data), function(i) {
    current_point <- data[i,]
    is_dominated <- any(apply(data, 1, function(x) {
      (x['Deployments_Reached_Asymptote_Ratio'] > current_point['Deployments_Reached_Asymptote_Ratio'] & x['Window_Size'] <= current_point['Window_Size']) |
        (x['Deployments_Reached_Asymptote_Ratio'] >= current_point['Deployments_Reached_Asymptote_Ratio'] & x['Window_Size'] < current_point['Window_Size'])
    }))
    if (!is_dominated) return(current_point)
    else return(NULL)
  }))
  pareto_front
}

deployment_asymptote_results <- check_deployment_asymptote(deployments, observations, 0.5)
print(deployment_asymptote_results)

pareto_front <- find_pareto_front(Observations_calculator_results$Summary_Results)
print(pareto_front)


