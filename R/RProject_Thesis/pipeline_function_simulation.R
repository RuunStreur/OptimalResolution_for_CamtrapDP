library(tidyverse)
library(lubridate)
library(iNEXT)
library(dplyr)
library(purrr)
library(foreach)
library(doParallel)

# Set up the parallel backend
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

simulate_multiple_deployments <- function(num_deployments, deployment_duration, num_observations, target_window=70, sdlog, num_species=30) {
  all_observations <- tibble()
  deployments_data <- tibble()
  
  rarity_scores_log_normal <- rlnorm(num_species, meanlog = 0, sdlog = sdlog)
  rarity_scores_log_normal <- rarity_scores_log_normal / sum(rarity_scores_log_normal)
  species_names <- paste0("Species_", seq_len(num_species))
  names(rarity_scores_log_normal) <- species_names
  
  for (dep in 1:num_deployments) {
    start_date <- today()
    end_date <- start_date + days(deployment_duration)
    dates <- seq.Date(start_date, end_date, by = "day")
    avg_daily_observations <- round(num_observations / length(dates))
    
    deployment_id <- paste("Deployment", dep, sep = "_")
    
    for (i in seq_along(dates)) {
      daily_obs_count <- sample(round(avg_daily_observations * 0.3):round(avg_daily_observations * 1.7), 1)
      observed_species <- sample(species_names, size = daily_obs_count, replace = TRUE, prob = rarity_scores_log_normal)
      
      daily_data <- tibble(
        observationID = paste("obs", deployment_id, dates[i], seq_len(daily_obs_count), sep = "_"),
        deploymentID = rep(deployment_id, daily_obs_count),
        date = rep(dates[i], daily_obs_count),
        eventStart = rep(dates[i], daily_obs_count),
        scientificName = observed_species,
        count = rep(1, daily_obs_count)
      )
      
      all_observations <- bind_rows(all_observations, daily_data)
    }
    
    deployments_data <- bind_rows(deployments_data, tibble(
      deploymentID = deployment_id,
      cameraID = paste("Camera", dep, sep = "_"),
      deploymentStart = start_date,
      deploymentEnd = end_date,
      latitude = runif(1, 52.35, 52.37),
      longitude = runif(1, 4.91, 4.92),
      cameraModel = "Simulated_Camera_Model",
      n_species = n_distinct(all_observations$scientificName[all_observations$deploymentID == deployment_id])
    ))
  }
  
  return(list(observations = all_observations, deployments = deployments_data, rarity_scores = rarity_scores_log_normal))
}

simulate_multiple_deployments_target <- function(num_deployments, deployment_duration, num_observations, target_window, sdlog, num_species) {
  all_observations <- tibble()
  deployments_data <- tibble()
  
  rarity_scores_log_normal <- rlnorm(num_species, meanlog = 0, sdlog = sdlog)
  rarity_scores_log_normal <- rarity_scores_log_normal / sum(rarity_scores_log_normal)
  species_names <- paste0("Species_", seq_len(num_species))
  names(rarity_scores_log_normal) <- species_names
  
  for (dep in 1:num_deployments) {
    start_date <- today()
    end_date <- start_date + days(deployment_duration)
    dates <- seq.Date(start_date, end_date, by = "day")
    avg_daily_observations <- round(num_observations / length(dates))
    
    deployment_id <- paste("Deployment", dep, sep = "_")
    species_spotted <- vector("list", length = ceiling(deployment_duration / target_window))
    base_rarity_scores <- rarity_scores_log_normal  
    
    for (i in seq_along(dates)) {
      day_in_window <- i %% target_window
      window_index <- (i %/% target_window) + 1
      
      # Adjust rarity scores if we are 7 days before reaching a new window
      if (day_in_window >= target_window - 7 && day_in_window < target_window) {
        not_seen <- setdiff(species_names, unlist(species_spotted[window_index]))
        rarity_scores_log_normal[not_seen] <- rarity_scores_log_normal[not_seen] * 20
        rarity_scores_log_normal <- rarity_scores_log_normal / sum(rarity_scores_log_normal)
      }
      
      # Reset rarity scores to original distribution right after reaching new window
      if (day_in_window == 1) {
        rarity_scores_log_normal <- base_rarity_scores
        species_spotted[[window_index]] <- character()
      }
      
      daily_obs_count <- sample(round(avg_daily_observations * 0.3):round(avg_daily_observations * 1.7), 1)
      observed_species <- sample(species_names, size = daily_obs_count, replace = TRUE, prob = rarity_scores_log_normal)
      species_spotted[[window_index]] <- c(species_spotted[[window_index]], observed_species)
      
      daily_data <- tibble(
        observationID = paste("obs", deployment_id, dates[i], seq_len(daily_obs_count), sep = "_"),
        deploymentID = rep(deployment_id, daily_obs_count),
        date = rep(dates[i], daily_obs_count),
        eventStart = rep(dates[i], daily_obs_count),
        scientificName = observed_species,
        count = rep(1, daily_obs_count)
      )
      
      all_observations <- bind_rows(all_observations, daily_data)
    }
    
    deployments_data <- bind_rows(deployments_data, tibble(
      deploymentID = deployment_id,
      cameraID = paste("Camera", dep, sep = "_"),
      deploymentStart = start_date,
      deploymentEnd = end_date,
      latitude = runif(1, 52.35, 52.37),
      longitude = runif(1, 4.91, 4.92),
      cameraModel = "Simulated_Camera_Model",
      n_species = n_distinct(all_observations$scientificName[all_observations$deploymentID == deployment_id])
    ))
  }
  
  return(list(observations = all_observations, deployments = deployments_data, rarity_scores = rarity_scores_log_normal))
}

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
    Start_date = start_date,
    End_date = end_date,
    N_Days = as.integer(end_date - start_date + 1),
    AsyEst = AsyEst,
    Target = floor(AsyEst * species_asymptote_threshold),
    Observed = observed_species,
    Reached = observed_species >= floor(AsyEst * species_asymptote_threshold),
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

compute_metrics <- function(df) {
  max_window_size <- max(df$Window_Size)  # Get the maximum window size for normalization
  
  df %>%
    mutate(
      Inverse = ( Ratio_Exceeded_Threshold  / Window_Size),
      Mean_inverse = ( Mean_Asymptote_Ratio  / Window_Size),
    )
}


clusterExport(cl, c("simulate_multiple_deployments_target", 
                    "simulate_multiple_deployments", 
                    "Preprocess", 
                    "Calculate_iNEXT", 
                    "Check_asymptote", 
                    "daterange_asymptote_ratio_calculator", 
                    "all_windows_for_windowsize_calculator", 
                    "all_windows_for_all", 
                    "check_deployment_asymptote", 
                    "compute_metrics"))

whole_pipeline_simulate <- function(n_runs, num_species, num_deployments, num_observations_per_deployment, sdlog, target_window_size, min_window, max_window, step_size, species_asymptote_threshold, reached_asymptote_ratio_threshold) {
  results_list <- foreach(run = 1:n_runs, .combine = rbind, .packages = c("dplyr", "iNEXT", "lubridate", "purrr", "tidyverse")) %dopar% {
    # Simulate data
    print(paste('Simulating data for run', run, '...'))
    simulation_results <- simulate_multiple_deployments_target(num_deployments, 365, num_observations_per_deployment, target_window_size, sdlog = sdlog, num_species = num_species)
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
    all_windows_for_all_results <- all_windows_for_all(observations, deployments, iNEXT_results, min_window, max_window, step_size, species_asymptote_threshold, reached_asymptote_ratio_threshold)
    
    # Compute advanced metrics
    metric_scores <- compute_metrics(all_windows_for_all_results)
    
    # Extract the best result based on Simple Inverse
    top_inverse <- metric_scores %>%
      arrange(desc(Inverse)) %>%
      slice(1) %>%
      select(Window_Size, Ratio_Exceeded_Threshold) %>%
      rename(Window_Size_inverse = Window_Size)
    
    top_mean_inverse <- metric_scores %>%
      arrange(desc(Mean_inverse)) %>%
      slice(1) %>%
      select(Window_Size, Mean_Asymptote_Ratio) %>%
      rename(Window_Size_mean = Window_Size)
    
    # Combine results
    data.frame(
      Run = run,
      num_deployments = num_deployments,
      num_species = num_species,
      num_observations_per_deployment = num_observations_per_deployment,
      top_mean_inverse,
      top_inverse
    )
  }
  
  return(results_list)
}

# Run the pipeline simulation
results_pipeline_simulate <- whole_pipeline_simulate(n_runs = 100,
                                                     num_deployments = 50,
                                                     num_species = 30,
                                                     num_observations_per_deployment = 5000,
                                                     sdlog = 2.3,
                                                     target_window_size = 70,
                                                     min_window = 7, max_window = 150,
                                                     step_size = 7,
                                                     species_asymptote_threshold = 0.95,
                                                     reached_asymptote_ratio_threshold = 1)

# Print results
print(results_pipeline_simulate)

# Save results
write_csv(results_pipeline_simulate, file.path('C:/Users/ruuns/Documents/GitHub/Thesis2024/R/RProject_Thesis/', 
                                               "RESULTS_5kObs_30Spec_parallel_50deps.csv"))

# Stop the parallel cluster
stopCluster(cl)

# 
# 
# simulate_multiple_deployments <- function(num_deployments, deployment_duration, num_observations, target_window =60 ,sdlog, num_species = 30) {
#   all_observations <- tibble()
#   deployments_data <- tibble()
#   
#   rarity_scores_log_normal <- rlnorm(num_species, meanlog = 0, sdlog = sdlog)
#   rarity_scores_log_normal <- rarity_scores_log_normal / sum(rarity_scores_log_normal)
#   species_names <- paste0("Species_", seq_len(num_species))
#   names(rarity_scores_log_normal) <- species_names
#   
#   for (dep in 1:num_deployments) {
#     start_date <- today()
#     end_date <- start_date + days(deployment_duration)
#     dates <- seq.Date(start_date, end_date, by = "day")
#     avg_daily_observations <- round(num_observations / length(dates))
#     
#     deployment_id <- paste("Deployment", dep, sep = "_")
#     
#     for (i in seq_along(dates)) {
#       daily_obs_count <- sample(round(avg_daily_observations * 0.3):round(avg_daily_observations * 1.7), 1)
#       observed_species <- sample(species_names, size = daily_obs_count, replace = TRUE, prob = rarity_scores_log_normal)
#       
#       daily_data <- tibble(
#         observationID = paste("obs", deployment_id, dates[i], seq_len(daily_obs_count), sep = "_"),
#         deploymentID = rep(deployment_id, daily_obs_count),
#         date = rep(dates[i], daily_obs_count),
#         eventStart = rep(dates[i], daily_obs_count),
#         scientificName = observed_species,
#         count = rep(1, daily_obs_count)
#       )
#       
#       all_observations <- bind_rows(all_observations, daily_data)
#     }
#     
#     deployments_data <- bind_rows(deployments_data, tibble(
#       deploymentID = deployment_id,
#       cameraID = paste("Camera", dep, sep = "_"),
#       deploymentStart = start_date,
#       deploymentEnd = end_date,
#       latitude = runif(1, 52.35, 52.37),
#       longitude = runif(1, 4.91, 4.92),
#       cameraModel = "Simulated_Camera_Model",
#       n_species = n_distinct(all_observations$scientificName[all_observations$deploymentID == deployment_id])
#     ))
#   }
#   
#   return(list(observations = all_observations, deployments = deployments_data, rarity_scores = rarity_scores_log_normal))
# }
# 




# 
# simulate_multiple_deployments_target <- function(num_deployments, deployment_duration, num_observations, target_window, sdlog, num_species = 30) {
#   all_observations <- tibble()
#   deployments_data <- tibble()
#   
#   rarity_scores_log_normal <- rlnorm(num_species, meanlog = 0, sdlog = sdlog)
#   rarity_scores_log_normal <- rarity_scores_log_normal / sum(rarity_scores_log_normal)
#   species_names <- paste0("Species_", seq_len(num_species))
#   names(rarity_scores_log_normal) <- species_names
#   
#   for (dep in 1:num_deployments) {
#     start_date <- today()
#     end_date <- start_date + days(deployment_duration)
#     dates <- seq.Date(start_date, end_date, by = "day")
#     avg_daily_observations <- round(num_observations / length(dates))
#     
#     deployment_id <- paste("Deployment", dep, sep = "_")
#     species_spotted <- vector("list", length = ceiling(deployment_duration / target_window))
#     base_rarity_scores <- rarity_scores_log_normal  
#     
#     for (i in seq_along(dates)) {
#       day_in_window <- i %% target_window
#       window_index <- (i %/% target_window) + 1
#       
#       # Adjust rarity scores if we are 7 days before reaching a new window
#       if (day_in_window >= target_window - 7 && day_in_window < target_window) {
#         not_seen <- setdiff(species_names, unlist(species_spotted[window_index]))
#         rarity_scores_log_normal[not_seen] <- rarity_scores_log_normal[not_seen] * 20
#         rarity_scores_log_normal <- rarity_scores_log_normal / sum(rarity_scores_log_normal)
#       }
#       
#       # Reset rarity scores to original distribution right after reaching new window
#       if (day_in_window == 1) {
#         rarity_scores_log_normal <- base_rarity_scores
#         species_spotted[[window_index]] <- character()
#       }
#       
#       daily_obs_count <- sample(round(avg_daily_observations * 0.3):round(avg_daily_observations * 1.7), 1)
#       observed_species <- sample(species_names, size = daily_obs_count, replace = TRUE, prob = rarity_scores_log_normal)
#       species_spotted[[window_index]] <- c(species_spotted[[window_index]], observed_species)
#       
#       daily_data <- tibble(
#         observationID = paste("obs", deployment_id, dates[i], seq_len(daily_obs_count), sep = "_"),
#         deploymentID = rep(deployment_id, daily_obs_count),
#         date = rep(dates[i], daily_obs_count),
#         eventStart = rep(dates[i], daily_obs_count),
#         scientificName = observed_species,
#         count = rep(1, daily_obs_count)
#       )
#       
#       all_observations <- bind_rows(all_observations, daily_data)
#     }
#     
#     deployments_data <- bind_rows(deployments_data, tibble(
#       deploymentID = deployment_id,
#       cameraID = paste("Camera", dep, sep = "_"),
#       deploymentStart = start_date,
#       deploymentEnd = end_date,
#       latitude = runif(1, 52.35, 52.37),
#       longitude = runif(1, 4.91, 4.92),
#       cameraModel = "Simulated_Camera_Model",
#       n_species = n_distinct(all_observations$scientificName[all_observations$deploymentID == deployment_id])
#     ))
#   }
#   
#   return(list(observations = all_observations, deployments = deployments_data, rarity_scores = rarity_scores_log_normal))
# }



# results_pipeline_simulate <- whole_pipeline_simulate(n_runs = 1,
#                                   num_deployments = 4,
#                                   num_species = 30,
#                                   num_observations_per_deployment = 20000,
#                                   sdlog = 2.3,
#                                   target_window_size = 84,
#                                   min_window = 30, max_window = 300,
#                                   step_size = 30,
#                                   species_asymptote_threshold = 0.95,
#                                   reached_asymptote_ratio_threshold = 1)

# whole_pipeline_simulate <- function(n_runs, num_species, num_deployments, num_observations_per_deployment, sdlog, target_window_size, min_window, max_window, step_size, species_asymptote_threshold, reached_asymptote_ratio_threshold) {
#   results_list <- vector("list", n_runs)
#   
#   for (run in 1:n_runs) {
#     
#     # Simulate data
#     print(paste('Simulating data for run',run,'...'))
#     simulation_results <<- simulate_multiple_deployments(num_deployments, 365, num_observations_per_deployment, target_window_size, sdlog = sdlog)
#     observations <- simulation_results$observations
#     deployments <- simulation_results$deployments
#     
#     # Process data
#     processed_data <<- Preprocess(observations, deployments)
#     observations <<- processed_data$observations
#     deployments <<- processed_data$deployments
#     
#     # Calculate iNEXT estimates
#     print(paste('Calculating iNEXT for run', run, '...'))
#     iNEXT_results <- Calculate_iNEXT(observations, deployments)
#     
#     # Run the all windows analysis
#     print(paste('Running analysis for run', run, '...'))
#     
#     all_windows_for_all_results <- all_windows_for_all(observations, deployments, iNEXT_results, min_window, max_window, step_size, species_asymptote_threshold, reached_asymptote_ratio_threshold)
#     
#     # Compute advanced metrics
#     optimized_values <<- compute_advanced_metrics(all_windows_for_all_results)
#     
#     # Extract the best result based on Simple Inverse
#     top_inverse <- optimized_values %>%
#       arrange(desc(Mean_inverse)) %>%
#       slice(1) %>%
#       select(Window_Size, Ratio_Exceeded_Threshold)
#     
#     # Store results in a list
#     results_list[[run]] <- top_inverse
#   }
#   
#   # Combine all results into a single dataframe
#   final_output_simulate <- bind_rows(results_list, .id = "Run")
#   return(final_output_simulate)
# }