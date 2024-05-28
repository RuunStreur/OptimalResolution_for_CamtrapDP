library(tidyverse)
library(lubridate)
library(iNEXT)
library(dplyr)
library(purrr)
library(fitdistrplus)

Preprocess <- function(observations, deployments) {
  
  # Convert date columns to Date type
  deployments$deploymentStart <- as.Date(deployments$deploymentStart)
  deployments$deploymentEnd <- as.Date(deployments$deploymentEnd)
  observations$date <- as.Date(observations$eventStart)
  
  # Calculate the active time of deployments in days
  deployments$time_active <- as.numeric(deployments$deploymentEnd - deployments$deploymentStart)
  
  # Filter out observations with missing or empty scientific names and dates
  observations <- observations %>%
    filter(scientificName != "" & !is.na(scientificName) & !is.na(date))
  
  # Set the minimum required observations per deployment
  min_observations <- 1
  
  # Count observations per deployment
  obs_count <- observations %>%
    group_by(deploymentID) %>%
    summarise(obs_count = n(), .groups = 'drop')
  
  # Filter deployments with at least the minimum required observations
  deployments <- deployments %>%
    inner_join(obs_count, by = "deploymentID") %>%
    filter(obs_count > min_observations)
  
  # Further filter deployments with active time of at least 1 day
  deployments <- deployments[deployments$time_active >= 1, ]
  
  # Filter observations to include only those from the remaining deployments
  observations <- observations[observations$deploymentID %in% deployments$deploymentID, ]
  
  return(list(observations = observations, deployments = deployments))
}

Calculate_iNEXT <- function(observations, deployments) {
  # Initialize a list to hold incidence matrices for each deployment
  deployments_inc_mats <- list()
  
  # Iterate over each unique deployment
  for (deployment in unique(deployments$deploymentID)) {
    # Extract deployment start and end dates
    deploy_start <- deployments$deploymentStart[deployments$deploymentID == deployment]
    deploy_end <- deployments$deploymentEnd[deployments$deploymentID == deployment]
    
    # Filter observations for the current deployment
    deploy_obs <- observations[observations$deploymentID == deployment,]
    deploy_spec <- unique(deploy_obs$scientificName)
    
    # Generate a sequence of days for the deployment period
    deploy_days <- seq.Date(deploy_start, deploy_end, by = "day")
    
    # Initialize an incidence matrix for the deployment
    inc_mat <- matrix(0, nrow = length(deploy_spec), ncol = length(deploy_days))
    row.names(inc_mat) <- deploy_spec
    colnames(inc_mat) <- as.character(deploy_days)
    
    # Populate the incidence matrix
    for (i in seq_along(deploy_days)) {
      curr_day <- deploy_days[i]
      curr_spec <- deploy_obs$scientificName[deploy_obs$date == curr_day]
      present_bool <- deploy_spec %in% curr_spec
      inc_mat[, i] <- as.numeric(present_bool)
    }
    
    # Store the incidence matrix in the list
    deployments_inc_mats[[deployment]] <- inc_mat
  }
  
  # Perform iNEXT analysis
  iNET_out <- iNEXT(deployments_inc_mats,
                    q = c(0),
                    datatype = "incidence_raw",
                    endpoint = 750)
  
  # Convert the result to a data frame and store it in a global variable
  INTERRESULT_iNEXT <<- as.data.frame(iNET_out$AsyEst)
  
  return(INTERRESULT_iNEXT)
}

Check_asymptote <- function(split_data, AsyEst, species_asymptote_threshold, start_date, end_date, common_ratio = NULL, use_common_ratio = FALSE) {
  # Calculate the number of observed species
  observed_species <- length(unique(split_data$scientificName))
  
  # Extract the unique deployment ID
  deployment_id <- unique(split_data$deploymentID)
  
  # Calculate the target using common_ratio if use_common_ratio is TRUE
  target <- if (use_common_ratio && !is.null(common_ratio)) {
    max(floor(common_ratio * AsyEst), 1)
  } else {
    max(floor(AsyEst * species_asymptote_threshold), 1)
  }
  
  # Create a data frame to store the result
  result_df <- data.frame(
    DeploymentID = deployment_id,
    Start_date = start_date,
    End_date = end_date,
    N_Days = as.integer(end_date - start_date + 1),
    AsyEst = AsyEst,
    Target = target,
    Observed = observed_species,
    Reached = observed_species >= target,
    stringsAsFactors = FALSE
  )
  
  # Append the result to the global result data frame
  INTERRESULT_check_asymptote <<- rbind(INTERRESULT_check_asymptote, result_df)
  
  return(result_df)
}

daterange_asymptote_ratio_calculator <- function(observations, deployments, AsyEsts, daterange, species_asymptote_threshold, common_ratios = NULL, use_common_ratio = FALSE) {
  start_date <- as.Date(daterange$start)
  end_date <- as.Date(daterange$end)
  
  results <- list()
  
  for (deployment_id in unique(deployments$deploymentID)) {
    # Filter observations for the current deployment within the specified date range
    observations_for_deployment <- observations %>%
      filter(deploymentID == deployment_id, date >= start_date, date <= end_date)
    
    if (nrow(observations_for_deployment) == 0) {
      results[[deployment_id]] <- NULL  # No data for this deployment
    } else {
      # Retrieve Asymptotic Estimator for the current deployment
      AsyEst <- AsyEsts %>%
        filter(Assemblage == deployment_id, Diversity == "Species richness") %>%
        pull(Estimator)
      
      # Retrieve common_ratio for the current deployment if use_common_ratio is TRUE
      common_ratio <- if (use_common_ratio && !is.null(common_ratios)) {
        common_ratios$common_ratio[common_ratios$deploymentID == deployment_id]
      } else {
        NULL
      }
      
      # Check asymptote and store the result
      result <- Check_asymptote(observations_for_deployment, AsyEst, species_asymptote_threshold, start_date, end_date, common_ratio, use_common_ratio)
      results[[deployment_id]] <- result
    }
  }
  
  # Filter out NULL entries and calculate the ratio
  valid_results <- results[!sapply(results, is.null)]
  n_deployments <- length(valid_results)
  reached_asymptote_ratio <- mean(sapply(valid_results, `[[`, "Reached"), na.rm = TRUE)
  
  # Create the result data frame
  results_daterange <- data.frame(
    Daterange = paste(format(start_date, "%d/%m/%y"), "-", format(end_date, "%d/%m/%y")),
    N_Days = as.integer(end_date - start_date + 1),
    N_Deployments = n_deployments,
    Reached_Asymptote_Ratio = reached_asymptote_ratio
  )
  
  return(results_daterange)
}

all_windows_for_windowsize_calculator <- function(observations, deployments, AsyEsts, window_size, species_asymptote_threshold, reached_asymptote_ratio_threshold, common_ratios = NULL, use_common_ratio = FALSE) {
  # Calculate step size and date range
  step <- window_size
  first_observation_date <- min(observations$date)
  last_observation_date <- max(observations$date)
  
  # Adjust step size if it exceeds the date range
  if (first_observation_date + step > last_observation_date) {
    warning("The step size is too large for the date range. Adjusting step size to fit the range.")
    step <- as.integer(difftime(last_observation_date, first_observation_date, units = "days"))
  }
  
  # Generate start and end dates for the windows
  start_dates <- seq(first_observation_date, last_observation_date - step, by = step)
  end_dates <- start_dates + step - 1
  n_windows <- length(start_dates)
  
  # Calculate asymptote ratio for each date range window
  results <- mapply(function(start, end) {
    daterange <- list(start = start, end = end)
    daterange_asymptote_ratio_calculator(observations, deployments, AsyEsts, daterange, species_asymptote_threshold, common_ratios, use_common_ratio)
  }, start_dates, end_dates, SIMPLIFY = FALSE)
  
  # Combine results into a single data frame
  results_df <- do.call(rbind, results)
  
  # Add additional columns to the results data frame
  results_df$Exceeded_threshold <- results_df$Reached_Asymptote_Ratio >= reached_asymptote_ratio_threshold
  results_df$N_windows <- n_windows
  
  # Append results to the global results data frame
  INTERRESULT_all_windows_for_windowsize_calculator <<- rbind(INTERRESULT_all_windows_for_windowsize_calculator, results_df)
  
  return(results_df)
}

all_windows_for_all <- function(observations, deployments, AsyEsts, min_window, max_window, step_size, species_asymptote_threshold, reached_asymptote_ratio_threshold, common_ratios = NULL, use_common_ratio = FALSE) {
  # Initialize global data frames for storing intermediate results
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
  
  INTERRESULT_all_windows_for_windowsize_calculator <<- data.frame(
    Daterange = character(),
    N_Days = integer(),
    N_Deployments = integer(),
    Reached_Asymptote_Ratio = numeric(),
    Exceeded_threshold = logical(),
    N_windows = integer(),
    stringsAsFactors = FALSE
  )
  
  results <- list()
  
  # Iterate over each window size from min_window to max_window with the given step size
  for (window_size in seq(min_window, max_window, by = step_size)) {
    # Skip window sizes larger than the overall date range
    if (window_size >= as.integer(difftime(max(observations$date), min(observations$date), units = "days"))) {
      next
    }
    # Calculate results for the current window size
    window_results <- all_windows_for_windowsize_calculator(observations, deployments, AsyEsts, window_size, species_asymptote_threshold, reached_asymptote_ratio_threshold, common_ratios, use_common_ratio)
    results[[as.character(window_size)]] <- window_results
  }
  
  # Combine results for all window sizes into a single data frame
  INTERRESULT_all_for_all <<- do.call(rbind, lapply(seq(min_window, max_window, by = step_size), function(window_size) {
    df <- results[[as.character(window_size)]]
    if (is.null(df)) return(NULL)
    data.frame(
      Window_Size = window_size,
      N_windows = unique(df$N_windows),
      Mean_Asymptote_Ratio = mean(df$Reached_Asymptote_Ratio, na.rm = TRUE), 
      Min_Asymptote_Ratio = min(df$Reached_Asymptote_Ratio, na.rm = TRUE), 
      Max_Asymptote_Ratio = max(df$Reached_Asymptote_Ratio, na.rm = TRUE), 
      Ratio_Exceeded_Threshold = mean(df$Exceeded_threshold, na.rm = TRUE)
    )
  }))
  
  return(INTERRESULT_all_for_all)
}

compute_metrics <- function(df) {
  # Find the maximum window size
  max_window_size <- max(df$Window_Size, na.rm = TRUE)
  
  # Compute metrics and return the updated data frame
  df %>%
    mutate(
      Inverse = Ratio_Exceeded_Threshold / Window_Size,
      Mean_inverse = Mean_Asymptote_Ratio / Window_Size
    )
}

calculate_ratios <- function(observations_data) {
  # Get unique deployment IDs
  deployment_ids <- unique(observations_data$deploymentID)
  
  # Initialize a data frame to store results
  results <- data.frame(
    deploymentID = character(length(deployment_ids)),
    total_species = integer(length(deployment_ids)),
    common_species = integer(length(deployment_ids)),
    rare_species = integer(length(deployment_ids)),
    common_ratio = numeric(length(deployment_ids)),
    stringsAsFactors = FALSE
  )
  
  # Iterate over each deployment ID
  for (i in seq_along(deployment_ids)) {
    deployment_id <- deployment_ids[i]
    
    # Filter data for the current deployment and summarize counts by species
    data_filtered <- observations_data %>%
      filter(deploymentID == deployment_id) %>%
      group_by(scientificName) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count))
    
    # Fit a log-normal distribution to the species count data
    fit <- fitdist(data_filtered$count, "lnorm")
    meanlog <- fit$estimate["meanlog"]
    sdlog <- fit$estimate["sdlog"]
    threshold <- exp(meanlog + 0.8 * sdlog)  # Calculate the threshold
    
    # Classify species as common or rare based on the threshold
    data_filtered <- data_filtered %>%
      mutate(classification = ifelse(count > threshold, "Common", "Rare"))
    
    # Count the number of common and rare species
    common_count <- sum(data_filtered$classification == "Common")
    rare_count <- sum(data_filtered$classification == "Rare")
    total_count <- nrow(data_filtered)
    
    # Store the results for the current deployment
    results[i, ] <- c(
      deployment_id,
      total_count,
      common_count,
      rare_count,
      common_count / total_count
    )
  }
  
  # Convert common_ratio to numeric type
  results$common_ratio <- as.numeric(results$common_ratio)
  
  # Update global variables with results
  ratio_results <<- results
  mean_ratio <<- mean(ratio_results$common_ratio, na.rm = TRUE)
  
  return(results)
}

whole_pipeline <- function(observations, deployments, min_window, max_window, step_size, species_asymptote_threshold, reached_asymptote_ratio_threshold, use_common_ratio = FALSE) {
  # Initialize a list to store results
  results_list <- vector("list", 1)
  
  # Preprocess the data
  processed_data <<- Preprocess(observations, deployments)
  
  # Calculate iNEXT results
  message('Calculating iNEXT ...')
  iNEXT_results <- Calculate_iNEXT(processed_data$observations, processed_data$deployments)
  # iNEXT_results <- INTERRESULT_iNEXT
  # Calculate ratios if use_common_ratio is TRUE
  if (use_common_ratio) {
    message('Calculating ratios ...')
    common_ratios <- calculate_ratios(processed_data$observations)
  } else {
    common_ratios <- NULL
  }
  
  # Run analysis over all windows
  message('Running analysis ...')
  all_windows_for_all_results <- all_windows_for_all(processed_data$observations, processed_data$deployments, iNEXT_results, min_window, max_window, step_size, species_asymptote_threshold, reached_asymptote_ratio_threshold, common_ratios, use_common_ratio)
  
  # Compute metrics from the results
  INTERRESULT_metric_scores <<- compute_metrics(all_windows_for_all_results)
  
  top_mean_inverse <- INTERRESULT_metric_scores %>%
    arrange(desc(Mean_inverse)) %>%
    slice(1) %>%
    dplyr::select(Window_Size, Mean_Asymptote_Ratio) %>%
    dplyr::rename(Window_Size_Metric1 = Window_Size)
  
  top_inverse <- INTERRESULT_metric_scores %>%
    arrange(desc(Inverse)) %>%
    slice(1) %>%
    dplyr::select(Window_Size, Ratio_Exceeded_Threshold) %>%
    dplyr::rename(Window_Size_Metric2 = Window_Size)
  
  # Compile the final results into a data frame
  result <- data.frame(
    species_asymptote_threshold = species_asymptote_threshold,
    reached_asymptote_ratio_threshold = reached_asymptote_ratio_threshold,
    top_mean_inverse,
    top_inverse
  )
  
  return(result)
}

deployments_artis <- read.csv("../ArtisData/deployments.csv")
observations_artis <- read.csv("../ArtisData/observations.csv")
results_pipeline <- whole_pipeline(observations_artis,
                                   deployments_artis,
                                   min_window = 7,
                                   max_window = 100,
                                   step_size = 7,
                                   species_asymptote_threshold = 0.22,
                                   reached_asymptote_ratio_threshold = .9,
                                   use_common_ratio = FALSE)

print(results_pipeline)
