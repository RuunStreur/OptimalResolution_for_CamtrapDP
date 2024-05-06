library(tidyverse)
library(lubridate)

set.seed(46)

# Simulation parameters
num_species <- 30
deployment_duration <- 365 
num_observations_per_deployment <- 20000
num_deployments <- 5

meanlog <- 0
sdlog <- 2.3

# Generate rarity scores for species
rarity_scores_log_normal <- rlnorm(num_species, meanlog = meanlog, sdlog = sdlog)
rarity_scores_log_normal <- rarity_scores_log_normal / sum(rarity_scores_log_normal)
species_names <- paste0("Species_", seq_len(num_species))
names(rarity_scores_log_normal) <- species_names

# simulate daily incidence data for multiple deployments
simulate_multiple_deployments <- function(num_deployments, rarity_scores, deployment_duration, num_observations) {
  all_observations <- tibble()
  deployments_data <- tibble()
  
  for (dep in 1:num_deployments) {
    start_date <- today()
    end_date <- start_date + days(deployment_duration)
    dates <- seq.Date(start_date, end_date, by = "day")
    avg_daily_observations <- round(num_observations / length(dates))
    
    deployment_id <- paste("Deployment", dep, sep = "_")
    
    for (i in seq_along(dates)) {
      daily_obs_count <- sample(round(avg_daily_observations * 0.3):round(avg_daily_observations * 1.7), 1)
      observed_species <- sample(species_names, size = daily_obs_count, replace = TRUE, prob = rarity_scores)
      
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
  
  return(list(observations = all_observations, deployments = deployments_data))
}

# Running the simulation
simulation_results <- simulate_multiple_deployments(num_deployments, rarity_scores_log_normal, deployment_duration, num_observations_per_deployment)

# Print results
print(simulation_results$deployments)
print(simulation_results$observations)
