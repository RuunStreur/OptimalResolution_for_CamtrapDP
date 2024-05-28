library(tidyverse)

# Parameters
num_observations <- 1000
num_species <- 50
deployment_duration <- 365 * 2  # Total duration of deployment in days
deployment_time_all_spotted <- 365  # Days by which all species should be observed

set.seed(43)

# Define parameters for the log-normal distribution
meanlog <- 0
sdlog <- 2

# Generate rarity scores from a log-normal distribution
rarity_scores_log_normal <- rlnorm(num_species, meanlog = meanlog, sdlog = sdlog)
rarity_scores_log_normal <- rarity_scores_log_normal / sum(rarity_scores_log_normal)  # Normalize scores
species_names <- paste0("Species_", seq_len(num_species))
names(rarity_scores_log_normal) <- species_names

# Function to simulate camera trap data
simulate_camera_trap_data <- function(num_observations, rarity_scores, deployment_duration, all_spotted_by) {
  start_date <- Sys.Date()  # Start from today
  end_date <- start_date + deployment_duration
  all_spotted_date <- start_date + all_spotted_by
  
  # Ensure each species is observed at least once by all_spotted_date
  early_dates <- seq(start_date, all_spotted_date, by = "day")
  early_observations <- sample(early_dates, num_species, replace = TRUE)
  
  # Additional observations distributed over the entire deployment duration
  remaining_dates <- sample(seq(start_date, end_date, by = "day"), num_observations - num_species, replace = TRUE)
  dates <- c(early_observations, remaining_dates)
  
  simulated_data <- tibble(
    observationID = paste("obs_", format(Sys.time(), "%Y%m%d%H%M%S"), "Simulated_Camera_1", seq_len(num_observations), sep = "_"),
    deploymentID = rep("Simulated_Camera_1", num_observations),
    mediaID = rep("", num_observations),
    eventID = rep("", num_observations),
    eventStart = rep("", num_observations),
    eventEnd = rep("", num_observations),
    observationType = rep("NA", num_observations),
    scientificName = sample(c(species_names, sample(species_names, num_observations - num_species, replace = TRUE, prob = rarity_scores)), num_observations, replace = FALSE),
    count = rep(1, num_observations),
    lifeStage = rep("NA", num_observations),
    sex = rep("NA", num_observations),
    behavior = rep("NA", num_observations),
    individualID = rep("NA", num_observations),
    bboxX = rep("NA", num_observations),
    bboxY = rep("NA", num_observations),
    bboxWidth = rep("NA", num_observations),
    bboxHeight = rep("NA", num_observations),
    classificationMethod = rep("NA", num_observations),
    classifiedBy = rep("NA", num_observations),
    classificationProbability = rep("NA", num_observations),
    observationComments = rep("", num_observations),
    date = dates
  )
  
  return(simulated_data)
}

# Generate simulated camera trap data using log-normal rarity scores
simulated_data_log_normal <- simulate_camera_trap_data(num_observations, rarity_scores_log_normal, deployment_duration, deployment_time_all_spotted)
