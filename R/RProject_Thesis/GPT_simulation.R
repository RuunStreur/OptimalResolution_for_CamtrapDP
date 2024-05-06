library(tidyverse)

# Parameters
num_observations <- 20000
num_species <- 50
deployment_duration <- 365*3

set.seed(42)
# using a beta distribution
set.seed(42)
alpha <- 0.3  # Lower alpha to make common species more common
beta <- 8    # Increase beta to make rare species rarer
rarity_scores <- rbeta(num_species, alpha, beta)
species_names <- paste0("Species_", seq_len(num_species))
names(rarity_scores) <- species_names

# Function to simulate camera trap data
simulate_camera_trap_data <- function(num_observations, rarity_scores, deployment_duration) {
  start_date <- Sys.Date() # Start from today
  end_date <- start_date + deployment_duration
  dates <- sample(seq(start_date, end_date, by = "day"), num_observations, replace = TRUE)
  
  observation_likelihood <- rarity_scores / sum(rarity_scores)
  
  simulated_data <- tibble(
    observationID = paste("obs_", format(Sys.time(), "%Y%m%d%H%M%S"), "Simulated_Camera_1", seq_len(num_observations), sep = "_"),
    deploymentID = rep("Simulated_Camera_1", num_observations),
    mediaID = rep("", num_observations),
    eventID = rep("", num_observations),
    eventStart = rep("", num_observations),
    eventEnd = rep("", num_observations),
    observationType = rep("NA", num_observations),
    scientificName = sample(species_names, num_observations, replace = TRUE, prob = observation_likelihood),
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

# Generate simulated camera trap data
simulated_data <- simulate_camera_trap_data(num_observations, rarity_scores, deployment_duration)

# View the first few rows of the simulated data
head(simulated_data)
