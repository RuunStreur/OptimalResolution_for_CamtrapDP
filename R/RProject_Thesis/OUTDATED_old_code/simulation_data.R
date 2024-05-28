library(tidyverse)

#Parameters
num_observations <- 20000 # Increase for a larger dataset
num_species <- 60 # Adjust number of species
common_ratio <- 0.05 # Decrease proportion of common species
mid_ratio <- 0.15 # Adjust proportion of mid-tier species
rare_ratio <- 0.8 # Increase proportion of rare species
observation_prob <- c(0.70, 0.25, 0.05)
deployment_duration <- 365 * 2

simulate_data <- function(num_observations, num_species, common_ratio, mid_ratio, rare_ratio, deployment_duration) {
  total_species <- num_species
  common_species <- floor(common_ratio * total_species)
  mid_species <- floor(mid_ratio * total_species)
  rare_species <- total_species - common_species - mid_species
  common_species_names <- replicate(common_species, paste(sample(letters, 3, replace = TRUE), collapse = ""))
  mid_species_names <- replicate(mid_species, paste(sample(letters, 10, replace = TRUE), collapse = ""))
  rare_species_names <- replicate(rare_species, paste(sample(letters, 20, replace = TRUE), collapse = ""))
  species_names <- c(common_species_names, mid_species_names, rare_species_names)
  rarity_levels <- c(rep(1, common_species), rep(2, mid_species), rep(3, rare_species))
  rarity_prob <- observation_prob
  start_date <- as.Date(Sys.Date())
  end_date <- start_date + deployment_duration
  dates <- sample(seq(start_date, end_date, by = "day"), num_observations, replace = TRUE)

  simulated_data <- data.frame(
    observationID = paste("obs_", format(Sys.time(), "%Y%m%d%H%M%S"), "Simulated_Camera_1", seq_along(1:num_observations), sep = "_"),
    deploymentID = rep("Simulated_Camera_1", num_observations),
    mediaID = rep("", num_observations),
    eventID = rep("", num_observations),
    eventStart = rep("", num_observations),
    eventEnd = rep("", num_observations),
    observationType = rep("NA", num_observations),
    scientificName = sample(species_names, num_observations, replace = TRUE, prob = rarity_prob[rarity_levels]),
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

simulated_data <- simulate_data(num_observations, num_species, common_ratio, mid_ratio, rare_ratio, deployment_duration)

head(simulated_data)
