library(tidyverse)

num_observations <- 20000
num_species <- 30
deployment_duration <- 365

set.seed(42)

# parameters for the log-normal distribution
meanlog <- 0
sdlog <- 2

# Rarity scores 
rarity_scores_log_normal <- rlnorm(num_species, meanlog = meanlog, sdlog = sdlog)
rarity_scores_log_normal <- rarity_scores_log_normal / sum(rarity_scores_log_normal) # Normalize scores
species_names <- paste0("Species_", seq_len(num_species))
names(rarity_scores_log_normal) <- species_names

# simulation func
simulate_camera_trap_data <- function(num_observations, rarity_scores, deployment_duration) {
  start_date <- Sys.Date()  
  end_date <- start_date + deployment_duration
  dates <- sample(seq(start_date, end_date, by = "day"), num_observations, replace = TRUE)
  
  simulated_data <- tibble(
    observationID = paste("obs_", format(Sys.time(), "%Y%m%d%H%M%S"), "Simulated_Camera_1", seq_len(num_observations), sep = "_"),
    deploymentID = rep("Simulated_Camera_1", num_observations),
    mediaID = rep("", num_observations),
    eventID = rep("", num_observations),
    eventStart = rep("", num_observations),
    eventEnd = rep("", num_observations),
    observationType = rep("NA", num_observations),
    scientificName = sample(species_names, num_observations, replace = TRUE, prob = rarity_scores),
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

# Generate
simulated_data_log_normal <- simulate_camera_trap_data(num_observations, rarity_scores_log_normal, deployment_duration)

# Dataframe of species names and their corresponding rarity (probability)
species_probabilities <- tibble(
  scientificName = names(rarity_scores_log_normal),
  probability = rarity_scores_log_normal
)


# Create deployment
simulated_deployment <- data.frame(
  deploymentID = rep("Simulated_Camera_1", 1),
  cameraID = NA,
  deploymentStart = NA,
  deploymentEnd = NA,
  cameraModel = NA,
  latitude = NA,
  longitude = NA,
  time_active = NA,
  n_species = NA,
  stringsAsFactors = FALSE
)

simulated_deployment$cameraID <- "Simulated_Camera_1"
simulated_deployment$latitude <- 52.354461295263874
simulated_deployment$longitude <- 4.955756812216412
simulated_deployment$deploymentStart <- min(simulated_data_log_normal$date)
simulated_deployment$deploymentEnd <- max(simulated_data_log_normal$date)
simulated_deployment$cameraModel <- "Simulated_Camera_Model"
simulated_deployment$time_active <- as.numeric(max(simulated_data_log_normal$date) - min(simulated_data_log_normal$date))
simulated_deployment$n_species <- length(unique(simulated_data_log_normal$scientificName))




