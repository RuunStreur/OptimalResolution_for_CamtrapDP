library(tidyverse)
library(gridExtra)

set.seed(120)

num_species <- 50
deployment_duration <- 365
num_observations <- 15000

# Parameters for the log-normal distribution
meanlog <- 0
sdlog <- 2

# Generate rarity scores based on a log-normal distribution
rarity_scores_log_normal <- rlnorm(num_species, meanlog = meanlog, sdlog = sdlog)
rarity_scores_log_normal <- rarity_scores_log_normal / sum(rarity_scores_log_normal) # Normalize scores
species_names <- paste0("Species_", seq_len(num_species))
names(rarity_scores_log_normal) <- species_names

# Function to simulate daily incidence data with variable observation counts
simulate_daily_incidence_data <- function(rarity_scores, deployment_duration, num_observations) {
  start_date <- Sys.Date()
  end_date <- start_date + deployment_duration
  dates <- seq(start_date, end_date, by = "day")
  
  avg_daily_observations <- round(num_observations / length(dates))
  
  all_observations <- data.frame(
    observationID = character(),
    deploymentID = character(),
    mediaID = character(),
    eventID = character(),
    eventStart = character(),
    eventEnd = character(),
    observationType = character(),
    scientificName = character(),
    count = integer(),
    lifeStage = character(),
    sex = character(),
    behavior = character(),
    individualID = character(),
    bboxX = character(),
    bboxY = character(),
    bboxWidth = character(),
    bboxHeight = character(),
    classificationMethod = character(),
    classifiedBy = character(),
    classificationProbability = character(),
    observationComments = character(),
    date = as.Date(character())
  )
  
  # Simulate data for each day
  for (i in seq_along(dates)) {
    # Adjust daily observations to add variability
    daily_obs_count <- sample(seq(from = avg_daily_observations * 0.3, to = avg_daily_observations * 1.7, by = 1), 1)
    
    # Sample species based on rarity scores
    observed_species <- sample(species_names, size = daily_obs_count, replace = TRUE, prob = rarity_scores)
    
    daily_data <- tibble(
      observationID = paste("obs_", format(Sys.time(), "%Y%m%d%H%M%S"), "Simulated_Camera_1", seq_len(length(observed_species)), sep = "_"),
      deploymentID = rep("Simulated_Camera_1", length(observed_species)),
      mediaID = rep("", length(observed_species)),
      eventID = rep("", length(observed_species)),
      eventStart = rep("", length(observed_species)),
      eventEnd = rep("", length(observed_species)),
      observationType = rep("NA", length(observed_species)),
      scientificName = observed_species,
      count = rep(1, length(observed_species)),
      lifeStage = rep("NA", length(observed_species)),
      sex = rep("NA", length(observed_species)),
      behavior = rep("NA", length(observed_species)),
      individualID = rep("NA", length(observed_species)),
      bboxX = rep("NA", length(observed_species)),
      bboxY = rep("NA", length(observed_species)),
      bboxWidth = rep("NA", length(observed_species)),
      bboxHeight = rep("NA", length(observed_species)),
      classificationMethod = rep("NA", length(observed_species)),
      classifiedBy = rep("NA", length(observed_species)),
      classificationProbability = rep("NA", length(observed_species)),
      observationComments = rep("", length(observed_species)),
      date = rep(dates[i], length(observed_species))
    )
    
    all_observations <- bind_rows(all_observations, daily_data)
  }
  
  return(all_observations)
}

simulated_incidence_data <- simulate_daily_incidence_data(rarity_scores_log_normal, deployment_duration, num_observations)


# Create a deployment
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

# Assigning values to the extracted columns
simulated_deployment$cameraID <- "Simulated_Camera_1"
simulated_deployment$latitude <- 52.354461295263874
simulated_deployment$longitude <- 4.955756812216412
simulated_deployment$deploymentStart <- min(simulated_incidence_data$date)
simulated_deployment$deploymentEnd <- max(simulated_incidence_data$date)
simulated_deployment$cameraModel <- "Simulated_Camera_Model"
simulated_deployment$time_active <- as.numeric(max(simulated_incidence_data$date) - min(simulated_incidence_data$date))
simulated_deployment$n_species <- length(unique(simulated_incidence_data$scientificName))

# Printing simulated_deployment
print(simulated_deployment)

