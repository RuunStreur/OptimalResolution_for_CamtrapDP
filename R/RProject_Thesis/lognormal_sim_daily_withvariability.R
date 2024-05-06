library(tidyverse)
library(gridExtra)

set.seed(41)

num_species <- 50
deployment_duration <- 365
num_observations <- 10000

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
  
  # Average daily observations
  avg_daily_observations <- round(num_observations / length(dates))
  
  # Create a dataframe to store all observations
  all_observations <- data.frame()
  
  # Simulate data for each day
  for (i in seq_along(dates)) {
    # Adjust daily observations by +/- 50%
    daily_obs_count <- sample(seq(from = avg_daily_observations * 0.5, to = avg_daily_observations * 1.5, by = 1), 1)
    
    # Sample species based on rarity scores
    observed_species <- sample(species_names, size = daily_obs_count, replace = TRUE, prob = rarity_scores)
    
    # Create a tibble for daily data
    daily_data <- tibble(
      date = rep(dates[i], length(observed_species)),
      scientificName = observed_species,
      count = 1
    ) %>%
      group_by(date, scientificName) %>%
      summarise(count = n(), .groups = 'drop')
    
    # Append to all observations
    all_observations <- bind_rows(all_observations, daily_data)
  }
  
  return(all_observations)
}

# Generate the simulated data
simulated_incidence_data <- simulate_daily_incidence_data(rarity_scores_log_normal, deployment_duration, num_observations)

num_unique_species <- n_distinct(simulated_incidence_data$scientificName)
print(paste("Number of unique species observed:", num_unique_species))



