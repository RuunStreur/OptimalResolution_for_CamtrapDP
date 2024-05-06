library(tidyverse)

num_species <- 50
deployment_duration <- 365
num_observations <- 2000
set.seed(42)

# Parameters for the log-normal distribution
meanlog <- 0
sdlog <- 2

# Generate rarity scores based on a log-normal distribution
rarity_scores_log_normal <- rlnorm(num_species, meanlog = meanlog, sdlog = sdlog)
rarity_scores_log_normal <- rarity_scores_log_normal / sum(rarity_scores_log_normal) # Normalize scores
species_names <- paste0("Species_", seq_len(num_species))
names(rarity_scores_log_normal) <- species_names

# Function to simulate daily incidence data with multiple observations
simulate_daily_incidence_data <- function(rarity_scores, deployment_duration, num_observations) {
  start_date <- Sys.Date()
  end_date <- start_date + deployment_duration
  dates <- seq(start_date, end_date, by = "day")
  
  # Create a list to store daily observations
  daily_data <- vector("list", length = length(dates))
  names(daily_data) <- as.character(dates)
  
  # Calculate daily observations
  daily_obs_count <- round(num_observations / length(dates))
  
  # Simulate data for each day
  for (i in seq_along(dates)) {
    # Simulate multiple observations based on daily count
    observed_species <- sample(species_names, size = daily_obs_count, replace = TRUE, prob = rarity_scores)
    
    # Create a tibble with count of occurrences
    observations_tibble <- tibble(
      date = rep(dates[i], length(observed_species)),
      scientificName = observed_species
    ) %>%
      group_by(date, scientificName) %>%
      summarise(count = n(), .groups = 'drop')
    
    # Store daily observations in the list
    daily_data[[i]] <- observations_tibble
  }
  
  # Combine all daily observations into a single dataframe
  bind_rows(daily_data)
}

# Generate the data
simulated_incidence_data <- simulate_daily_incidence_data(rarity_scores_log_normal, deployment_duration, num_observations)

# Summarize the data to find the number of unique species per day
daily_species_count <- simulated_incidence_data %>%
  group_by(date) %>%
  summarise(num_unique_species = n_distinct(scientificName)) %>%
  ungroup()

# Create the plot
ggplot(daily_species_count, aes(x = date, y = num_unique_species)) +
  geom_line(color = "blue", size = 1) +  # Line plot
  labs(title = "Number of Unique Species Spotted Each Day",
       x = "Day",
       y = "Number of Unique Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Calculate the number of unique species in the data
num_unique_species <- n_distinct(simulated_incidence_data$scientificName)

# Print the number of unique species
print(paste("Number of unique species observed:", num_unique_species))

