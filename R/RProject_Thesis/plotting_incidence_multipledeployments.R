library(ggplot2)
library(dplyr)
library(gridExtra)

# Filter data for the third deployment
deployment_id <- simulation_results$deployments$deploymentID[4]
data <- simulation_results$observations %>% 
  filter(deploymentID == deployment_id)

# Summarizing observation counts by species
observation_counts <- data %>%
  group_by(scientificName) %>%
  summarise(count = sum(count), .groups = 'drop') %>%
  arrange(desc(count))

# Plotting the number of observations per species
species_obs_plot <- ggplot(observation_counts, aes(x = reorder(scientificName, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Scientific Name", y = "Total Observations", title = "Total Observations per Scientific Name")

# Calculating days since first observation for each species
days_since_first_observation <- data %>%
  group_by(scientificName) %>%
  summarise(first_observation_date = min(date), .groups = 'drop') %>%
  mutate(days_since_first_observation = as.numeric(Sys.Date() - first_observation_date)) %>%
  arrange(desc(days_since_first_observation))

# Plot daily species count
daily_species_count <- data %>%
  group_by(date) %>%
  summarise(num_unique_species = n_distinct(scientificName), .groups = 'drop')

species_plot <- ggplot(daily_species_count, aes(x = date, y = num_unique_species)) +
  geom_line(color = "black") +
  labs(title = "Number of Unique Species Spotted Each Day",
       x = "Day",
       y = "Number of Unique Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot daily observation count
daily_observation_count <- data %>%
  group_by(date) %>%
  summarise(num_observations = sum(count), .groups = 'drop')

observation_plot <- ggplot(daily_observation_count, aes(x = date, y = num_observations)) +
  geom_line(color = "red") +
  labs(title = "Number of Observations Made Each Day",
       x = "Day",
       y = "Number of Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a Rank Abundance Diagram
observation_counts <- observation_counts %>%
  mutate(rank = row_number(),  
         log_count = log10(count))

rad_plot <- ggplot(observation_counts, aes(x = rank, y = log_count)) +
  geom_point() +  
  geom_line() + 
  theme_minimal() +  
  labs(x = "Rank", y = "Log10 of Total Observations", title = "Rank Abundance Diagram (RAD)")

# Display all plots together
grid.arrange(species_obs_plot, species_plot, observation_plot, rad_plot, nrow = 2, ncol = 2)
