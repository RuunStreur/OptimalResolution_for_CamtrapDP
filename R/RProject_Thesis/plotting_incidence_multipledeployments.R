library(ggplot2)
library(dplyr)
library(gridExtra)


rarity_scores_log_normal <- simulation_results$rarity_scores
# Create a data frame from the rarity scores
species_rarity_df <- tibble(
  Species = names(rarity_scores_log_normal),
  Rarity_Score = rarity_scores_log_normal
)

# Reorder Species factor based on Rarity_Score in descending order
species_rarity_df <- species_rarity_df %>%
  mutate(Species = reorder(Species, Rarity_Score, FUN = function(x) -x))

# First Plot - Original rarity scores
original_plot <- ggplot(species_rarity_df, aes(x = Species, y = Rarity_Score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Original Species Rarity Scores",
    x = "Species",
    y = "Rarity Score"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Filter data for a specific deployment
deployment_id <- simulation_results$deployments$deploymentID[3]
data <- simulation_results$observations %>% 
  filter(deploymentID == deployment_id)

# Summarizing observation counts by species
observation_counts <- data %>%
  group_by(scientificName) %>%
  summarise(count = sum(count), .groups = 'drop')

# Include all species, also those not observed
all_species_df <- species_rarity_df %>%
  select(Species) %>%
  left_join(observation_counts, by = c("Species" = "scientificName")) %>%
  replace_na(list(count = 0))

# Calculate total observations for normalization, including unobserved species as 0
total_observations <- sum(all_species_df$count)

# Normalize observation counts to rarity scores
all_species_df <- all_species_df %>%
  mutate(normalizedRarityScore = count / total_observations)

# Create a data frame formatted like the initial rarity scores
normalized_rarity_df <- tibble(
  Species = all_species_df$Species,
  Rarity_Score = all_species_df$normalizedRarityScore
)

# Reorder the Species factor based on Rarity_Score in descending order
normalized_rarity_df <- normalized_rarity_df %>%
  mutate(Species = reorder(Species, Rarity_Score, FUN = function(x) -x))

# Second Plot - Normalized rarity scores based on observations
normalized_plot <- ggplot(normalized_rarity_df, aes(x = Species, y = Rarity_Score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    title = paste("Frequency of detection of", deployment_id),
    x = "Species",
    y = "Rarity Score"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Arrange plots side by side with equal y scales


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
  labs(title = "Number of unique species spotted per day by Deployment 2",
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
  labs(title = "Number of observations made each day by Deployment 2",
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
grid.arrange(observation_plot, species_plot, nrow = 1, ncol = 2)
grid.arrange(original_plot, normalized_plot, nrow = 1, ncol = 2)


