library(ggplot2)
library(dplyr)

# Assuming simulated_incidence_data is the data frame you want to use
data <- simulated_incidence_data

# Summarizing observation counts by species
observation_counts <- data %>%
  group_by(scientificName) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) 

# Plotting the number of observations per species


# Calculating days since first observation for each species
days_since_first_observation <- data %>%
  group_by(scientificName) %>%
  summarise(first_observation_date = min(date)) %>%
  mutate(days_since_first_observation = as.numeric(Sys.Date() - first_observation_date)) %>%
  arrange(desc(days_since_first_observation))

days_since_first_observation

daily_species_count <- simulated_incidence_data %>%
  group_by(date) %>%
  summarise(num_unique_species = n_distinct(scientificName)) %>%
  ungroup()
species_plot <- ggplot(daily_species_count, aes(x = date, y = num_unique_species)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(title = "Number of Unique Species Spotted Each Day",
       x = "Day",
       y = "Number of Unique Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Enhance readability of x-axis labels

daily_observation_count <- simulated_incidence_data %>%
  group_by(date) %>%
  summarise(num_observations = sum(count)) %>%
  ungroup()

observation_plot <- ggplot(daily_observation_count, aes(x = date, y = num_observations)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Number of Observations Made Each Day",
       x = "Day",
       y = "Number of Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

grid.arrange(species_plot, observation_plot, ncol = 2)

# Creating a Rank Abundance Diagram
observation_counts <- observation_counts %>%
  mutate(rank = row_number(),  
         log_count = log10(count))

ggplot(observation_counts, aes(x = rank, y = log_count)) +
  geom_point() +  
  geom_line() + 
  theme_minimal() +  
  labs(x = "Rank", y = "Log10 of Total Observations", title = "Rank Abundance Diagram (RAD)")

# Analyzing the rounded abundance distribution
observation_counts$rounded_count <- round(observation_counts$count, -2) # Rounds to the nearest 100
abundance_distribution_rounded <- observation_counts %>%
  group_by(rounded_count) %>%
  summarise(num_species = n(), .groups = 'drop') %>%
  arrange(rounded_count)

ggplot(abundance_distribution_rounded, aes(x = rounded_count, y = num_species)) +
  geom_point() +
  geom_line() + 
  theme_minimal() +  
  scale_x_continuous(name = "Rounded Total Observations (nearest 100)", breaks = seq(min(abundance_distribution_rounded$rounded_count, na.rm = TRUE), max(abundance_distribution_rounded$rounded_count, na.rm = TRUE), by = 200)) + 
  scale_y_continuous(name = "Number of Species") + 
  labs(title = "Species Abundance Distribution (Rounded)")


ggplot(observation_counts, aes(x = reorder(scientificName, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  labs(x = "Scientific Name", y = "Total Observations", title = "Total Observations per Scientific Name")
