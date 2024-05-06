library(ggplot2)
library(dplyr)

data <- simulated_data_log_normal

observation_counts <- data %>%
  group_by(scientificName) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 

ggplot(observation_counts, aes(x = reorder(scientificName, -count), y = count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  labs(x = "Scientific Name", y = "Number of Observations", title = "Observations per Scientific Name")

unique_species <- unique(data$scientificName)
days_since_first_observation <- numeric(length(unique_species))
for (i in seq_along(unique_species)) {
  species_data <- data[data$scientificName == unique_species[i], ]
  first_observation_date <- min(species_data$date)
  days_since_first_observation[i] <- as.numeric(Sys.Date() - first_observation_date)
}
result <- data.frame(Species = unique_species, Days_Since_First_Observation = days_since_first_observation)
result <- result[order(result$Days_Since_First_Observation, decreasing = TRUE), ]
print(result)

observation_counts <- observation_counts %>%
  mutate(rank = row_number(),  
         log_count = log10(count))

# Rank Abundance Diagram
ggplot(observation_counts, aes(x = rank, y = log_count)) +
  geom_point() +  
  geom_line() + 
  theme_minimal() +  
  labs(x = "Rank", y = "Log10('Abundance')", title = "Rank 'Abundance' Diagram (RAD)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


observation_counts$rounded_count <- round(observation_counts$count, -2) # Rounds to the nearest 100
abundance_distribution_rounded <- observation_counts %>%
  group_by(rounded_count) %>%
  summarise(num_species = n()) %>%
  ungroup()

ggplot(abundance_distribution_rounded, aes(x = rounded_count, y = num_species)) +
  geom_point() +
  geom_line() + 
  theme_minimal() +  
  scale_x_continuous(name = "Rounded 'Abundance' (number of individual observations)", breaks = seq(min(abundance_distribution_rounded$rounded_count), max(abundance_distribution_rounded$rounded_count), by = 200)) + 
  scale_y_continuous(name = "Number of Species") + 
  labs(title = "Species Abundance Distribution (Rounded)") 