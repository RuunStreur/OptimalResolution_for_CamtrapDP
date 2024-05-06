library(iNEXT)
library(dplyr)

# Unique species for a subset (needs rarefaction by bootstrapping)
calculate_coverage <- function(data, start_date, end_date) {
  subset_data <- data %>% 
    filter(date >= start_date & date <= end_date)
    unique_species <- unique(subset_data$scientificName)
  
  return(length(unique_species))
}

# find the optimal duration
find_optimal_duration <- function(data, max_days, target_species_count, step_size = 10) {
  start_date <- min(data$date)
  end_date <- start_date + max_days
  
  for (days in seq(step_size, max_days, by = step_size)) {
    current_end_date <- start_date + days
    species_count <- calculate_coverage(data, start_date, current_end_date)
    
    if (species_count >= target_species_count) {
      # Refine search
      for (refine_day in seq(days - step_size + 1, days)) {
        current_end_date <- start_date + refine_day
        species_count <- calculate_coverage(data, start_date, current_end_date)
        if (species_count >= target_species_count) {
          return(refine_day)
        }
      }
    }
  }
  
  return(NA)
}

observations_simulated$date <- as.Date(observations_simulated$date)
target_species_count <- ceiling(0.99 * length(unique(observations_simulated$scientificName)))
max_deployment_days <- as.integer(max(observations_simulated$date) - min(observations_simulated$date))

optimal_days <- find_optimal_duration(observations_simulated, max_deployment_days, target_species_count)

print(optimal_days)
