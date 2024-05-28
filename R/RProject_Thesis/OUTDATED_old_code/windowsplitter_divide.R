library(tidyverse)
library(iNEXT)

# Function to split data and evaluate species richness per subset
evaluate_time_window_richness <- function(incidence_data, iNEXT_output, num_windows) {
  # Calculate total estimated species and 90% of this estimate
  total_estimated_species <- iNEXT_output$AsyEst$Estimator[1]  # Assuming species richness is the first row
  target_species <- floor(total_estimated_species * 0.9)
  print(target_species)
  
  # Define the time windows
  start_date <- as.Date(min(incidence_data$date))
  end_date <- as.Date(max(incidence_data$date))
  time_period <- as.numeric(end_date - start_date)
  window_length <- ceiling(time_period / num_windows)
  
  # Data structure to store the results
  results <- tibble(
    window = integer(),
    days_in_window = integer(),
    window_start_date = as.Date(character()),
    window_end_date = as.Date(character()),
    reached_target = logical(),
    observed_species_count = integer()
  )
  
  # Split data and calculate for each window
  for (i in seq_len(num_windows)) {
    window_start <- start_date + (i-1) * window_length
    window_end <- start_date + i * window_length - 1
    if (i == num_windows) window_end <- end_date  # Ensure last window covers all remaining days
    
    # Filter data for the current window
    window_data <- incidence_data %>%
      filter(date >= window_start & date <= window_end) %>%
      distinct(scientificName)  # Consider each species once per window
    
    # Calculate observed species richness
    observed_species_count <- n_distinct(window_data$scientificName)
    
    # Check if observed species reach the adjusted target
    reached_target <- observed_species_count >= target_species
    
    # Calculate the number of days in this window
    days_in_window <- as.numeric(window_end - window_start + 1)
    
    # Record results
    results <- results %>%
      add_row(
        window = i,
        days_in_window = days_in_window,
        window_start_date = window_start,
        window_end_date = window_end,
        reached_target = reached_target,
        observed_species_count = observed_species_count
      )
  }
  
  return(results)
}

set.seed(64)  

results <- evaluate_time_window_richness(simulated_incidence_data, iNET_out, 2)
print(results)
