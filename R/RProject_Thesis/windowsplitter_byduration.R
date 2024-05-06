library(tidyverse)
library(iNEXT)
set.seed(64)  
days_per_window <- 120  
evaluate_time_window_richness_by_days <- function(incidence_data, iNEXT_output, days_per_window) {
  #total_estimated_species <- floor(iNEXT_output$AsyEst$Estimator[1])
  total_estimated_species <- floor(iNEXT_output$Estimator[1])
  target_species <- floor(total_estimated_species * 0.95)
  start_date <- as.Date(min(incidence_data$date))
  end_date <- as.Date(max(incidence_data$date))
  total_days <- as.numeric(end_date - start_date + 1)
  
  num_full_windows <- total_days %/% days_per_window
  days_in_last_window <- total_days %% days_per_window
  
  num_windows <- num_full_windows
  
  results <- tibble(
    window = integer(),
    days_in_window = integer(),
    window_start_date = as.Date(character()),
    window_end_date = as.Date(character()),
    reached_target = logical(),
    observed_species_count = integer()
  )
  
  current_start_date <- start_date
  
  for (i in 1:num_windows) {
    window_end_date <- current_start_date + days_per_window - 1
    
    window_data <- incidence_data %>%
      filter(date >= current_start_date & date <= window_end_date) %>%
      distinct(scientificName)  # Consider each species once per window
    
    observed_species_count <- n_distinct(window_data$scientificName)
    
    reached_target <- observed_species_count >= target_species
    
    results <- results %>%
      add_row(
        window = i,
        days_in_window = as.numeric(window_end_date - current_start_date + 1),
        window_start_date = current_start_date,
        window_end_date = window_end_date,
        reached_target = reached_target,
        observed_species_count = observed_species_count
      )
    
    current_start_date <- window_end_date + 1
  }
  
  return(results)
}

results_by_days <- evaluate_time_window_richness_by_days(simulated_incidence_data, iNET_out, days_per_window)
print(results_by_days)
