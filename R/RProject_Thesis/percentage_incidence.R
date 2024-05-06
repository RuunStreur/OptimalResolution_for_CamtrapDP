# Function to calculate the deployment duration for observing 95% of estimated total species
calculate_percent_duration <- function(iNEXT_output, incidence_data) {
  
  # Get the estimated total species count from iNEXT output
  total_estimated_species <- iNEXT_output$AsyEst$Estimator[1]  # q = 0, assuming first row is for species richness
  
  # Target 95% of the estimated species
  species_target <- total_estimated_species * .95
  print(species_target)
  
  # Ensure the data is sorted by date
  incidence_sorted <- incidence_data %>%
    arrange(date) %>%
    distinct(scientificName, .keep_all = TRUE)  # Keep only the first observation of each species
  
  # Calculate the cumulative number of unique species up to each day
  cumulative_species <- incidence_sorted %>%
    mutate(cumulative_species = cumsum(!duplicated(scientificName)))
  
  # Find the first date where the cumulative number of species meets or exceeds the target
  sufficient_coverage_date <- cumulative_species %>%
    filter(cumulative_species >= species_target) %>%
    summarize(first_date = min(date)) %>%
    pull(first_date)
  
  # Calculate the number of days from the start of deployment to when the target was met
  start_date <- min(incidence_sorted$date)
  days_to_percent <- as.numeric(sufficient_coverage_date - start_date)
  
  return(days_to_percent)
}

# Example use of the function with iNEXT output and incidence data
# Assuming `iNET_out` from a previous iNEXT analysis and `simulated_incidence_data` as the incidence data
deployment_days_to_percent <- calculate_percent_duration(iNET_out, simulated_incidence_data)

print(paste("Deployment days needed to observe 95% of estimated species:", deployment_days_to_percent))
