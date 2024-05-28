# calculate the deployment duration for observing 95% of estimated total species
calculate_percent_duration <- function(iNEXT_output, observed_data) {
  
  total_estimated_species <- iNEXT_output$AsyEst$Estimator[1]  # q = 0
  
  species_target <- total_estimated_species * 0.95
  
  observed_sorted <- observed_data[order(observed_data$date),]
  
  cumulative_species <- cumsum(!duplicated(observed_sorted$scientificName))
  
  sufficient_coverage_date <- observed_sorted$date[which(cumulative_species >= species_target)[1]]
  
  start_date <- min(observed_sorted$date)
  days_to_percent <- as.numeric(sufficient_coverage_date - start_date)
  
  return(days_to_percent)
}

deployment_days_to_percent <- calculate_percent_duration(iNET_out, deploy_obs)

print(paste("Deployment days needed:", deployment_days_to_percent))
