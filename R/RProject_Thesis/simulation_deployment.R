# Extracting necessary columns from simulated_data
simulated_deployment <- data.frame(
  deploymentID = rep("Simulated_Camera_1", 1),
  cameraID = NA,
  deploymentStart = NA,
  deploymentEnd = NA,
  cameraModel = NA,
  latitude = NA,
  longitude = NA,
  time_active = NA,
  n_species = NA,
  stringsAsFactors = FALSE
)

# Assigning values to the extracted columns
simulated_deployment$cameraID <- "Simulated_Camera_1"
simulated_deployment$latitude <- 52.354461295263874
simulated_deployment$longitude <- 4.955756812216412
simulated_deployment$deploymentStart <- min(simulated_data_log_normal$date)
simulated_deployment$deploymentEnd <- max(simulated_data_log_normal$date)
simulated_deployment$cameraModel <- "Simulated_Camera_Model"
simulated_deployment$time_active <- as.numeric(max(simulated_data_log_normal$date) - min(simulated_data_log_normal$date))
simulated_deployment$n_species <- length(unique(simulated_data_log_normal$scientificName))

# Printing simulated_deployment
print(simulated_deployment)
