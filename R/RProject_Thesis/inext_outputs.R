# Assume 'observations' is your complete dataframe with multiple deployments
observations <- simulation_results$observations
observations$date <- as.Date(observations$date)

# Function to create incidence matrix from observations of a single deployment
create_incidence_matrix <- function(data) {
  unique_dates <- sort(unique(data$date))
  unique_species <- sort(unique(data$scientificName))
  inc_matrix <- matrix(0, nrow = length(unique_species), ncol = length(unique_dates), dimnames = list(unique_species, unique_dates))
  
  for (i in seq_along(unique_dates)) {
    daily_obs <- data[data$date == unique_dates[i], ]
    for (species in unique_species) {
      inc_matrix[species, i] <- any(daily_obs$scientificName == species)
    }
  }
  return(inc_matrix)
}

# Split observations by 'deploymentID' and create incidence matrices
incidence_matrices <- observations %>%
  group_by(deploymentID) %>%
  group_split() %>%
  map(~ create_incidence_matrix(.x))

# Associate each matrix with its corresponding deploymentID
deployment_ids <- unique(observations$deploymentID)
names(incidence_matrices) <- deployment_ids

# Run iNEXT analysis on each matrix
# Initialize a list to store the iNEXT output for each deployment
iNEXT_outputs <- list()

# Loop through the incidence matrices by their names
for (deployment_id in names(incidence_matrices)) {
  # Run iNEXT analysis on each matrix
  iNEXT_outputs[[deployment_id]] <- iNEXT(incidence_matrices[[1]], q = 0, datatype = "incidence_raw", endpoint = 750)
}



# You now have a list of iNEXT analysis results, one for each deployment
print(iNEXT_outputs)