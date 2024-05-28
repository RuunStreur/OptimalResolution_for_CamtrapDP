requiredPackages <- c("sf", "iNEXT", "tidyverse")
lapply(requiredPackages, require, character.only = TRUE)

deployment <- simulated_deployment
observations <- simulated_incidence_data

# Ensure the start and end dates in deployments are formatted as Date
deployment$deploymentStart <- as.Date(deployment$deploymentStart)
deployment$deploymentEnd <- as.Date(deployment$deploymentEnd)
deployment$time_active <- as.numeric(deployment$deploymentEnd - deployment$deploymentStart)
observations$date <- as.Date(observations$date)

# Create an incidence matrix
unique_dates <- sort(unique(observations$date))
unique_species <- sort(unique(observations$scientificName))
inc_mat_sim <- matrix(0, nrow = length(unique_species), ncol = length(unique_dates), dimnames = list(unique_species, unique_dates))

for (i in seq_along(unique_dates)) {
  daily_obs <- observations[observations$date == unique_dates[i], ]
  for (j in seq_along(unique_species)) {
    if (any(daily_obs$scientificName == unique_species[j])) {
      inc_mat_sim[j, i] <- 1
    }
  }
}

# Prepare data for iNEXT analysis
deployments_inc_mat_sims <- list(Simulated_Camera_1 = inc_mat_sim)

# iNEXT analysis
iNET_out <- iNEXT(deployments_inc_mat_sims, q = 0, datatype = "incidence_raw", endpoint = 750)

# Plotting the rarefaction curve
curve_plot <- ggiNEXT(iNET_out, type = 1)
curve_plot + labs(title = "Rarefaction Curve", y = 'Species richness', x = 'Deployment days')

# Display the asymptotic richness estimate
print(iNET_out$AsyEst)
