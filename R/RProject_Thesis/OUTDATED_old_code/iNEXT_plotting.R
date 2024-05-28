requiredPackages=c("sf","maptiles","terra","iNEXT","corrplot","e1071","MASS","car")
library(sf)
library(terra)
library(maptiles)
library(iNEXT)
library(ggplot2)

deployments_simulated <- simulated_deployment
observations_simulated <- simulated_data_log_normal

deployments_simulated$deploymentStart <- as.Date(deployments_simulated$deploymentStart)
deployments_simulated$deploymentEnd <- as.Date(deployments_simulated$deploymentEnd)
deployments_simulated$time_active <- deployments_simulated$deploymentEnd - deployments_simulated$deploymentStart
deployments_simulated$time_active <- as.numeric(deployments_simulated$time_active)

locations <- deployments_simulated[c("deploymentID","longitude", "latitude")]
locations_sf <- st_as_sf(locations,
                         coords = c("longitude", "latitude"),
                         crs = 4326)

observations_simulated$date <- as.Date(observations_simulated$date)
obs_deploy_index <- match(observations_simulated$deploymentID,deployments_simulated$deploymentID)
observations_simulated$deploymentID <- deployments_simulated$deploymentID[obs_deploy_index]
obs_place_ind = match(observations_simulated$deploymentID,locations$deploymentID)


for(deployment_artis in deployments_simulated$deploymentID){
  deployment_obs <- observations_simulated$scientificName[observations_simulated$deploymentID==deployment_artis]
  uni_obs <- unique(deployment_obs)
  n_uni_obs <- length(uni_obs)
  deployments_simulated$n_species[deployments_simulated$deploymentID==deployment_artis] <- n_uni_obs
}

deployment <- "Simulated_Camera_1"
deploy_start <- deployments_simulated$deploymentStart[deployments_simulated$deploymentID==deployment]
deploy_end <- deployments_simulated$deploymentEnd[deployments_simulated$deploymentID==deployment]
deploy_obs <- observations_simulated[observations_simulated$deploymentID==deployment,]
deploy_spec <- unique(deploy_obs$scientificName)
deploy_days <- seq.Date(deploy_start,deploy_end,by="day")
inc_mat_sim <- matrix(0,length(deploy_spec),length(deploy_days))
row.names(inc_mat_sim) <- deploy_spec
for(i in 1:length(deploy_days)){
  currday <- deploy_days[i]
  curr_spec <- deploy_obs$scientificName[deploy_obs$date==currday]
  present_bool <- deploy_spec%in%curr_spec
  inc_mat_sim[,i]=as.numeric(present_bool)
}

deployments_inc_mat_sims = list()
deployments_inc_mat_sims[[1]] = inc_mat_sim
names(deployments_inc_mat_sims)[[1]] = deployment

iNET_out <- iNEXT(deployments_inc_mat_sims,
                  q=0,
                  datatype="incidence_raw",
                  endpoint=750)

curveplot <- ggiNEXT(iNET_out, type = 1)
curveplot + labs(y='Species richness', x='Deployment days')

iNET_out$AsyEst
