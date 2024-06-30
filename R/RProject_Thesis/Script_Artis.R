requiredPackages=c("sf","maptiles","terra","iNEXT","corrplot","e1071","MASS","car")
library(sf)
library(terra)
library(maptiles)
library(iNEXT)
library(ggplot2)

deployments_artis <- read.csv("../ArtisData/deployments.csv")
observations_artis <- read.csv("../ArtisData/observations.csv")
#events_artis <- read.csv("../ArtisData/events.csv")
#media_artis <- read.csv("../ArtisData/media.csv")
#events_artis <- read.csv("../ArtisData/events.csv")

observations_artis$date <- as.Date(observations_artis$eventStart)

deployments_artis$deploymentStart <- as.Date(deployments_artis$deploymentStart)
deployments_artis$deploymentEnd <- as.Date(deployments_artis$deploymentEnd)


deployments_artis$deploymentEnd[deployments_artis$deploymentEnd > as.Date('2023-01-01')] <- as.Date('2023-01-01')
deployments_artis <- deployments_artis[deployments_artis$deploymentID != "artis_20_03272023_wildlifecamera1", ]
observations_artis <- observations_artis[observations_artis$deploymentID != "artis_20_03272023_wildlifecamera1", ]

deployments_artis$time_active <- deployments_artis$deploymentEnd - deployments_artis$deploymentStart
deployments_artis$time_active <- as.numeric(deployments_artis$time_active)

locations <- deployments_artis[c("deploymentID","longitude", "latitude")]
locations_sf <- st_as_sf(locations,
                         coords = c("longitude", "latitude"),
                         crs = 4326)

locations_basemap <- get_tiles(locations_sf, provider = "Esri.WorldTopoMap")
# plot(locations_basemap)
# plot(locations_sf, add = TRUE, col = "red", pch = 16)
# distance_matrix <- st_distance(locations_sf)
# diag(distance_matrix) <- NA
# summary(as.vector(distance_matrix))

par(mar=c(4,8,1,1))
plot(0,
     type="n",
     xlim=c(min(deployments_artis$deploymentStart),max(deployments_artis$deploymentEnd)),
     ylim=c(1,nrow(locations)),
     yaxt="n",
     xaxt="n",
     xlab="Date",
     ylab=""
)
axis.Date(1,format="%Y-%m")
axis(2,at=c(1:nrow(locations)),labels=locations$deploymentID,las=1)
for(i in 1:nrow(deployments_artis)){
  curr_deployment = deployments_artis[i,]
  location_ind = which(locations$deploymentID==curr_deployment$deploymentID)
  lines(x=c(curr_deployment$deploymentStart,curr_deployment$deploymentEnd),
        y=c(location_ind,location_ind),
        lwd=2)
}

observations_artis$date <- as.Date(observations_artis$eventStart)

obs_deploy_index <- match(observations_artis$deploymentID,deployments_artis$deploymentID)
observations_artis$deploymentID <- deployments_artis$deploymentID[obs_deploy_index]
obs_place_ind = match(observations_artis$deploymentID,locations$deploymentID)
points(observations_artis$date,obs_place_ind,col="red",pch=16)

det_per_deployment <- aggregate(
  count~deploymentID,
  FUN=sum,
  data=observations_artis)
head(det_per_deployment)

pic_per_location<-aggregate(
  count~deploymentID,#number of objects (in a picture) BY deployment
  FUN=length,
  data=observations_artis)
head(pic_per_location)

for(deployment_artis in deployments_artis$deploymentID){
  deployment_obs <- observations_artis$scientificName[observations_artis$deploymentID==deployment_artis]
  uni_obs <- unique(deployment_obs)
  n_uni_obs <- length(uni_obs)
  deployments_artis$n_species[deployments_artis$deploymentID==deployment_artis] <- n_uni_obs
}


# plot(n_species~time_active,
#      data=deployments_artis,
#      #solid point symbol
#      pch=16,
#      #axis labels
#      xlab="Days active",
#      ylab="n. species detected")

# deployment <- "artis_17_wildlifecamera1"
# deploy_start <- deployments_artis$deploymentStart[deployments_artis$deploymentID==deployment]
# deploy_end <- deployments_artis$deploymentEnd[deployments_artis$deploymentID==deployment]
# deploy_obs <- observations_artis[observations_artis$deploymentID==deployment,]
# deploy_spec <- unique(deploy_obs$scientificName)
# deploy_days <- seq.Date(deploy_start,deploy_end,by="day")
# inc_mat <- matrix(0,length(deploy_spec),length(deploy_days))
# row.names(inc_mat) <- deploy_spec
# for(i in 1:length(deploy_days)){
#   currday <- deploy_days[i]
#   curr_spec <- deploy_obs$scientificName[deploy_obs$date==currday]
#   present_bool <- deploy_spec%in%curr_spec
#   inc_mat[,i]=as.numeric(present_bool)
# }
# 
# deployments_inc_mats = list()
# deployments_inc_mats[[1]] = inc_mat
# names(deployments_inc_mats)[[1]] = deployment
# 
# deployment <- "artis_23_wildlifecamera1"
# deploy_start <- deployments_artis$deploymentStart[deployments_artis$deploymentID==deployment]
# deploy_end <- deployments_artis$deploymentEnd[deployments_artis$deploymentID==deployment]
# deploy_obs <- observations_artis[observations_artis$deploymentID==deployment,]
# deploy_spec <- unique(deploy_obs$scientificName)
# deploy_days <- seq.Date(deploy_start,deploy_end,by="day")
# inc_mat <- matrix(0,length(deploy_spec),length(deploy_days))
# row.names(inc_mat) <- deploy_spec
# for(i in 1:length(deploy_days)){
#   currday <- deploy_days[i]
#   curr_spec <- deploy_obs$scientificName[deploy_obs$date==currday]
#   present_bool <- deploy_spec%in%curr_spec
#   inc_mat[,i]=as.numeric(present_bool)
# }
# 
# deployments_inc_mats[[2]] = inc_mat
# #give this the name of our deployment
# names(deployments_inc_mats)[[2]] = deployment
# 
# 
# iNET_out <- iNEXT(deployments_inc_mats,
#                   q=c(0),
#                   datatype="incidence_raw",
#                   endpoint=300)
# plot(iNET_out)
# 
# curveplot <- ggiNEXT(iNET_out, type = 1)
# 
# curveplot + labs(y='Species richness', x='Deployment days')
# 
# 
# iNET_out$AsyEst

# Assuming the dataframe is named df
# You can replace 'df' with the actual name of your dataframe

df<-deployments_artis

# Calculate the mean and standard deviation for time_active
mean_time_active <- mean(df$time_active, na.rm = TRUE)
sd_time_active <- sd(df$time_active, na.rm = TRUE)

# Calculate the mean and standard deviation for n_species
mean_n_species <- mean(df$n_species, na.rm = TRUE)
sd_n_species <- sd(df$n_species, na.rm = TRUE)

# Print the results
cat("Mean of time_active:", mean_time_active, "\n")
cat("Standard deviation of time_active:", sd_time_active, "\n")
cat("Mean of n_species:", mean_n_species, "\n")
cat("Standard deviation of n_species:", sd_n_species, "\n")


