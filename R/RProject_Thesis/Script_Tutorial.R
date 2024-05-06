#List of packages
requiredPackages=c("sf","maptiles","terra","iNEXT","corrplot","e1071","MASS","car")
library(sf)
library(terra)
library(maptiles)

deployments <- read.csv("../Tutorial_CameraTraps/cameratrap_data/deployments.csv")
observations <- read.csv("../Tutorial_CameraTraps/cameratrap_data/images.csv")

unique_locations <- length(unique(deployments$placename))
unique_deployments <- length(unique(deployments$deployment_id))
cat("Number of unique locations:", unique_locations, "\n")
cat("Number of unique deployments:", unique_deployments, "\n")

deployments$start_date <- as.Date(deployments$start_date)
deployments$end_date <- as.Date(deployments$end_date)
deployments$time_active <- deployments$end_date - deployments$start_date
deployments$time_active <- as.numeric(deployments$time_active)

unique_images <- length(unique(observations$image_id))
cat("Number of unique images:", unique_images, "\n")
total_observations <- nrow(observations)
cat("Number of observations:", total_observations, "\n")

dup_locations <- duplicated(deployments$placename)
locations <- deployments[!dup_locations,c("placename","longitude", "latitude")]
locations_sf <- st_as_sf(locations,
                         coords = c("longitude", "latitude"),
                         crs = 4326)

locations_basemap <- get_tiles(locations_sf, provider = "Esri.WorldTopoMap")
plot(locations_basemap)
plot(locations_sf, add = TRUE, col = "red", pch = 16)