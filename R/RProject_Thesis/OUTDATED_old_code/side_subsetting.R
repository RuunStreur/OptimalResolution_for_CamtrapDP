library(taxize)
library(dplyr)
deployments_artis <- read.csv("../ArtisData/deployments.csv")
observations_artis <- read.csv("../ArtisData/observations.csv")

unique_species <- observations_artis %>%
  filter(!is.na(scientificName)) %>%
  select(scientificName) %>%
  distinct()

species_list <- c(
  "Rattus norvegicus", "Apodemus sylvaticus", "Columba palumbus", "Felis catus",
  "Turdus merula", "Parus major", "Erithacus rubecula", "Cyanistes caeruleus",
  "Fringilla coelebs", "Prunella modularis", "Troglodytes troglodytes",
  "Streptopelia decaocto", "Gallinula chloropus", "Coloeus monedula",
  "Alopochen aegyptiaca", "Ardea cinerea", "Corvus corone", "Nycticorax nycticorax",
  "Phalacrocorax carbo", "Turdus philomelos", "Passer domesticus", "Turdus iliacus",
  "Erinaceus europaeus", "Mus musculus", "Fulica atra", "Columbidae",
  "Capreolus capreolus", "Lepus europaeus", "Chroicocephalus ridibundus",
  "Certhia brachydactyla", "Garrulus glandarius", "Turdus viscivorus",
  "Sylvia atricapilla", "Scolopax rusticola", "Corvidae", "Turdus", "Corvus",
  "Ardea purpurea", "Passeriformes", "Phasianus colchicus", "Felis", "Buteo buteo",
  "Phylloscopus trochilus", "Dendrocopos major", "Anas platyrhynchos domesticus",
  "Anas", "Phoenicurus ochruros"
)

is_bird <- function(species_name) {
  classification <- tax_name(query = species_name, get = "class", db = "itis")
  if ("Aves" %in% classification$class) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

bird_check <- sapply(species_list, is_bird)
bird_species <- c(species_list[bird_check],"Anas platyrhynchos domesticus")
bird_observations <- observations_artis %>%
  filter(scientificName %in% bird_species)

is_mammal <- function(species_name) {
  classification <- tax_name(query = species_name, get = "class", db = "itis")
  if ("Mammalia" %in% classification$class) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

mammal_check <- sapply(species_list, is_mammal)
mammal_species <- species_list[mammal_check]
mammal_observations <- observations_artis %>%
  filter(scientificName %in% mammal_species)


###############################################################################


deployments_artis <- read.csv("../ArtisData/deployments.csv")
observations_artis <- read.csv("../ArtisData/observations.csv")
observations_artis$date <- as.Date(observations_artis$eventStart)
observations_artis$month <- format(observations_artis$date, "%m")
observations_artis$season <- with(observations_artis, ifelse(month %in% c("12", "01", "02"), "Winter",
                                                             ifelse(month %in% c("03", "04", "05"), "Spring",
                                                                    ifelse(month %in% c("06", "07", "08"), "Summer",
                                                                           ifelse(month %in% c("09", "10", "11"), "Fall", NA)))))

observations_artis <- observations_artis[order(observations_artis$date), ]

observations_winter <- subset(observations_artis, season == "Winter")
observations_spring <- subset(observations_artis, season == "Spring")
observations_summer <- subset(observations_artis, season == "Summer")
observations_fall <- subset(observations_artis, season == "Fall")