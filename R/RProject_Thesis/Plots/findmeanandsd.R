# Load the required library
library(dplyr)





# Assume data is already loaded into a dataframe named df
# If not, the data can be read from a file using read.csv or similar functions
data <- processed_data$observations

data <- data %>%
  filter(!is.na(eventStart) & eventStart != "")

# Calculate the total number of observations
total_observations <- nrow(data)

# Number of unique deployments
unique_deployments <- data %>% 
  distinct(deploymentID) %>% 
  nrow()

# Calculate the exact number of observations per deploymentID
observations_per_deployment <- data %>% 
  group_by(deploymentID) %>% 
  summarise(count = n())

# Calculate average and standard deviation of observations per deployment
summary_stats <- observations_per_deployment %>% 
  summarise(average = mean(count), sd = sd(count))

average_observations <- summary_stats$average
sd_observations <- summary_stats$sd

# Display the results
total_observations
unique_deployments
average_observations
sd_observations

# Display the dataframe with the exact number of observations per deploymentID
observations_per_deployment


