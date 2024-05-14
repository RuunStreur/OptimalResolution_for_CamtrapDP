library(dplyr)

check_deployment_asymptote <- function(deployments, observations, species_asymptote_threshold) {
  species_richness_estimates <- Calculate_iNEXT_results %>%
    filter(Diversity == "Species richness") %>%
    select(Assemblage, Estimator)
  
  observed_species_counts <- observations %>%
    group_by(deploymentID) %>%
    summarise(Observed_Species = n_distinct(scientificName), .groups = 'drop')
  
  deployment_checks <- left_join(species_richness_estimates, observed_species_counts, by = c("Assemblage" = "deploymentID"))
  
  deployment_checks <- deployment_checks %>%
    mutate(Target = floor(Estimator * species_asymptote_threshold),
           Reached_Asymptote = Observed_Species >= Target)
  
  return(deployment_checks)
}

# Define the function to compute additional metrics including the refined third metric
compute_advanced_metrics <- function(df) {
  max_window_size <- max(df$Window_Size)  # Get the maximum window size for normalization
  
  df %>%
    mutate(
      Simple_Inverse = ( Ratio_Exceeded_Threshold  / Window_Size),
      Simple_Inverse_mean = ( Mean_Asymptote_Ratio  / Window_Size),
      # Ratio2_Inverse = (( (100*Ratio_Exceeded_Threshold) *  (100*Ratio_Exceeded_Threshold)) / Window_Size),
      # sqrt_ws_Inverse = (Ratio_Exceeded_Threshold  / sqrt(Window_Size)),
    )
}


optimize_values <- compute_advanced_metrics(final_results)

# Assuming optimize_values is your dataframe as shown above
top_simple_inverse <- optimize_values %>%
  arrange(desc(Simple_Inverse)) %>%
  slice(1) %>%
  select(Window_Size, Ratio_Exceeded_Threshold)

# Display the result
print(top_simple_inverse)


# find_pareto_front <- function(data) {
#   pareto_front <- do.call(rbind, lapply(1:nrow(data), function(i) {
#     current_point <- data[i,]
#     print(data)
#     is_dominated <- any(apply(data, 1, function(x) {
#       (x['Ratio_Exceeded_Threshold'] > current_point['Ratio_Exceeded_Threshold'] & x['Window_Size'] <= current_point['Window_Size']) |
#         (x['Ratio_Exceeded_Threshold'] >= current_point['Ratio_Exceeded_Threshold'] & x['Window_Size'] < current_point['Window_Size'])
#     }))
#     if (!is_dominated) return(current_point)
#     else return(NULL)
#   }))
#   pareto_front
# }
# 
# deployment_asymptote_results <- check_deployment_asymptote(deployments, observations, 0.9)
# print(deployment_asymptote_results)
# 
# pareto_front <- find_pareto_front(final_results)
# print(pareto_front)
