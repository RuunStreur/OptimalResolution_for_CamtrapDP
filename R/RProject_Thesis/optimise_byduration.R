library(tidyverse)
library(iNEXT)

target_rate<-1

# Function to find the smallest optimal time window duration with at least 50% success rate
find_smallest_optimal_time_window <- function(incidence_data, iNEXT_output, min_days_per_window) {
  # Set an initial large number of days per window, such as half the total deployment days
  start_date <- as.Date(min(incidence_data$date))
  end_date <- as.Date(max(incidence_data$date))
  initial_days_per_window <- as.numeric(end_date - start_date + 1)
  
  last_successful_days_per_window <- NULL  # Store the last successful days per window that met the condition
  detailed_results <- tibble(days_per_window = integer(), 
                             success_ratio = numeric(),
                             days_in_window = integer())  # Store results for all day configurations
  
  # Loop through potential numbers of days per window starting from the initial large size down to the minimum
  for (days_per_window in seq(initial_days_per_window, min_days_per_window, by = -1)) {
    # Evaluate species richness for the current number of days per window
    results <- evaluate_time_window_richness_by_days(incidence_data, iNEXT_output, days_per_window)
    
    # Calculate the ratio of windows that reached the target
    ratio_reached_target <- mean(results$reached_target)
    
    # Add results to the detailed results dataframe
    detailed_results <- detailed_results %>%
      add_row(days_per_window = days_per_window,
              success_ratio = ratio_reached_target,
              days_in_window = days_per_window)  # Using the input days per window as all windows have the same duration
    
    # Check if at least 50% of windows reached the target
    if (ratio_reached_target > 0) {
      last_successful_days_per_window <- days_per_window
    }
  }
  
  # Check if there was any successful configuration
  if (!is.null(last_successful_days_per_window)) {
    return(list(
      smallest_optimal_days_per_window = last_successful_days_per_window,
      reached_target_ratio = mean(results$reached_target),
      detailed_results = detailed_results
    ))
  } else {
    return(list(
      message = "No window size found in which asymptote is reached.",
      detailed_results = detailed_results
    ))
  }
}

# Example usage
# Assuming simulated_incidence_data and iNEXT_output are prepared and loaded
optimal_result <- find_smallest_optimal_time_window(simulated_incidence_data, iNET_out, 1)  # Reduce to weekly increments
print(optimal_result$detailed_results)

# Extract the detailed results dataframe from the optimal_result
detailed_results <- optimal_result$detailed_results

successful_configs <- detailed_results %>%
  filter(success_ratio >= target_rate)
if(nrow(successful_configs) > 0) {
  min_successful_days <- min(successful_configs$days_per_window)
  print(paste("The smallest optimal number of days per window that maintains at least a",target_rate, "success rate is:", min_successful_days))
} else {
  print("No configurations met the 50% threshold.")
}
