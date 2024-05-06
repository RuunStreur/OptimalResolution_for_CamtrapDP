library(tidyverse)
library(iNEXT)

# Function to find the largest optimal number of windows with at least 50% success rate
find_largest_optimal_time_window <- function(incidence_data, iNEXT_output, max_windows) {
  last_successful_num_windows <- NULL  # Store the last successful number of windows
  detailed_results <- tibble(num_windows = integer(), 
                             success_ratio = numeric(),
                             days_in_window = integer())  # Store results for all window configurations
  
  # Loop through potential numbers of windows from 2 up to the specified maximum
  for (num_windows in 1:max_windows) {
    # Evaluate species richness for the current number of windows
    results <- evaluate_time_window_richness(incidence_data, iNEXT_output, num_windows)
    
    # Calculate the ratio of windows that reached the target
    ratio_reached_target <- mean(results$reached_target)
    
    # Add results to the detailed results dataframe
    detailed_results <- detailed_results %>%
      add_row(num_windows = num_windows,
              success_ratio = ratio_reached_target,
              days_in_window = results$days_in_window[1])  # Assuming all windows have the same number of days
    
    # Check if at least 50% of windows reached the target
    if (ratio_reached_target >= 0.5) {
      # Update last successful number of windows
      last_successful_num_windows <- num_windows
    } else {
      # If a configuration fails to meet the criteria, break the loop
      break
    }
  }
  
  # Check if there was any successful configuration
  if (!is.null(last_successful_num_windows)) {
    return(list(
      largest_optimal_num_windows = last_successful_num_windows,
      reached_target_ratio = mean(results$reached_target[results$window <= last_successful_num_windows]),
      detailed_results = detailed_results
    ))
  } else {
    return(list(
      message = "No window size found that satisfies the condition of reaching the target in at least 50% of splits.",
      detailed_results = detailed_results
    ))
  }
}

# Example usage
# Assuming simulated_incidence_data and iNEXT_output are prepared and loaded
optimal_result <- find_largest_optimal_time_window(simulated_incidence_data, iNET_out, 12)
print(optimal_result$detailed_results)
