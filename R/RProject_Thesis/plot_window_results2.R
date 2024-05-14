library(ggplot2)
library(dplyr)
library(lubridate)

# Assume 'final_results' is loaded and available

# Prepare the plotting versions of the INTERRESULT data frames
PLOTTING_INTERRESULT_check_asymptote <- INTERRESULT_check_asymptote %>%
  mutate(
    Deployment_Label = paste(DeploymentID, "\nTarget:", Target),
    Start_date = as.Date(Start_date),
    End_date = as.Date(End_date),
    Time_Window = paste(Start_date, End_date, sep = " to ")
    
  )

PLOTTING_INTERRESULT_all_windows_for_windowsize_calculator <- INTERRESULT_all_windows_for_windowsize_calculator %>%
  mutate(
    Start_date = as.Date(sub(" -.*", "", Daterange), format = "%d/%m/%y"),
    End_date = as.Date(sub(".*- ", "", Daterange), format = "%d/%m/%y")
  )

# Calculate ratio of TRUE vs FALSE for 'Reached' for each Time_Window
ratios <- PLOTTING_INTERRESULT_check_asymptote %>%
  group_by(Time_Window) %>%
  summarise(
    Ratio = sum(Reached == "TRUE") / n(),
    .groups = 'drop'
  )

# Join the plotting dataframe with ratios
PLOTTING_INTERRESULT_check_asymptote <- PLOTTING_INTERRESULT_check_asymptote %>%
  left_join(ratios, by = "Time_Window") %>%
  mutate(
    Time_Window_Label = ifelse(is.na(Ratio), Time_Window, paste(Time_Window, "\nRatio:", sprintf("%.2f", Ratio)))
  )

window_sizes <- unique(PLOTTING_INTERRESULT_all_windows_for_windowsize_calculator$N_Days)

# Generate plots for each window size
plots <- lapply(window_sizes, function(ws) {
  data_to_plot <- PLOTTING_INTERRESULT_check_asymptote %>%
    filter(N_Days == ws)
  ratio_exceeded <- final_results %>%
    filter(Window_Size == ws) %>%
    pull(Ratio_Exceeded_Threshold)
  
  ggplot(data_to_plot, aes(x = Time_Window_Label, y = Deployment_Label, fill = Reached)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = Observed), vjust = 1.5, color = "black", size = 3) +
    scale_fill_manual(values = c("TRUE" = "darkolivegreen2", "FALSE" = "darksalmon")) +
    labs(
      title = paste("Deployment Outcome by Time Window (", ws, " days)"),
      x = "Time Window",
      y = "Deployment ID",
      fill = "Threshold Reached"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = element_text(angle = 0, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    ) +     coord_cartesian(clip = "off") +  # Prevent clipping off the text

    annotate("text", x = Inf, y = Inf, label = paste("Exceeded Threshold Ratio:", sprintf("%.2f", ratio_exceeded)), hjust = .42, vjust = -.7, size = 5, color = "black")
  
})

# Print all plots
lapply(plots, print)
