library(ggplot2)
library(dplyr)
library(lubridate)


INTERRESULT_check_asymptote <- INTERRESULT_check_asymptote %>%
  mutate(Deployment_Label = paste(DeploymentID, "\nTarget:", Target))

INTERRESULT_all_windows_for_windowsize_calculator <- INTERRESULT_all_windows_for_windowsize_calculator %>%
  mutate(Start_date = as.Date(sub(" -.*", "", Daterange), format = "%d/%m/%y"),
         End_date = as.Date(sub(".*- ", "", Daterange), format = "%d/%m/%y"))

INTERRESULT_check_asymptote <- INTERRESULT_check_asymptote %>%
  mutate(Start_date = as.Date(Start_date),
         End_date = as.Date(End_date))

# Join INTERRESULT_check_asymptote data onto INTERRESULT_all_windows_for_windowsize_calculator based on overlapping dates
matched_data <- INTERRESULT_all_windows_for_windowsize_calculator %>%
  rowwise() %>%
  do({
    daterange <- .
    INTERRESULT_check_asymptote %>%
      filter(Start_date <= daterange$End_date & End_date >= daterange$Start_date) %>%
      mutate(Time_Window = daterange$Daterange)
  }) %>%
  ungroup()

INTERRESULT_check_asymptote <- INTERRESULT_check_asymptote %>%
  mutate(
    Start_date = as.Date(Start_date),
    End_date = as.Date(End_date),
    Time_Window = paste(Start_date, End_date, sep = " to ")
  )

window_size_mapping <- INTERRESULT_all_windows_for_windowsize_calculator %>%
  select(Daterange, N_Days) %>%
  unique()

window_sizes <- unique(window_size_mapping$N_Days)

# Loop through each window size and create a plot
plots <- lapply(window_sizes, function(ws) {
  data_to_plot <- INTERRESULT_check_asymptote %>%
    filter(N_Days == ws)
  
  ggplot(data_to_plot, aes(x = Time_Window, y = Deployment_Label, fill = Reached)) +
    geom_tile(color = "white", size = 0.5) +  # Creates tiles for each data point
    geom_text(aes(label = paste(Observed)), color = "black", size = 3) +
    scale_fill_manual(values = c("TRUE" = "darkolivegreen2", "FALSE" = "darksalmon")) +  # Color coding
    labs(
      title = paste("Deployment Outcome by Time Window (", ws, " days)", sep=""),
      x = "Time Window",
      y = "Deployment ID",
      fill = "Threshold Reached"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = element_text(angle = 0, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
})

lapply(plots, print)

