library(ggplot2)
library(dplyr)
library(lubridate)

# Assume 'INTERRESULT_all_for_all' is loaded and available

# Prepare the plotting versions of the INTERRESULT data frames
PLOTTING_INTERRESULT_check_asymptote <- INTERRESULT_check_asymptote %>%
  mutate(
    Y_Label = paste(DeploymentID, "\nTarget:", Target),
    Time_Window = paste(Start_date, End_date, sep = " to ")
  ) %>%
  group_by(Time_Window) %>%
  mutate(
    Ratio = sum(Reached == "TRUE") / n()
  ) %>%
  ungroup() %>%
  mutate(
    Time_Window_Label = ifelse(is.na(Ratio), Time_Window, paste(Time_Window, "\nRatio:", sprintf("%.2f", Ratio)))
  )

create_window_plot <- function(ws) {
  data_to_plot <- PLOTTING_INTERRESULT_check_asymptote %>%
    filter(N_Days == ws)
  ratio_exceeded <- INTERRESULT_all_for_all %>%
    filter(Window_Size == ws) %>%
    pull(Ratio_Exceeded_Threshold)
  ratio_exceeded_mean <- INTERRESULT_all_for_all %>%
    filter(Window_Size == ws) %>%
    pull(Mean_Asymptote_Ratio)
  
  ggplot(data_to_plot, aes(x = Time_Window, y = Y_Label, fill = Reached)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = Observed), vjust = 0.8, color = "black", size = 5) +
    scale_fill_manual(values = c("TRUE" = "darkolivegreen2", "FALSE" = "darksalmon")) +
    labs(
      title = paste("Deployment Outcome by Time Window (", ws, " days)"),
      x = "Time Window",
      y = "Deployment ID",
      fill = "Threshold Reached"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
      axis.text.y = element_text(angle = 0, hjust = 1, size = 10),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      plot.title = element_text(hjust = 0.5, size = 18),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16)
    ) +
    coord_cartesian(clip = "off") +
    annotate(
      "text", x = Inf, y = Inf,
      label = paste("Exceeded Threshold Ratio:", sprintf("%.2f", ratio_exceeded),
                    "\nMean Threshold Reached:", sprintf("%.2f", ratio_exceeded_mean)),
      hjust = 0.42, vjust = 0.2, size = 5, color = "black"
    )
}

# Create and print window plots
window_plots <- lapply(unique(INTERRESULT_all_windows_for_windowsize_calculator$N_Days), create_window_plot)
lapply(window_plots[8], print)
