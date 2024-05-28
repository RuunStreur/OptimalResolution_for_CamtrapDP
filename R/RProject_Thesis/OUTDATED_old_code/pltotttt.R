library(ggplot2)
library(dplyr)
library(gridExtra)  # Load gridExtra for arranging plots
library(grid)       # Load grid for annotations

# Filter out the specific deployment ID
filtered_observations <- observations_artis %>%
  filter(deploymentID != "artis_20_03272023_wildlifecamera1")

# Get unique deployment IDs
deployment_ids <- unique(filtered_observations$deploymentID)

# Function to create the plot for each deployment ID
create_plot <- function(deployment_id) {
  data_filtered <- filtered_observations %>%
    filter(deploymentID == deployment_id) %>%
    group_by(scientificName) %>%
    summarise(count = n(), .groups = 'drop') %>%
    arrange(desc(count))
  
  ggplot(data_filtered, aes(x = reorder(scientificName, -count), y = count)) +
    geom_bar(stat = "identity") +
    labs(
      title = paste(deployment_id)
    ) +
    theme(axis.text.x = element_blank(),    # Remove x-axis text
          axis.title.x = element_blank(),   # Remove x-axis title
          axis.ticks.x = element_blank(),   # Remove x-axis ticks
          axis.title.y = element_blank())   # Remove y-axis title
}

# Create plots for each deployment ID
plots <- lapply(deployment_ids, create_plot)

# Combine plots into a grid
plot_grid <- arrangeGrob(grobs = plots, ncol = 5)  # Adjust ncol as needed for layout

# Add a title and y-axis label
title <- textGrob("Number of observations per species for", gp = gpar(fontsize = 20, fontface = "bold"))
y_label <- textGrob("Number of Observations", rot = 90, gp = gpar(fontsize = 20, fontface = "bold"))

# Arrange the final plot with title and y-axis label
final_plot <- grid.arrange(arrangeGrob(plot_grid, left = y_label, top = title))

# Draw the final plot
grid.newpage()
grid.draw(final_plot)
