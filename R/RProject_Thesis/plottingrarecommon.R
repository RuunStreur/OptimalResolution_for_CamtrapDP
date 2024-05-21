library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(fitdistrplus)

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
  
  # print(paste("Deployment ID:", deployment_id))
  # print(data_filtered)
  
  # Fit a lognormal distribution to the counts
  fit <- fitdist(data_filtered$count, "lnorm")
  
  # Use one standard deviation above the mean of the fitted distribution to classify species
  meanlog <- fit$estimate["meanlog"]
  sdlog <- fit$estimate["sdlog"]
  threshold <- exp(meanlog + .8*sdlog)
  
  data_filtered <- data_filtered %>%
    mutate(classification = ifelse(count > threshold, "Common", "Rare"))
  

  ggplot(data_filtered, aes(x = reorder(scientificName, -count), y = count, fill = classification)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Common" = "darkolivegreen2", "Rare" = "lightsalmon"), 
                      name = "Species Type Classification") +
    labs(
      title = paste(deployment_id)
    ) +
    theme(axis.text.x = element_blank(),    # Remove x-axis text
          axis.title.x = element_blank(),   # Remove x-axis title
          axis.ticks.x = element_blank(),   # Remove x-axis ticks
          axis.title.y = element_blank(),   # Remove y-axis title
          legend.position = "right")        # Keep the legend
}

# Create plots for each deployment ID
plots <- lapply(deployment_ids, create_plot)

# Extract legend from one of the plots
g_legend <- function(a.gplot){
  tmp <- ggplotGrob(a.gplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Get the legend from the first plot
legend <- g_legend(plots[[1]])

# Remove legends from all individual plots
plots <- lapply(plots, function(plot) plot + theme(legend.position = "none"))

# Combine plots into a grid
plot_grid <- arrangeGrob(grobs = plots, ncol = 5)  # Adjust ncol as needed for layout

# Add a title and y-axis label
title <- textGrob("Number of observations per species for all deployments in Artis", gp = gpar(fontsize = 20, fontface = "bold"))
y_label <- textGrob("Number of Observations", rot = 90, gp = gpar(fontsize = 20, fontface = "bold"))

# Arrange the final plot with title, y-axis label, and legend
final_plot <- grid.arrange(arrangeGrob(plot_grid, left = y_label, top = title), legend, ncol = 2, widths = c(4/5, 1/5))

# Draw the final plot
grid.newpage()
grid.draw(final_plot)
