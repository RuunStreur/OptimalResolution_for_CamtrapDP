library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(fitdistrplus)

observations <- processed_data$observations

# Filter out the specific deployment ID
filtered_observations <- observations %>%
  filter(deploymentID != "artis_20_03272023_wildlifecamera1")

# Get unique deployment IDs
deployment_ids <- unique(filtered_observations$deploymentID)

# Helper function to calculate the ratio of common to total species for each deployment
calculate_ratios <- function(observations_data) {
  deployment_ids <- unique(observations_data$deploymentID)
  
  results <- data.frame(
    deploymentID = character(length(deployment_ids)),
    total_species = integer(length(deployment_ids)),
    common_species = integer(length(deployment_ids)),
    rare_species = integer(length(deployment_ids)),
    common_ratio = numeric(length(deployment_ids)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(deployment_ids)) {
    deployment_id <- deployment_ids[i]
    data_filtered <- observations_data %>%
      filter(deploymentID == deployment_id) %>%
      group_by(scientificName) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count))
    
    fit <- fitdist(data_filtered$count, "lnorm")
    meanlog <- fit$estimate["meanlog"]
    sdlog <- fit$estimate["sdlog"]
    threshold <- exp(meanlog + .7 * sdlog)
    
    data_filtered <- data_filtered %>%
      mutate(classification = ifelse(count > threshold, "Common", "Rare"))
    
    common_count <- sum(data_filtered$classification == "Common")
    rare_count <- sum(data_filtered$classification == "Rare")
    total_count <- nrow(data_filtered)
    
    results[i, ] <- c(
      deployment_id,
      total_count,
      common_count,
      rare_count,
      common_count / total_count
    )
  }
  results$common_ratio <- as.numeric(results$common_ratio)
  results$total_species <- as.integer(results$total_species)
  results$common_species <- as.integer(results$common_species)
  results$rare_species <- as.integer(results$rare_species)
  ratio_results <<- results
  mean_ratio <<- mean(ratio_results$common_ratio)
  # mean_ratio <<- mean(results$common_ratio)
  return(results)
}

# Calculate ratios and mean ratio for the filtered observations
ratio_results <- calculate_ratios(filtered_observations)
mean_ratio <- mean(ratio_results$common_ratio)

# Function to create the plot for each deployment ID
create_plot <- function(deployment_id) {
  data_filtered <- filtered_observations %>%
    filter(deploymentID == deployment_id) %>%
    group_by(scientificName) %>%
    summarise(count = n(), .groups = 'drop') %>%
    arrange(desc(count))
  
  fit <- fitdist(data_filtered$count, "lnorm")
  meanlog <- fit$estimate["meanlog"]
  sdlog <- fit$estimate["sdlog"]
  threshold <- exp(meanlog + .8*sdlog)
  
  data_filtered <- data_filtered %>%
    mutate(classification = ifelse(count > threshold, "Common", "Rare"))
  
  ggplot(data_filtered, aes(x = reorder(scientificName, -count), y = count, fill = classification)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Common" = "darkolivegreen2", "Rare" = "lightsalmon"), 
                      name = "Species Class") +
    labs(
      title = paste(deployment_id)
    ) +
    theme(axis.text.x = element_blank(),    # Remove x-axis text
          axis.title.x = element_blank(),   # Remove x-axis title
          axis.ticks.x = element_blank(),   # Remove x-axis ticks
          axis.title.y = element_blank(),   # Remove y-axis title
          legend.position = "right",
          legend.text = element_text(size = 14),     # Increase legend text size
          legend.title = element_text(size = 16)    # Increase legend title size
          )# Keep the legend'
  
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
title <- textGrob("Number of observations and classification of species for all deployments in Artis", gp = gpar(fontsize = 20, fontface = "bold"))
y_label <- textGrob("Number of Observations", rot = 90, gp = gpar(fontsize = 20, fontface = "bold"))

# Arrange the final plot with title, y-axis label, and legend
final_plot <- grid.arrange(arrangeGrob(plot_grid, left = y_label, top = title), legend, ncol = 2, widths = c(4/5, 1/5))

# Draw the final plot
grid.newpage()
grid.draw(final_plot)