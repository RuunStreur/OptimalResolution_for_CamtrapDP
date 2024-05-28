slope_target <- .1

size_based_est <- iNET_out$iNextEst$size_based

df <- size_based_est[, c("t", "qD")]

# Calculate slopes
df$next_qD <- dplyr::lead(df$qD)
df$next_t <- dplyr::lead(df$t)
df$slope <- (df$next_qD - df$qD) / (df$next_t - df$t)

df <- na.omit(df)

closest_point_index <- which.min(abs(df$slope - slope_target))
closest_point <- df[closest_point_index, ]

closest_sample_time <- closest_point$t
closest_slope <- closest_point$slope



#normalize
df$normalized_t <- (df$t - min(df$t)) / (max(df$t) - min(df$t))
df$normalized_qD <- (df$qD - min(df$qD)) / (max(df$qD) - min(df$qD))
df$next_normalized_qD <- dplyr::lead(df$normalized_qD)
df$next_normalized_t <- dplyr::lead(df$normalized_t)

# Calculate slope as change in normalized_qD over change in normalized_t
df$normalized_slope <- (df$next_normalized_qD - df$normalized_qD) / (df$next_normalized_t - df$normalized_t)

df <- na.omit(df)
closest_normalized_point_index <- which.min(abs(df$normalized_slope - slope_target))
closest_normalized_point <- df[closest_normalized_point_index, ]
closest_normalized_time <- closest_normalized_point$normalized_t
closest_normalized_slope <- closest_normalized_point$normalized_slope

original_t_value <- (closest_normalized_time * (max(df$t) - min(df$t))) + min(df$t)

print(paste("Closest sample time to target slope",slope_target ,": ", closest_sample_time, "deploymentdays"))
print(paste("Slope at this point: ", closest_slope))

print(paste("Original non-normalized 't' value corresponding to the closest normalized deploymentdays: ", original_t_value))











