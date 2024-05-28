# Assuming observations_artis is your dataframe

# Step 1: Convert the eventStart column to Date
observations_artis$date <- as.Date(observations_artis$eventStart)

# Step 2: Extract the month from the Date object
observations_artis$month <- format(observations_artis$date, "%m")

# Step 3: Create a new column for the season based on the month
observations_artis$season <- with(observations_artis, ifelse(month %in% c("12", "01", "02"), "Winter",
                                                             ifelse(month %in% c("03", "04", "05"), "Spring",
                                                                    ifelse(month %in% c("06", "07", "08"), "Summer",
                                                                           ifelse(month %in% c("09", "10", "11"), "Fall", NA)))))

# Ensure the observations are in order by date
observations_artis <- observations_artis[order(observations_artis$date), ]

# Step 4: Split the dataset into four subsets based on the season column
observations_winter <- subset(observations_artis, season == "Winter")
observations_spring <- subset(observations_artis, season == "Spring")
observations_summer <- subset(observations_artis, season == "Summer")
observations_fall <- subset(observations_artis, season == "Fall")

# Display the head of each seasonal subset to verify
print(head(observations_winter))
print(head(observations_spring))
print(head(observations_summer))
print(head(observations_fall))
