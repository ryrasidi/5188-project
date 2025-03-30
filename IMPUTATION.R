library(tidyverse)
library(stringr)

#Start logging
cat("Starting script execution...\n")

# Load the dataset
cat("Loading dataset...\n")
data <- read.csv("final_JURONG EAST_long.csv")

# Extract first month and year from the bi-monthly period
# Example: "Mar-Apr 2000" -> Extract "Mar" and "2000"
cat("Extracting first month and year...\n")
data$first_month <- str_extract(data$period, "^[A-Za-z]+")  # First month
data$year <- str_extract(data$period, "\\d{4}")             # Year

# Convert first month and year into proper Date format
cat("Converting to Date format...\n")
data$Date <- as.Date(paste("1", data$first_month, data$year), format="%d %b %Y")

# Create a full sequence of bi-monthly periods
cat("Creating full date sequence...\n")
full_dates <- seq(from = min(data$Date, na.rm = TRUE), 
                  to = max(data$Date, na.rm = TRUE), 
                  by = "2 months")

# Expand the dataset to include all missing periods
cat("Expanding dataset...\n")
expanded_data <- expand.grid(Date = full_dates, x = unique(data$x), y = unique(data$y))

# Merge with original data
cat("Merging with original data...\n")
merged_data <- left_join(expanded_data, data, by = c("Date", "x", "y"))

# Perform LOESS imputation
cat("Performing LOESS imputation...\n")
loess_fit <- merged_data %>%
  group_by(x, y) %>%
  mutate(avg_LST = ifelse(is.na(avg_LST),
                          predict(loess(avg_LST ~ as.numeric(Date), span = 0.9, data = ., na.action = na.exclude)),
                          avg_LST))

# Convert Date back to original bi-monthly format
cat("Converting Date back to original format...\n")
loess_fit$period <- paste0(format(loess_fit$Date, "%b"), "-", 
                           format(loess_fit$Date + months(1), "%b"), " ",
                           format(loess_fit$Date, "%Y"))

final_imputed_data <- loess_fit %>% select(x, y, period, avg_LST) %>% rename(Date = period, Value = avg_LST)

# Save the imputed dataset
cat("Saving the imputed data...\n")
write.csv(final_imputed_data, "final_imputed_JURONG_EAST.csv", row.names = FALSE)

cat("Script execution completed.\n")
