
# clear working directory
rm(list = ls())

scripts = list.files("../Code/GP-models/spTimer", full.names = TRUE)

library(spTimer)

#memory.limit(size = 8000)  # Increase memory limit (in MB) In windows

library(ff)
library(dplyr)

# Load the dataset
data <- read.csv("changi_train_wide.csv")

# Randomly select 4000 rows
set.seed(123)  # Ensures reproducibility
data <- data %>% sample_n(4000)

# Add small random noise to coordinates
set.seed(123)
data <- data %>%
  mutate(
    x = x + runif(n(), -0.1, 0.1),  # Add noise to x
    y = y + runif(n(), -0.1, 0.1)   # Add noise to y
  )

#rescaling
data <- data %>%
  mutate(
    x = (x - min(x)) / (max(x) - min(x)),
    y = (y - min(y)) / (max(y) - min(y))
  )

# Extract coordinates
coords <- as.matrix(data[, c("x", "y")])

# Define spatial decay prior [Updated]
decay_prior <- spTimer::spT.decay(
  distribution = spTimer::Gamm(a = 2, b = 1),
  tuning = 0.1
)

# Define priors
priors <- spTimer::spT.priors(
  model = "GP",
  inv.var.prior = spTimer::Gamm(2, 0.1),
  beta.prior = spTimer::Norm(0, 10^4),
)

# Define the specific two-month periods you want to run
selected_months <- c("Mar-Apr 2000", "May-Jun 2000")  # Replace with your desired months

# Loop over the selected months and fit the model
models <- list()
for (month in selected_months) {
  formula <- as.formula(paste0("`", month, "` ~ x + y"))  # Create formula dynamically
  
  # Fit the model
  model <- spTimer::spT.Gibbs(
    formula = formula,
    data = data,
    coords = coords,
    model = "GP",
    priors = priors,
    decay = decay_prior,
    nItr = 100,
    nBurn = 10,
    cov.fnc = "exponential",
    tol.dist = 1e-2
  )
  
  # Store model in the list
  models[[month]] <- model
  
  # Print progress
  cat("Finished model for:", month, "\n")
}

# Print summaries of the models for the selected months
lapply(models, summary)
