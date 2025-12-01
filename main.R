source("estimation.R")
source("simulation.R")
source("optimization.R")

library(tidyverse)
set.seed(123)

# Load the sample dataset
bike_data <- read.csv("sample_bike.csv")

# Estimate arrival rates
arrival_rates <- estimate_arrival_rates(bike_data)

# Optimize bike placement for 50 bikes
# Using 10 days of simulated data
optimization_results(arrival_rates, 50, 10)

# Optimize bike placement for 100 bikes
# Using 10 days of simulated data
optimization_results(arrival_rates, 100, 10)

# Optimize bike placement for 200 bikes
# Using 10 days of simulated data
optimization_results(arrival_rates, 200, 10)

# Optimize bike placement for 50 bikes
# But using 20 days of simulated data
optimization_results(arrival_rates, 50, 20)


