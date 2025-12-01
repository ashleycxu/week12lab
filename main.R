source("estimation.R")
source("optimization.R")

library(tidyverse)
set.seed(123)

# Load the sample dataset
bike_data <- read.csv("sample_bike.csv")

# Estimate arrival rates
arrival_rates <- estimate_arrival_rates(bike_data)

# Optimize bike placement for 50 bikes
# Use 10 days of simulated data
placement50 <- add_bikes(arrival_rates, 50, 10)
# Convert to data frame and save as csv
placement50 <- enframe(placement50)
names(placement50) <- c("station", "number of bikes")
write.csv(placement50, "results/placement50.csv", row.names=F, quote=F)

# Optimize bike placement for 100 bikes
placement100 <- add_bikes(arrival_rates, 100, 10)
# Convert to data frame and save as csv
placement100 <- enframe(placement100)
names(placement100) <- c("station", "number of bikes")
write.csv(placement100, "results/placement100.csv", row.names=F, quote=F)

# Optimize bike placement for 200 bikes
placement200 <- add_bikes(arrival_rates, 200, 10)
# Convert to data frame and save as csv
placement200 <- enframe(placement200)
names(placement200) <- c("station", "number of bikes")
write.csv(placement200, "results/placement200.csv", row.names=F, quote=F)


