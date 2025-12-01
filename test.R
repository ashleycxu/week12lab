source("estimation.R")
source("simulation.R")
source("optimization.R")

library(tidyverse)
set.seed(123)

toy_data <- data.frame(X = c(1,2,3,4,5,6,7,8),
                       start_station = c("1","1","2","2","2","2","1","1"),
                       end_station = c(2,2,1,1,1,1,2,2),
                       start_time = c("2022-02-15 00:12:00", "2022-02-15 00:13:00", 
                                      "2022-02-15 00:13:00", "2022-02-15 00:14:00",
                                      "2022-02-16 00:12:00", "2022-02-16 00:13:00",
                                      "2022-02-16 00:13:00", "2022-02-16 00:14:00"),
                       end_time = c("2022-02-15 00:12:00", "2022-02-15 00:13:00", 
                                    "2022-02-15 00:13:00", "2022-02-15 00:14:00",
                                    "2022-02-16 00:12:00", "2022-02-16 00:13:00",
                                    "2022-02-16 00:13:00", "2022-02-16 00:14:00"), 
                       customer_type = rep("None", 8))


# Test estimation of arrival rates
test_estimation <- estimate_arrival_rates(toy_data)

# Test random placement of bikes
random <- random_placement(test_estimation, 10)

# Test simulation of bike trips
test_simulation <- simulate_day(test_estimation, 10, random)

# Test optimization of bike placement
test_placement <- add_bikes(test_estimation, 10, 5)$placement


