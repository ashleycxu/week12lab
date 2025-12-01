library(tidyverse)
set.seed(123)

source("simulation.R")

#' Add bikes
#' @description Optimizes bike placement using simulated bikeshare data from 
#' a certain number of days. Starts with 0 bikes placed and iteratively adds a 
#' bike to the most unhappy station until all bikes have been placed.
#' @param arrival_rates Data frame of arrival rates for each station pair and hour
#' @param k Total number of bikes to be placed among stations
#' @param days Total number of days to simulate
#' @return A named vector with number of bikes placed at each station
add_bikes <- function(arrival_rates, k, days) {
  
  # Initialize empty data frame of simulated data
  sim_data <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(sim_data) <- c('start_station', 'end_station', 't', 'trip_possible')
  
  # Simulate trips for each day, and append to previous days' data each time
  # Start with random distribution of bikes
  init_place <- random_placement(arrival_rates, k)
  for (day in 1:days) {
    sim_day <- simulate_day(arrival_rates, k, init_place)
    # print(mean(sim_day$trip_possible))
    sim_data <- full_join(sim_data, sim_day)
  }
  
  # Estimate average number of unhappy customers at each station
  satisfaction <- sim_data %>%
    group_by(start_station) %>%
    summarize(unhappy = sum(!trip_possible)/days)

  # Start with 0 bikes at each station
  placement <- even_placement(arrival_rates, 0)
  
  # Iterate until we have placed all k bikes
  while (k > 0) {
    # Place 1 bike at the most unhappy station
    satisfaction <- arrange(satisfaction, desc(unhappy))
    station <- satisfaction$start_station[1]
    placement[as.character(station)] <- placement[as.character(station)] + 1
    # Update average # of unhappy customers at that station
    satisfaction[satisfaction$start_station==station, ]$unhappy <- 
      satisfaction[satisfaction$start_station==station, ]$unhappy - 1
    # Subtract 1 from # of bikes left to be placed
    k <- k-1
  }
  return(placement)
}



