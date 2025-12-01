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
#' @return A list with (1) a vector with percentage of successful trips for each 
#' simulated day, and (2) a named vector with number of bikes placed at each station
add_bikes <- function(arrival_rates, k, days) {
  
  # Initialize empty data frame of simulated data
  sim_data <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(sim_data) <- c('start_station', 'end_station', 't', 'trip_possible')
  
  # Simulate trips for each day, and append to previous days' data each time
  # Start with random distribution of bikes
  init_place <- random_placement(arrival_rates, k)
  # Initialize vector to keep track of percentage of successful trips for each day
  pct_trips <- c()
  for (day in 1:days) {
    sim_day <- simulate_day(arrival_rates, k, init_place)
    pct_trips <- c(pct_trips, mean(sim_day$trip_possible))
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
  return(list(pct_trips=pct_trips, placement=placement))
}


#' Optimization results
#' @description Saves optimized placement as csv files and also saves results on 
#' percentage of successful trips before and after optimization
#' @param arrival_rates Data frame of arrival rates for each station pair and hour
#' @param k Total number of bikes to be placed among stations
#' @param days Total number of days to simulate
#' @return Nothing, but saves results as csv files
optimization_results <- function(arrival_rates, k, days) {

  # Get results from placing bikes
  placement_results <- add_bikes(arrival_rates, k, days)
  # Percentage of successful trips for each simulated day
  pct_trips <- placement_results$pct_trips
  placement <- placement_results$placement
  
  # Simulate new day using optimized placement
  new_sim <- simulate_day(arrival_rates, k, placement)
  new_pct <- mean(new_sim$trip_possible)
  
  # Convert optimized placement to data frame and save as csv
  placement <- enframe(placement)
  names(placement) <- c("station", "number of bikes")
  write.csv(placement, paste0("results/placement",k,"_",days,".csv"), row.names=F, quote=F)
  
  # Append average percentage of successful trips across simulated days
  pct_trips <- c(pct_trips, mean(pct_trips))
  # Append percentage of successful trips after new day with optimized placement
  pct_trips <- c(pct_trips, new_pct)
  
  # Add names to vector
  for (d in 1:days) {
    names(pct_trips)[d] <- paste0("sim day ", d)
  }
  names(pct_trips)[days+1] <- "Avg of sim days"
  names(pct_trips)[days+2] <- "After opt placement"
  
  # Convert to data frame and save results as csv
  pct_trips <- enframe(pct_trips)
  names(pct_trips) <- c("", "% of successful trips")
  write.csv(pct_trips, paste0("results/results",k,"_",days,".csv"), row.names=F, quote=F)
}

