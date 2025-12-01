library(tidyverse)
set.seed(123)

#' Thinning
#' @description Determines whether a simulated observation (customer) should be 
#' kept or discarded
#' @param start Origin station (int)
#' @param end Destination station (int)
#' @param t Time of arrival (num)
#' @param arrival_rates Data frame of arrival rates for each station pair and hour
#' @param lambda_max The maximum arrival rate (mu_hat) for the start/end station pair, 
#' calculated from arrival_rates (num)
#' @return Whether to keep the observation (0 or 1)
thinning <- function(start, end, t, arrival_rates, lambda_max) {
  
  # Get rows where start and end stations are correct
  rows <- filter(arrival_rates, start_station==start & end_station==end)
  
  if (floor(t) %in% rows$hour) {
    # Get row where time corresponds to the correct hour
    row <- filter(rows, hour==floor(t))
    # Get arrival rate for that station pair and hour
    current_mu_hat <- row$mu_hat
    # Calculate probability of keeping observation
    p <- current_mu_hat/lambda_max
  } else {
    # If there is no arrival rate for the station pair and hour, 
    # probability of keeping observation is 0
    p <- 0
  }
  # Flip a coin to determine whether observation is kept (0 or 1)
  keep <- rbinom(n=1, size=1, prob=p)
  return(keep)
}


#' Simulate demand
#' @description Simulates bike demand using data on past trips, i.e. simulates
#' customers coming to a station to use a bike
#' @param arrival_rates Data frame of arrival rates for each station pair and hour
#' @return Data frame of hypothetical trips and their times
simulate_demand <- function(arrival_rates) {
  
  # Calculate lambda_max (max arrival rate) for each pair of start/end stations
  lambda_vals <- arrival_rates %>%
    group_by(start_station, end_station) %>%
    summarize(lambda_max = max(mu_hat))
  
  # Simulate customers for each pair of start/end stations
  sim_demand <- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(sim_demand) <- c('start_station', 'end_station', 't')
  
  # Iterate through each pair of start/end stations
  for (n in 1:nrow(lambda_vals)) {
    row <- lambda_vals[n, ]
    t <- 0
    # Start at beginning of the day, and simulate arrivals until day is over
    while (t < 24) {
      # Sample time between customer arrivals
      e <- rexp(n=1, rate=row$lambda_max)
      # Add to current time
      t <- t+e
      if (t > 24) {
        break
      }
      # Determine whether arrival time is kept using thinning
      keep <- thinning(row$start_station, row$end_station, t, arrival_rates, row$lambda_max)
      if (keep==1) {
        sim_demand[nrow(sim_demand)+1, ] <- c(row$start_station, row$end_station, t)
      }
    }
  }
  return(sim_demand)
}


#' Simulate trips
#' @description Loops through simulated hypothetical bike trips and determines 
#' whether trip can occur, in which case bike is moved
#' @param sim_data Data frame of simulated demand
#' @param placement The initial placement of bikes (named vector)
#' @return Data frame of simulated demand with additional column indicating 
#' whether trip is possible
simulate_trips <- function(sim_data, placement) {
  
  # Sort hypothetical trips in chronological order
  sim_data <- arrange(sim_data, t)
  sim_data$trip_possible <- NA
  
  # Loop through hypothetical trips
  for (n in 1:nrow(sim_data)) {
    start <- as.character(sim_data$start_station[n])
    end <- as.character(sim_data$end_station[n])
    if (placement[start] > 0) {
      # If a bike is available, the trip is possible and bike is moved
      sim_data$trip_possible[n] <- TRUE
      placement[start] <- placement[start] - 1
      placement[end] <- placement[end] + 1
    } else {
      # If a bike is not available, the trip is not possible
      sim_data$trip_possible[n] <- FALSE
    }
  }
  return(sim_data)
}


#' Simulate day
#' @description Simulates a day of bikeshare data, including trips that did 
#' happen and trips that did not happen
#' @param arrival_rates Data frame of arrival rates for each station pair and hour 
#' @param k Total number of bikes
#' @param placement A named vector with number of bikes corresponding to each station
#' @return Data frame of simulated bikeshare data for a single day 
simulate_day <- function(arrival_rates, k, placement) {
  sim_data <- simulate_demand(arrival_rates)
  sim_data <- simulate_trips(sim_data, placement)
  return(sim_data)
}


#' Even placement
#' @description Evenly allocates a certain number of bikes among stations. 
#' If there is remainder after even allocation, these bikes are distributed to 
#' stations based on sorted number order.
#' @param arrival_rates Data frame of arrival rates for each station pair and hour 
#' (used to get list of unique stations)
#' @param k Total number of bikes
#' @return A named vector with number of bikes corresponding to each station
even_placement <- function(arrival_rates, k) {
  
  # Get list of unique stations
  stations <- sort(union(arrival_rates$start_station, arrival_rates$end_station))
  n_stations <- length(stations)
  
  # Evenly allocate bikes to each station
  placement <- rep(x = k %/% n_stations, times = n_stations)
  names(placement) <- stations
  
  # If there is a remainder, distribute to the first stations in sorted order
  if (k%%n_stations != 0) {
    placement[1:(k%%n_stations)] <- placement[1:(k%%n_stations)]+1
  }
  return(placement)
}


#' Random placement
#' @description Randomly allocates a certain number of bikes among stations
#' @param arrival_rates Data frame of arrival rates for each station pair and hour 
#' (used to get list of unique stations)
#' @param k Total number of bikes
#' @return A named vector with number of bikes corresponding to each station
random_placement <- function(arrival_rates, k) {
  
  # Get list of unique stations
  stations <- sort(union(arrival_rates$start_station, arrival_rates$end_station))
  n_stations <- length(stations)
  
  # Initialize placement vector
  placement <- rep(0, n_stations)
  names(placement) <- stations
  
  # For each bike, randomly choose a station and place the bike there
  for (i in 1:k) {
    s <- as.character(sample(x=stations, size=1))
    placement[s] <- placement[s] + 1
  }
  return(placement)
}

