library(tidyverse)
set.seed(123)

#' Thinning
#' @description 
#' @param 
#' @return whether to keep the observation (0 or 1)
thinning <- function(start, end, t, arrival_rates, lambda_max) {
  rows <- filter(arrival_rates, start_station==start & end_station==end)
  if (floor(t) %in% rows$hour) {
    row <- filter(rows, hour==floor(t))
    current_mu_hat <- row$mu_hat
    p <- current_mu_hat/lambda_max
  } else {
    p <- 0
  }
  keep <- rbinom(n=1, size=1, prob=p)
  return(keep)
}
    
thinning(10, 4, 6.5, arrival_rates, 0.3897344)


#' Simulate demand
#' @description 
#' @param 
#' @return 
simulate_demand <- function(arrival_rates) {
  # calculate lambda_max (max arrival rate) for each pair of start/end stations
  lambda_vals <- arrival_rates %>%
    group_by(start_station, end_station) %>%
    summarize(lambda_max = max(mu_hat))
  
  # simulate day for each pair of start/end stations
  sim_demand <- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(sim_demand) <- c('start_station', 'end_station', 't')
  for (n in 1:nrow(lambda_vals)) {
    row <- lambda_vals[n, ]
    t <- 0
    while (t < 24) {
      e <- rexp(n=1, rate=row$lambda_max)
      t <- t+e
      if (t > 24) {
        break
      }
      keep <- thinning(row$start_station, row$end_station, t, arrival_rates, row$lambda_max)
      if (keep==1) {
        sim_demand[nrow(sim_demand)+1, ] <- c(row$start_station, row$end_station, t)
      }
    }
  }
  return(list(lambda_vals=lambda_vals, sim_demand=sim_demand))
}

lambda_vals <- simulate_demand(arrival_rates)$lambda_vals 
sim_demand <- simulate_demand(arrival_rates)$sim_demand


#' Simulate trips
#' @description 
#' @param 
#' @return 
simulate_trips <- function(sim_data, placement) {
  sim_data <- arrange(sim_data, t)
  sim_data$trip_possible <- NA
  sim_data$buffer <- NA
  for (n in 1:nrow(sim_data)) {
    start <- as.character(sim_data$start_station[n])
    end <- as.character(sim_data$end_station[n])
    if (placement[start] > 0) {
      sim_data$trip_possible[n] <- TRUE
      placement[start] <- placement[start] - 1
      placement[end] <- placement[end] + 1
      sim_data$buffer[n] <- placement[start]
    } else {
      sim_data$trip_possible[n] <- FALSE
      sim_data$buffer[n] <- 0
    }
  }
  return(list(sim_data=sim_data, end_placement=placement))
}

placement <- rep(x=5, times=25)
sim_data <- simulate_trips(sim_demand, placement)$sim_data
end_placement <- simulate_trips(sim_demand, placement)$end_placement


even_placement <- function(arrival_rates, k) {
  stations <- sort(union(arrival_rates$start_station, arrival_rates$end_station))
  n_stations <- length(stations)
  placement <- rep(x = k %/% n_stations, times = n_stations)
  names(placement) <- stations
  if (k%%n_stations != 0) {
    placement[1:(k%%n_stations)] <- placement[1:(k%%n_stations)]+1
  }
  return(placement)
}

even_placement(arrival_rates, 50)
even_place <- even_placement(arrival_rates, 110)
even_place

simulate_day <- function(arrival_rates, k, placement) {
  sim_data <- simulate_demand(arrival_rates)$sim_demand
  sim_data <- simulate_trips(sim_data, placement)$sim_data
  return(sim_data)
}

sim_data <- simulate_day(arrival_rates, 110, even_place)



random_placement <- function(arrival_rates, k) {
  stations <- sort(union(arrival_rates$start_station, arrival_rates$end_station))
  n_stations <- length(stations)
  placement <- rep(0, n_stations)
  names(placement) <- stations
  for (i in 1:k) {
    s <- as.character(sample(x=stations, size=1))
    placement[s] <- placement[s] + 1
  }
  return(placement)
}
place <- random_placement(arrival_rates, 110)
place
