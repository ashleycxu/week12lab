library(tidyverse)
set.seed(123)

source("simulation.R")

#' Add bikes
#' @description 
#' @param 
#' @return 
add_bikes <- function(arrival_rates, k, days) {
  
  # initialize empty data frame of simulated data
  sim_data <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(sim_data) <- c('start_station', 'end_station', 't', 'trip_possible')
  
  # simulate trips for each day, and append to previous days' data each time
  # start with even distribution of bikes
  even_place <- even_placement(arrival_rates, k)
  for (day in 1:days) {
    sim_day <- simulate_day(arrival_rates, k, even_place)
    sim_data <- full_join(sim_data, sim_day)
  }
  
  # estimate average number of happy/unhappy customers at each station
  satisfaction <- sim_data %>%
    group_by(start_station) %>%
    summarize(happy = sum(trip_possible)/days,
              unhappy = sum(!trip_possible)/days)
  
  # start with 0 bikes at each station
  placement <- even_placement(arrival_rates, 0)
  # iterate until we have placed all k bikes
  while (k > 0) {
    # place 1 bike at the most unhappy station
    satisfaction <- arrange(satisfaction, desc(unhappy))
    station <- satisfaction$start_station[1]
    placement[as.character(station)] <- placement[as.character(station)] + 1
    # update average # of happy/unhappy customers at that station
    satisfaction[satisfaction$start_station==station, ]$happy <- 
      satisfaction[satisfaction$start_station==station, ]$happy + 1
    satisfaction[satisfaction$start_station==station, ]$unhappy <- 
      max(0, satisfaction[satisfaction$start_station==station, ]$unhappy-1)
    # subtract 1 from # of bikes left to be placed
    k <- k-1
  }
  return(placement)
}

sim_day <- simulate_day(arrival_rates, 110)
sim_join <- full_join(sim_data, sim_day)

satisfaction1 <- sim_data %>%
  group_by(start_station) %>%
  summarize(happy = sum(trip_possible),
            unhappy = sum(!trip_possible)) %>%
  arrange(desc(unhappy))

satisfaction2 <- sim_day %>%
  group_by(start_station) %>%
  summarize(happy = sum(trip_possible),
            unhappy = sum(!trip_possible)) %>%
  arrange(desc(unhappy))

satisfaction <- sim_join %>%
  group_by(start_station) %>%
  summarize(happy = sum(trip_possible)/2,
            unhappy = sum(!trip_possible)/2) %>%
  arrange(desc(unhappy))


opt_place <- add_bikes(arrival_rates, 110, 5)
opt_place
add_bikes(arrival_rates, 110, 10)

opt_sim <- simulate_day(arrival_rates, 110, opt_place)

sum(sim_data$trip_possible) # 559 / 631
mean(sim_data$trip_possible) # 0.886
opt_trips <- simulate_trips(sim_data, opt_place)$sim_data
sum(opt_trips$trip_possible) # 542 / 631
mean(opt_trips$trip_possible) # 0.859

sum(sim_day$trip_possible) # 492 / 593
mean(sim_day$trip_possible) # 0.830
sum(simulate_trips(sim_day, opt_place)$sim_data$trip_possible) # 465 


swap_bikes <- function(arrival_rates, k, days, n_iter) {
  # initialize empty data frame of simulated data
  sim_data <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(sim_data) <- c('start_station', 'end_station', 't', 'trip_possible')
  
  # simulate trips for each day, and append to previous days' data each time
  # start with even distribution of bikes
  even_place <- even_placement(arrival_rates, k)
  for (day in 1:days) {
    sim_day <- simulate_day(arrival_rates, k, even_place)
    sim_data <- full_join(sim_data, sim_day)
  }
  
  # estimate average # of happy/unhappy customers and buffer at each station
  satisfaction <- sim_data %>%
    group_by(start_station) %>%
    summarize(happy = sum(trip_possible)/days,
              unhappy = sum(!trip_possible)/days)
  
  # start with even distribution of bikes
  placement <- even_placement(arrival_rates, k)
  # iterate until we have placed all k bikes
  for (i in 1:n_iter) {
    # determine most unhappy station and most happy station
    most_unhappy <- slice_max(satisfaction, order_by=unhappy, n=1)$start_station
    most_happy <- slice_max(satisfaction, order_by=happy, n=1)$start_station
    # move a bike from most happy station to most unhappy station
    placement[as.character(most_unhappy)] <- placement[as.character(most_unhappy)] + 1
    placement[as.character(most_happy)] <- placement[as.character(most_happy)] - 1
    # update average # of happy/unhappy customers at those stations
    satisfaction[satisfaction$start_station==most_unhappy, ]$unhappy <- 
      max(0, satisfaction[satisfaction$start_station==most_unhappy, ]$happy-1)
    satisfaction[satisfaction$start_station==most_happy, ]$happy <- 
      satisfaction[satisfaction$start_station==most_happy, ]$happy+1
  }
  return(placement)
}

swap_bikes(arrival_rates, 110, 5, 100)


