library(car)
library(tidyverse)
library(lubridate)
library(caret)
library(caretEnsemble)

#Loads data ####
circuits <- read_csv("Raw Data/circuits.csv")
constructor_results <- read_csv("Raw Data/constructorResults.csv")
constructors <- read_csv("Raw Data/constructors.csv")
constructor_standings <- read_csv("Raw Data/constructorStandings.csv")
drivers <- read_csv("Raw Data/drivers.csv")
driver_standings <- read_csv("Raw Data/driverStandings.csv")
lap_times <- read_csv("Raw Data/lapTimes.csv")
pit_stops <- read_csv("Raw Data/pitStops.csv")
qualifying <- read_csv("Raw Data/qualifying.csv")
races <- read_csv("Raw Data/races.csv")
results <- read_csv("Raw Data/results.csv")
seasons <- read_csv("Raw Data/seasons.csv")
status <- read_csv("Raw Data/status.csv")

#Examine data structure ####
map(list(circuits, constructor_results, constructors,
         constructor_standings, drivers, driver_standings,
         lap_times, pit_stops, pit_stops,
         qualifying, races, results,
         seasons, status), str)

#Each some interesting data that can be joined to a master table. Before I can really dig into my analysis I want to join as much of the
#data that makes sense to a master table. I'm choosing to start with the driver_standing table as it should contain the most unique
#entries for driver activity for a given race. Before doing that I am going to calcucalte some key statistic for a couple of drivers,
#races, constructors, and circuits. Calculating these statistic prior to and after joinging the data will allow me to check that my
#was completed properly

#First up to compare known race stats to the data that we have. 
#I'm choosing to exmaine a driver who was retired before the last date of our data.
#That way we can confirm if our data captures all necessary win statistics to calculate a variety of driver stats

#Total career wins
results %>%
  left_join(drivers, by = "driverId") %>%
  filter(surname %in% c("Prost", "Schumacher"), 
         forename %in% c("Alain", "Michael"), 
         position == 1) %>%
  group_by(forename, surname) %>%
  count()

#Total career podiums(placed 3rd of higher)
results %>%
  left_join(drivers, by = "driverId") %>%
  filter(surname %in% c("Prost", "Schumacher"), 
         forename %in% c("Alain", "Michael"),
         position %in% c(1:3)) %>%
  group_by(forename, surname) %>%
  count()

#Total career points(including non Championship points)
results %>%
  left_join(drivers, by = "driverId") %>%
  filter(surname %in% c("Prost", "Schumacher"), 
         forename %in% c("Alain", "Michael")) %>%
  group_by(forename, surname) %>%
  summarize(total_points = sum(points))

#The max wins from the driver_standings table should return the same value as the count of 1st place positions
#in the results table. I also tried to recalcuate the wins by taking the count of 1st place positions in the
#drivers_standings table. Further information may be needed to understand what the values in the driver_standings table represent
#but I will remove this table for now since we can use the results table to calculcat the same information
#I will have to add a running total column to the results table to calculate the cummalitive wins for each driver for each race
driver_standings %>%
  left_join(drivers, by = "driverId") %>%
  filter(surname == "Prost") %>%
  summarize(total_wins = max(wins))

driver_standings %>%
  left_join(drivers, by = "driverId") %>%
  filter(surname == "Prost", position == 1) %>%
  count()


driver_standings %>%
  left_join(drivers, by = "driverId") %>%
  filter(driverId %in% c(1, 5, 90, 55, 102)) %>%
  group_by(driverId, surname, forename) %>%
  summarize(total_wins = max(wins))

results %>%
  left_join(drivers, by = "driverId") %>%
  filter(surname == "Prost", position == 1)

driver_standings %>%
  left_join(drivers, by = "driverId") %>%
  left_join(races, by = "raceId") %>%
  filter(driverId %in% c(1, 2, 5, 90, 55) | str_detect(surname, "Senna"),
         str_detect(name, "Grand Prix")) %>%
  group_by(driverId, surname, forename) %>%
  mutate(win_ind = if_else(position == 1, 1, 0),
         cumm_wins = cumsum(win_ind)) %>%
  summarize(total_wins = max(cumm_wins))
  
driver_standings %>%
  left_join(races, by = "raceId") %>%
  arrange(date)



#My first goal is to join together all the variables that can be associated with a single race. After I have combined these I will examine
#the other tables that contain multiple data points for a singe race and determine the best way to aggregate that data into single race
#metrics

driver_standings %>%
  rename(d_position = position,
         d_points = points,
         d_wins = wins) %>%
  select(-driverStandingsId, -positionText) %>%
  left_join(races %>%
              rename(start_time = time,
                     race_name = name) %>%
              select(-url), 
            by = "raceId") %>%
  left_join(results %>%
              select(raceId, driverId, constructorId, grid,
                     laps, time, milliseconds, fastestLap,
                     fastestLapTime, fastestLapSpeed,
                     statusId),
            by = c("raceId", "driverId")) %>%
  left_join(status, 
            by = "statusId") %>%
  left_join(circuits %>%
              rename(circuit_name = name) %>%
              select(-url),
            by = "circuitId") %>%
  left_join(drivers %>%
              select(-url, -code, -number, -driverRef) %>%
              rename(d_nationality = nationality),
            by = "driverId") %>%
  left_join(constructors %>%
              select(constructorId, name, nationality) %>%
              rename(c_nationality = nationality),
            by = "constructorId") %>%
  left_join(constructor_standings %>%
              select(raceId, constructorId, points, position, wins) %>%
              rename(c_points = points, c_position = position, c_wins = wins),
            by = c("raceId", "constructorId")) -> master_table

master_table %>%
  filter(surname == "Hamilton", forename == "Lewis", d_position == 1) %>%
  select(date, d_wins, d_position, driverId) %>%
  summarize(max(d_wins))

driver_standings %>%
  filter(driverId %in% c(1, 5, 90, 55)) %>%
  group_by(driverId) %>%
  summarize(total_wins = max(wins))

driver_standings %>%
  filter(driverId == 1) %>%
  distinct(wins)

results %>%
  rename(results_time = time,
         d_position = position,
         d_points = points) %>%
  filter(driverId == 1, d_position == 1)
  
results %>%
  filter(driverId == 1, raceId == 18) %>%
  select(fastestLapTime)

#Needed to convert the time column to a proper hms formatting. Was orginally recording minutes as hours
lap_times %>%
  filter(driverId == 1, raceId == 18) %>%
  mutate(m_time = paste0("00:", str_sub(paste0(time), start = 1, end = 5))) %>%
  mutate_at("m_time", hms::as_hms)
  arrange(m_time)

  summarize(seconds_to_period(min(time)))

seconds_to_period(milliseconds(87785))

lap_times %>%
  filter(driverId == 1, raceId == 18) %>%
  slice(1) %>%
  pull(time) %>%
  str()
    
  paste0() %>%
  str_sub(start = 1, end = 5) %>%
  ms()

?time

str(hms("00:05:23"))

lap_times %>%
  as_tibble() %>%
  filter(raceId == 300)
