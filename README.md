F1 Racing EDA and Podium Predictions (In Progress)
================
Matthew Harris
11/11/2019

  - [Introduction](#introduction)
  - [Analysis Goals](#analysis-goals)
  - [R Shiny Dashboarding](#r-shiny-dashboarding)
  - [Data Sources](#data-sources)
  - [Data Import](#data-import)
  - [Data Wrangling/Cleansing](#data-wranglingcleansing)
      - [Data Inspection](#data-inspection)
      - [Data Cleansing](#data-cleansing)
      - [Data Validation](#data-validation)
      - [Joining the Tables](#joining-the-tables)

## Introduction

This will be used as a sample to display some of my analytical
capabilities in R. This project will demonstrate how useful R can be to
perform analysis that is easy to reproduce and communicate. This is by
no means an exhaustive demonstration of my proficiency with R, but
should highlight common data analysis functions that I perform
regularly.

## Analysis Goals

I have little to no knowledge about F1 racing. My only insight prior to
conducting this analysis is that Lewis Hamilton is one of the best
drivers to ever touch a steering wheel. My goal for this project is to
analyze the data to increase my knowledge of the sport and its
participants. After conducting my EDA I plan on creating a model that
can be used to predict a driver’s probability of finishing 3rd or higher
in their next race.

## R Shiny Dashboarding

Coming soon…

## Data Sources

The data used for this analysis contains a wide range of information of
information pertaining to F1 races from 1950 through 2017. The data is
currently separated into various tables. Part of this exercise will
require me to verify the accuracy of the data and combine the tables
into a more easy to interpret format. The data used for this analysis
can be found at the following Kaggle link. <br> [F1 Race
Data](https://www.kaggle.com/cjgdev/formula-1-race-data-19502017)

## Data Import

Loading necessary packages for analysis.

``` r
library(car)
library(tidyverse)
library(lubridate)
library(scales)
library(caret)
library(caretEnsemble)
```

Loading csv files containing the data.

``` r
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
```

## Data Wrangling/Cleansing

### Data Inspection

Now that the data is loaded I can begin to inspect it to determine if
any transformations will be needed before I can conduct further
analysis.

``` r
glimpse(drivers)
```

    ## Observations: 842
    ## Variables: 9
    ## $ driverId    <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1…
    ## $ driverRef   <chr> "hamilton", "heidfeld", "rosberg", "alonso", "kovala…
    ## $ number      <dbl> 44, NA, 6, 14, NA, NA, NA, 7, NA, NA, NA, NA, 19, NA…
    ## $ code        <chr> "HAM", "HEI", "ROS", "ALO", "KOV", "NAK", "BOU", "RA…
    ## $ forename    <chr> "Lewis", "Nick", "Nico", "Fernando", "Heikki", "Kazu…
    ## $ surname     <chr> "Hamilton", "Heidfeld", "Rosberg", "Alonso", "Kovala…
    ## $ dob         <chr> "07/01/1985", "10/05/1977", "27/06/1985", "29/07/198…
    ## $ nationality <chr> "British", "German", "German", "Spanish", "Finnish",…
    ## $ url         <chr> "http://en.wikipedia.org/wiki/Lewis_Hamilton", "http…

I will repeat this process with the other tables using the following
method.

``` r
map(list(circuits, constructor_results, constructors,
         constructor_standings, drivers, driver_standings,
         lap_times, pit_stops, pit_stops,
         qualifying, races, results,
         seasons, status), glimpse)
```

### Data Cleansing

As expected the data need to be cleaned. There are unneeded fields for
links to wikipedia pages, missing data in fields, redundant variables,
and datetime issues that need to be resolved. The next couple of
sections will focus on the transformations needed to clean the data.

``` r
results %>%
  #Removes the redundant positionText variable
  select(-positionText) %>%
  #Converts the fastestLapTime and race time variables to hms
  mutate(fastestLapTime = hms::as_hms(paste0("00:", str_sub(paste0(fastestLapTime), start = 1, end = 5))),
         f_time = hms::as_hms(as_datetime(milliseconds(milliseconds)))) %>%
  #Removes the updated variables
  select(-time, -milliseconds) -> results

drivers %>%
  #Removing the driverRef variable since it usually the same as the surname
  #Removing the number and code variables becuase they aren't available for all drivers
  select(-c(driverRef, number, code, url)) %>%
  #Converts the dob variable to date format
  mutate(dob = dmy(dob)) -> drivers

#Some of the circuit names and locations contain characters that weren't formatted correctly
#These update are used to correct the circuit name issues
circuits[18, 3] <- "Autódromo José Carlos Pace"
circuits[20, 3] <- "Nürburgring"
circuits[20, 4] <- "Nürburg"
circuits[4, 4] <- "Montmeló"
circuits[25, 3] <- "Autódromo Juan y Oscar Gálvez"
circuits[27, 3] <- "Autódromo do Estoril"
circuits[32, 3] <- "Autódromo Hermanos Rodríguez"
circuits[36, 3] <- "Autódromo Internacional Nelson Piquet"
circuits[49, 3] <- "Montjuïc"

#Removes unneeded variables
circuits %>%
  select(-c(circuitRef, lat, lng, alt, url)) -> circuits

#Removes unneeded variables
constructors %>%
  rename(c_name = name, c_nationality = nationality) %>%
  select(-c(constructorRef, url, X6)) -> constructors

#Renames the pit stop duration variable
pit_stops %>%
  mutate(ps_duration = seconds(duration)) %>%
  select(-c(duration, milliseconds)) -> pit_stops

#Updates the variable names and removes the url variable
races %>%
  rename(race_year = year, race_round = round, race_name = name, race_date = date,
         race_time = time) %>%
  select(-url) -> races

#Uses the milliseconds column to calculate the lap times in hms format
lap_times %>%
  mutate(l_time = hms::as_hms(as_datetime(milliseconds(milliseconds)))) %>%
  select(-c(time, milliseconds)) -> lap_times
```

The rest of the tables contain either unneeded or redudant information
that can be imputed by using other available information.

``` r
rm(constructor_results, constructor_results, constructor_standings, driver_standings, qualifying, seasons)
```

    ## Warning in rm(constructor_results, constructor_results,
    ## constructor_standings, : object 'constructor_results' not found

### Data Validation

The last step before combining the various tables is to validate the
accuracy of the information that they hold. A quick Wikipedia search
returns information that should assist with this process. Validating the
data allows me to have greated confidence in the accuracy of my analysis
and predictions. I can also use the statistics that I am going to
calculate to check my data once it’s joined. <br>

First up are the `results` and `drivers` tables. I’m choosing to exmaine
a driver who was retired before the last date that the data was
collected. That way I can confirm if the data capture all necessary win
statistics to calculate a variety of driver stats.

``` r
results %>%
  left_join(drivers, by = "driverId") %>%
  filter(surname %in% c("Prost", "Schumacher"), 
         forename %in% c("Alain", "Michael")) %>%
  group_by(forename, surname) %>%
  mutate(win = if_else(position == 1, 1, 0),
         podium = if_else(position %in% c(1:3), 1, 0),
         #Total career wins
         career_wins = sum(win, na.rm = TRUE), 
         #Total career podiums(placed 3rd of higher)
         career_podium = sum(podium, na.rm = TRUE),
         #Total career points(including non Championship points)
         total_points = sum(points, na.rm = TRUE)) %>%
  select(forename, surname, career_wins, career_podium, total_points) %>%
  distinct()
```

    ## # A tibble: 2 x 5
    ## # Groups:   forename, surname [2]
    ##   forename surname    career_wins career_podium total_points
    ##   <chr>    <chr>            <dbl>         <dbl>        <dbl>
    ## 1 Michael  Schumacher          91           155        1566 
    ## 2 Alain    Prost               51           106         798.

``` r
results %>%
  select(raceId, driverId, constructorId) %>%
  left_join(drivers, by = "driverId") %>%
  left_join(constructors, by = "constructorId") %>%
  filter(driverId %in% c(30, 117)) %>%
  group_by(forename, surname) %>%
  distinct(name) %>%
  arrange(surname)
```

    ## Warning: Trying to compute distinct() for variables not found in the data:
    ## - `name`
    ## This is an error, but only a warning is raised for compatibility reasons.
    ## The operation will return the input unchanged.

    ## # A tibble: 2 x 2
    ## # Groups:   forename, surname [2]
    ##   forename surname   
    ##   <chr>    <chr>     
    ## 1 Alain    Prost     
    ## 2 Michael  Schumacher

So far so good. The values for the `career_wins`, `career_podium`, and
`total_points` variables all match the values found on wikipedia for the
drivers shown below. I have also confirmed the various constructors that
each driver raced for during their career. I’ll repeat this process for
other variables in the remaining tables.

<img src="Project Images/driver_stats.png" width="550px" /> <br>

Digging a little deeper into the `lap_times` and `races` tables reveal
that the laptime data is only available for races after March, 3rd 1996.
There are some other tables that are missing data before a certain date.
Even so there is still enough complete data to gain some interesting
insights.

``` r
lap_times %>%
  left_join(races, by = "raceId") %>%
  summarize(oldest_race = min(race_date)) 
```

    ## # A tibble: 1 x 1
    ##   oldest_race
    ##   <date>     
    ## 1 1996-03-10

### Joining the Tables

The final step before I can begin analyzing and visualizing the data is
to join all of these tables into a master table. This will also make
model creation creation and testing easier.
