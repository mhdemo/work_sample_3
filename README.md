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
```
