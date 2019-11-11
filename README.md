F1 Racing EDA and Podium Predictions (In Progress)
================
Matthew Harris
11/11/2019

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
currently separated into various tables. Part of this excerise will
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
