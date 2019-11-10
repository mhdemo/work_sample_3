library(car)
library(tidyverse)
library(lubridate)
library(caret)

#Goals of using student attributes to predict their interest in higher education

#Loads data ####
students <- read_csv("Raw Data/student-por.csv")

#Inspect the data ####
summary(students)

#

students %>%
  group_by(higher) %>%
  summarize(sum(absences) / n())
  
  ggplot(aes(higher, n, fill = factor(Pstatus))) + geom_col(position = "fill")




