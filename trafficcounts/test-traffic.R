# Aim: test traffic counts data
library(tidyverse)
library(sf)
tc = read_csv("trafficcounts/trafficcounts.csv")
tc # 174 vars, 33k rows

# Explore linking variables (to assign to osm)
summary(as.factor(tc$road)) # 10k, 1/3rd
plot(tc$longitude, tc$latitude) # UK coverage


