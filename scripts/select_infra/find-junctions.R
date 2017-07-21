# Aim: find junctions in Bristol

library(sf)
library(dplyr)
osm_data = readRDS("../example-data/bristol/osm-all-highways.Rds")
ways = osm_data$osm_lines
points = osm_data$osm_points
jp = filter(points, !is.na(junction))
jw = filter(ways, !is.na(junction))

library(tmap)
tmap_mode("view")
qtm(jw)
