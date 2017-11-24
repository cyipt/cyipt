# Prep PCT Stright Lines
library(sf)
library(stplanr)
library(tmap)
tmap_mode("view")

pct <- readRDS("../cyipt-securedata/pct-routes-all.Rds")
pct <- pct[,c("lsoa1","lsoa2","ID")]
pct <- as.data.frame(pct)
pct$geometry <- NULL

cents <- st_read("../cyipt-bigdata/centroids/LSOA/Lower_Layer_Super_Output_Areas_December_2011_Population_Weighted_Centroids.shp")
cents <- cents[,c("lsoa11cd")]

lines <- od2line(flow = pct, zones = cents)
lines <- st_transform(lines, 27700)
lines$length <- as.numeric(st_length(lines))

lines <- lines[,c("ID","length")]
qtm(lines[1:5,])

saveRDS(lines,"../cyipt-securedata/pct-lines-all.Rds")
