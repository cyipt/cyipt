#Prepare OS and OSM Data
library(sf)
library(dplyr)

#Before Running Check and Repair Geometry of OS data using ArcGIS Repair Geometry Tool
#Before runing Delete duplicate geometrys

#Read in data drop unneded values
boundary <- st_read(dsn = "areas/bristol-poly.geojson")
boundary <- st_transform(boundary, 27700) #Change to British Nat Grid
os <- st_read(dsn = "D:/roadwidth", layer = "SWroads") #1118955
os <- os[,c("OBJECTID","DESCGROUP","geometry")] #Dump Uneeded Columns
os <- st_transform(os, 27700) #Change to British Nat Grid
os <- os[boundary,] #36592 lines
os <- os[st_is_valid(os),] #remove invalid geometry #36530
os <- st_cast(os, "POLYGON", group_or_split = TRUE) #Convert Mulipolgon to Polygon

#Save Results
saveRDS(os, "../example-data/bristol/os_data/roads.Rds")
