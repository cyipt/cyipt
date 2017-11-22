# PRep the TTWAs
library(sf)
library(stringr)
library(dplyr)

bounds <- st_read("../cyipt-bigdata/boundaries/TTWA/shape/full extent/Travel_to_Work_Areas_December_2011_Full_Extent_Boundaries_in_United_Kingdom.shp")
bounds$ttwa11cd <- as.character(bounds$ttwa11cd)
bounds <- st_transform(bounds, 27700)
bounds <- st_simplify(bounds, dTolerance = 10)

bounds <- bounds[substr(bounds$ttwa11cd,1,1) == "E" |bounds$ttwa11nm %in% c("Carlisle","Chester","Oswestry","Berwick"),]

bounds$ttwa11nm <- as.character(bounds$ttwa11nm)
bounds$ttwa11nm <- str_replace_all(bounds$ttwa11nm,"[[:punct:]]","")
bounds$ttwa11nm <- str_replace_all(bounds$ttwa11nm," ","")

bounds <- st_transform(bounds, 4326)

saveRDS(bounds,"../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")


