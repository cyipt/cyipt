# PRep the TTWAs
library(sf)
library(stringr)

bounds <- st_read("../cyipt-bigdata/boundaries/TTWA/shape/Travel_to_Work_Areas_December_2011_Ultra_Generalised_Clipped_Boundaries_in_United_Kingdom.shp")
bounds$ttwa11cd <- as.character(bounds$ttwa11cd)
bounds <- bounds[substr(bounds$ttwa11cd,1,1) == "E",]

bounds <- st_transform(bounds,4326)

bounds$ttwa11nm <- as.character(bounds$ttwa11nm)
bounds$ttwa11nm <- str_replace_all(bounds$ttwa11nm,"[[:punct:]]","")
bounds$ttwa11nm <- str_replace_all(bounds$ttwa11nm," ","")

saveRDS(bounds,"../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")


