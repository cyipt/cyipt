#Add PCT values to OSM lines
#Quite slow about 1h for bristol

library(sf)
library(sp)
library(raster)
library(rgeos)
library(rgdal)

#Setting to boost performance
limit = 200 #Maximum number of lines to be done at once
rasterOptions(maxmemory = 1e+10) #10+8 is default
rasterOptions(maxmemory = 1e+09)
rasterOptions(tmpdir = "F:/RasterTmp")

#Read in Data
osm <- readRDS("../example-data/bristol/osm_data/roads_osm.Rds")
osm <- as(osm, "Spatial") #Raster does not work with sf so converte back to SP
pct <- raster("../example-data/pct/census-all.tif")
pct <- crop(pct,extent(osm))
ext <- extract(pct,osm, buffer = 2.5, fun = mean, sp = T)
ext <- st_as_sf(ext)
ext$census.all <- floor(ext$census.all)

saveRDS(ext,"../example-data/bristol/osm_data/roads_osm_pct_2011.Rds")
