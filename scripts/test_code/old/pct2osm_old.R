#Add PCT values to OSM lines

library(sf)
library(sp)
library(raster)
library(rgeos)
library(rgdal)

source("R/functions.R")

#Read in lines and raster and set
lines <- readRDS("../example-data/bristol/osm-lines-quietness-full.Rds")
#lines <- lines[1:1000,]
lines_sp <- as(lines,"Spatial")
lines_sp <- spTransform(lines_sp,CRS("+init=epsg:27700"))
lines_seg <- SegmentSpatialLines(lines_sp, length = 100, merge.last = FALSE)
plot(lines_sp)
raster <- raster("../example-data/pct/census-all.tif")
raster <- crop(raster,extent(lines_sp))
writeRaster(raster,"../example-data/bristol/pct-cenus.tif", "GTiff", overwrite = T)
ext <- extract(raster,lines_sp, buffer = 2.5, fun = mean, sp = T)
writeOGR(ext,"../example-data/bristol","pct-cenus_lines", driver = "ESRI Shapefile", overwrite_layer = T)
saveRDS(ext,"ext.Rds")


