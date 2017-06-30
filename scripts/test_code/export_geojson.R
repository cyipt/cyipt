library(sf)
library(sp)
library(rgdal)

cents <- readRDS("../pct-lsoa/Data/02_Input/LSOA_cents_mod.Rds")
cents <- spTransform(cents, CRS("+init=epsg:27700"))
cents <- st_as_sf(cents)

bounds <- st_read("../cyipt/areas/bristol-poly.geojson")
bounds <- st_transform(bounds, 27700)

st_crs(cents)  <- st_crs(bounds)
cents <- cents[bounds,]

cents <- st_transform(cents, 4326)
cents$id <- 1:nrow(cents)
cents <- cents[,c("id","code")]

st_write(cents, "../example-data/bristol/LSOA/testcentroids5.geojson")







cents2 <- as(cents,"Spatial")
writeOGR(obj = cents2, dsn = "../example-data/bristol/LSOA/", layer = "test", driver = "GeoJSON")

library(geojsonio)
geojson_write(cents,file = "../example-data/bristol/LSOA/testcentroids6.geojson")
