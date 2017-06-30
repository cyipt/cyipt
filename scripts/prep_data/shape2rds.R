#Shapefile to R

library(sf)
shp <- st_read("../cyipt-bigdata/boundaries/local_authority/Local_Authority_Districts_December_2016_Generalised_Clipped_Boundaries_in_Great_Britain.shp")
shp <- st_transform(shp, 27700)
saveRDS(shp,"../cyipt-bigdata/boundaries/local_authority/local_authority.Rds")

