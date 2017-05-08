#Read in OSM Lines
#Can't figure out how to split lines at each intersection in R so did it in ArcGIS
osm <- st_read(dsn ="../example-data/bristol",layer = "osm_split")
osm <- st_transform(osm, 27700) #Change to British Nat Grid
osm <- osm[boundary,] #35257
osm <- osm[,c("osm_id","name","geometry")]
osm <- osm[st_is_valid(osm),] #remove invalid geometry #18944
#remove(boundary)

#Add in a unique idea for each segment of and OSM line
osm$id <- c(1:nrow(osm))
osm$width <- NA
osm$widthpath <- NA

#Save Results
saveRDS(osm, "../example-data/bristol/osm_data/roads_osm.Rds")
