#Estimate road widths
library(sf)
library(geosphere)
library(dplyr)
#library(rgrass7)
#library(tmap)


#Before Running Check and Repair Geometry of OS data using ArcGIS Repair Geometry Tool
#Before runing Delete duplicate geometrys
#Before running convert mulipart to single part
#tmap_mode("view")
#Read in data drop unneded values
boundary <- st_read(dsn = "areas/bristol-poly.geojson")
boundary <- st_transform(boundary, 27700) #Change to British Nat Grid
os <- st_read(dsn = "D:/roadwidth", layer = "SWroads") #1118955
os <- os[,c("OBJECTID","DESCGROUP","geometry")] #Dump Uneeded Columns
os <- os[os$DESCGROUP == "Road Or Track" | os$DESCGROUP == "Path",] #Remove Paths and Pavements (for now) #540884
os <- st_transform(os, 27700) #Change to British Nat Grid
os <- os[boundary,] #18985 lines
os <- os[st_is_valid(os),] #remove invalid geometry #18944

#Estimate the width of each polygon
#From http://gis.stackexchange.com/questions/20279/how-can-i-calculate-the-average-width-of-a-polygon
os$area <- as.numeric(st_area(os))
os$perimeter <- as.numeric(st_length(os, dist_fun = geosphere::distGeo))
os$width <- 2 * os$area / os$perimeter

#Calcualte Squarness
#Asumming that lenght = x * width, x is a measure of how square a retangle is
#Squares are usually at junctions not roads of road
os$squareness <- ifelse((os$perimeter ^ 4) <= (16 * os$area * (os$perimeter ^ 2)),0,(sqrt((os$perimeter ^ 4) - (16 * os$area * (os$perimeter ^ 2))) - (8 * os$area) + (os$perimeter ^ 2)) / (8 * os$area) )
summary(is.nan(os$squareness))
os <- os[os$squareness >= 2,] #Dump square(ish) polygons #13494
summary(is.na(os$OBJECTID))
summary(st_is_valid(os))
summary(st_is_simple(os))

#Drop Unneded Columns
os$squareness <- NULL
os$area <- NULL
os$perimeter <- NULL
os$DESCGROUP <- NULL

#Read in OSM Lines
osm <- readRDS("../example-data/bristol/osm-lines-quietness-full.Rds")
osm <- osm[,c("osm_id","name","geometry")] #Dum unneeded columns

osm <- st_transform(osm, 27700) #Change to British Nat Grid
osm <- osm[boundary,]

####################################################
#Can't figure out how to split lines at each intersection in R so did it in ArcGIS
#st_write(osm,"../example-data/bristol","osm_orig", driver = "ESRI Shapefile")
osm <- st_read(dsn ="../example-data/bristol",layer = "osm_split")#, driver = "ESRI Shapefile") #35311
osm <- st_transform(osm, 27700) #Change to British Nat Grid
osm <- osm[boundary,] #35257
osm <- osm[,c("osm_id","name","geometry")]
####################################################
remove(boundary)
gc()

summary(st_is_valid(osm))
summary(st_is_simple(osm))
osm$id <- c(1:nrow(osm))

#Remove short stubs from OSM data
#osm$length <- as.numeric(st_length(osm))
#osm <- osm[osm$length > 20,] #24702 lines

#Intersection the OSM with the OS
osm <- st_join(osm,os, join = st_intersects, left = TRUE) #30117
osm$id2 <- c(1:nrow(osm))
#osm_2 <- st_join(osm,os, join = st_intersects, FUN = mean, left = TRUE) #35257
summary(st_is_valid(osm))
summary(st_is_simple(osm))

dup <- duplicated(osm$id)
dupid <- osm$id[dup]

osm_dup <- osm[osm$id %in% dupid,]
osm_nodup <- osm[!(osm$id %in% dupid),]

nrow(osm) == nrow(osm_dup) + nrow(osm_nodup) #Check that we haven't lost any lines
rm(dup, dupid)
rownames(osm_dup) <- c(1:nrow(osm_dup))
dolist <- as.integer(rownames(osm_dup[!duplicated(osm_dup$id),]))
#osm_dup_old <- osm_dup
#Loop THough an check

########################### This loop is unfinished
#Basic idea find the overlapping polygons and get the lenght of the line in each polygon, then take the longest line and drop the others
for(d in dolist){
  #Get the relavant OSM lines and OS polygons
  osmid <- osm_dup$id[d]
  osm_sub <- osm_dup[osm_dup$id == osmid,]
  osm_sub <- osm_sub[1,] #Only need one copy of the line
  osids <- osm_dup$OBJECTID[osm_dup$id == osmid]
  os_sub <- os[os$OBJECTID %in% osids,]
  #plot(os_sub[1], col = "White")
  #plot(osm_sub[1], add = T)

  #Find intersections
  os_sub_str <- st_cast(os_sub, "MULTILINESTRING", group_or_split=TRUE)
  osm_inter <- st_intersection(osm_sub,os_sub_str)
  osm_inter <- osm_inter$geoms
  #plot(osm_inter, add = T, col = "Blue")

  #Split Points and Mulitpoints
  pORmp <- vector(mode = "logical",length = length(osm_inter))
  for(e in 1:length(osm_inter)){
    pORmp[[e]] <- any(class(osm_inter[[e]]) == "MULTIPOINT")
  }
  osm_inter_mp <- osm_inter[pORmp]
  osm_inter_p <- osm_inter[!pORmp]
  rm(pORmp, osm_inter,os_sub_str)

  #Convert Multipoints into single points
  osm_inter_mp <- st_cast(st_sfc(osm_inter_mp), "POINT", group_or_split = TRUE)
  osm_inter_p <- st_cast(st_sfc(osm_inter_p), "POINT", group_or_split = TRUE)

  #Put points back togther
  osm_inter <- c(osm_inter_p,osm_inter_mp)
  osm_inter <- st_cast(st_sfc(osm_inter), "POINT", group_or_split = TRUE) #Incase mp or p is empty have to run again

  #Remove Duplicates
  inter_dup <- duplicated(osm_inter)
  osm_inter <- osm_inter[!inter_dup]
  inter_df <- data.frame(id = c(1:length(osm_inter)))
  inter_df$geom <- osm_inter
  inter_df <- st_sf(inter_df)
  rm(osm_inter,osm_inter_p,osm_inter_mp,inter_dup)

  #Buffer Pointsand make into a singe mulipolygon
  osm_buff <- st_buffer(inter_df, dist = 1)
  buff_geom <- osm_buff$geom
  buff_geom <- st_union(buff_geom)
  #plot(buff_geom, add = T, col = "Red")
  rm(osm_buff, inter_df)

  #Cut the line with buffered points
  osm_diff <- st_cast(st_difference(osm_sub,buff_geom), "LINESTRING")
  osm_diff$length <- as.numeric(st_length(osm_diff, dist_fun = geosphere::distGeo))
  #plot(osm_diff["length"], add = T)

  #Select the right segment of the line
  osm_right <- osm_diff[osm_diff$length == max(osm_diff$length),]
  osm_right <- osm_right[,c("osm_id","id","id2","geometry","length")]
  #plot(osm_right, add = T, col = "Green", lwd = 3)
  osm_join <- st_join(osm_right,os_sub, join = st_intersects, left = TRUE)

  #Update Table
  osm_dup$width[osm_dup$id == osmid] <- osm_join$width[1]
  rm(osm_join,osm_right,os_sub,osm_diff,osm_sub,buff_geom)

}

osm_dupdup <- duplicated(osm_dup$id)
osm_duprem <- osm_dup[!osm_dupdup,]
osm_clean <- rbind(osm_nodup, osm_duprem)


plot(missing[1:1000,], col = "red")
plot(os, col = "White", add = T)
