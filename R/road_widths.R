#Estimate road widths
library(sf)
library(geosphere)
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
os <- os[os$DESCGROUP == "Road Or Track",] #Remove Paths and Pavements (for now) #540884
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

nrow(osm) == nrow(osm_dup) + nrow(osm_nodup)
osm_dup_old <- osm_dup
#Loop THough an check
for(d in 1:nrow(osm_dup)){
  osmid <- osm_dup$osm_id[d]
  osm_sub <- osm_dup[osm_dup$osm_id == osmid,]
  osids <- osm_dup$OBJECTID[osm_dup$osm_id == osmid]
  os_sub <- os[os$OBJECTID %in% osids,]
  plot(os_sub[1], col = "White")
  plot(osm_sub[1], add = T)
  osm_inter <- st_intersection(osm_sub[1,],os_sub)
  osm_inter$length <- st_length(osm_inter)
  #totlength <- as.numeric(st_length(osm_sub[1,]))
  #os_id_keep <- osm_inter$OBJECTID.1[osm_inter$length == max(osm_inter$length)]
  os_id_rem <- osm_inter$OBJECTID.1[osm_inter$length != max(osm_inter$length)]
  keep <- (osm_dup$OBJECTID != os_id_rem)
  osm_dup <- osm_dup[keep,]
  rm(osmid, osm_sub, osids,os_sub,osm_inter,os_id_rem,keep)
}





osm_union <- st_union(osm_sub[1,],os_sub)
osm_diff <- st_difference(osm_sub[1,],os_sub)
osm_inter <- st_intersection(osm_sub[1,],os_sub)
osm_dist <- st_disjoint(osm_sub[1,],os_sub)

osm_diff2 <- st_difference(osm_diff[1,],osm_diff[2,])

plot(osm_union[1], col = "Black")
plot(osm_diff[1], col = "Blue", add = T)
plot(osm_inter[1], lwd = 3)
plot(osm_sub[1], add = T)
plot(os_sub, add = T)
plot(osm_diff2, lwd = 5, add = T)


plot(osm_sub[1,1], col = "Black")
plot(os_sub[1], col = "White", add = T)
plot(osm_diff[2,1], add = T, lwd = 15, col = "Blue")
plot(osm_diff[1,1], add = T, lwd = 10, col = "Pink")
plot(osm_inter[1,1],add = T, col = "Red", lwd = 4)
plot(osm_inter[2,1],add = T, col = "Green", lwd = 2 )
plot(osm_diff2, add = T, col = "Black")






plot(osm_diff[2,1], add = T, lwd = 2, col = "Red")
plot(osm_diff[1,1], add = T, lwd = 2, col = "Red")

#Clean out NA from OBJECTID
for(b in 1:nrow(osm_1)){
  osm_1$OBJECTID[b] <- if(is.na(osm_1$OBJECTID[b])){0}else{osm_1$OBJECTID[b]}
}

#Look for unique matches
osm_1$nmatch <- NULL
for(a in 1:nrow(osm_1)){
  osm_1$nmatch[a] <- nrow(osm_1[osm_1$OBJECTID == osm_1$OBJECTID[a],])
}
#osm_1$minmatch <- NULL
#for(c in 1:nrow(osm_1)){
#  osm_1$minmatch[c] <- if(osm_1$nmatch[c] == min(osm_1$nmatch[osm_1$id == osm_1$id[c]])){TRUE}else{FALSE}
#}


osm_union <- st_union(osm,os)


dup <- duplicated(osm_1$id)
summary(dup)
diff <- osm_1[dup,]
qtm(diff)


id = 122
ids = osm_1$OBJECTID[osm_1$id == id]
id_other = osm_1$id[osm_1$OBJECTID %in% ids]
ids_other = osm_1$OBJECTID[osm_1$id %in% id]
plot(os[os$OBJECTID %in% ids,"OBJECTID"], col = "White")
plot(os[os$OBJECTID %in% ids_other,"OBJECTID"], col = "blue")
plot(osm_1[osm_1$id == id,"osm_id"], add = T, lwd = 3)
plot(osm_1[osm_1$id %in% id_other,"osm_id"], add = T, col = "Red")
test <- osm_1[osm_1$id == id,]
#qtm(osm_1[osm_1$id == id,"osm_id"])
osm_1$nmatch[osm_1$osm_id == id]

test2 <- osm_1[osm_1$nmatch == 1,]

qtm1 <- qtm(os)
qtm2 <- qtm(osm_1)
tmap_arrange(qtm1,qtm2)




#plot(os[os$OBJECTID %in% id_mean,"OBJECTID"], col = "Red", add = T)
#plot(osm_2[osm_2$osm_id == id,"osm_id"], add = T, col = "Blue")

#Match Lines with Polygons
osm

osmid <-3508489
ids <- osm_old$OBJECTID[osm_old$osm_id == osmid]
#id <- 306640
plot(os[os$OBJECTID %in% ids,"OBJECTID"], col = "White")
plot(osm_old[osm_old$osm_id == osmid,"osm_id"], add = T, col = "Red")
