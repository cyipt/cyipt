#Get Traffic counts
library(sf)
library(dplyr)


# Read in Data
osm <- readRDS("../cyipt-bigdata/osm-clean/BristolCityof/osm-lines.Rds")
traffic.lines <- readRDS("../cyipt-bigdata/traffic/trafficlines.Rds")
traffic.points <-  readRDS("../cyipt-bigdata/traffic/trafficpoints.Rds")

#dump unneded columns
traffic.lines <- traffic.lines[,c("road","aadt","cycles")]
traffic.points <- traffic.points[,c("road","aadt","cycles")]

#Get bounding box
ext <- st_bbox(osm)
ext <- st_sfc(st_polygon(list(rbind(c(ext[1],ext[2]),c(ext[3],ext[2]),c(ext[3],ext[4]),c(ext[1],ext[4]),c(c(ext[1],ext[2]))))) )
pol <- data.frame(id = 1, geometry = NA)
st_geometry(pol) <- ext
st_crs(pol) <- 27700
rm(ext)

#Subset Traffic data to bounding box
traffic.lines <- traffic.lines[st_intersects(pol,traffic.lines)[[1]],]
traffic.points <- traffic.points[st_intersects(pol,traffic.points)[[1]],]

#start with the lines
lines.buff <- st_buffer(traffic.lines, 100) #buffer out 100m
osm_sub <- osm[osm$ref %in% traffic.lines$road, ] #get the bits of the OSM that have the same name as the traffic data e.g. M25, A6

inter <- st_intersects(lines.buff,osm_sub)

get.aadt.lines <- function(b){
  #Get the osm lines that overlap the buffer and have the same name
  osm.inter <- osm_sub[inter[[b]],]
  osm.inter <- osm.inter[osm.inter$ref == lines.buff$road[b],]
  if(nrow(osm.inter) == 0){
    res <- NULL #for when no data is returned
  }else{
    res <- data.frame(osm_id = osm.inter$osm_id , aadt = traffic.lines$aadt[b], cycles = traffic.lines$cycles[b])
  }
  return(res)
}

lines.aadt <- lapply(1:nrow(traffic.lines),get.aadt.lines)
lines.aadt <- do.call(rbind,lines.aadt)

#Join onto the original osm data
osm <- left_join(osm,lines.aadt, by = c("osm_id" = "osm_id"))




#No do the points
points.buff <- st_buffer(traffic.points, 20)
inter.points <- st_intersects(points.buff,osm)

get.aadt.points <- function(b){
  #Get the osm lines that overlap the buffer and have the same name
  osm.inter <- osm[inter.points[[b]],]
  qtm(inter.points[[b]])
  #osm.inter <- osm.inter[osm.inter$ref == points.buff$road[b],]
  if(nrow(osm.inter) == 0){
    res <- NULL #for when no data is returned
  }else{
    res <- data.frame(osm_id = osm.inter$osm_id , aadt.p = traffic.lines$aadt[b], cycles.p = traffic.lines$cycles[b])
  }
  return(res)
}

points.aadt <- lapply(1:nrow(traffic.points),get.aadt.points)
points.aadt <- do.call(rbind,points.aadt)


#Join onto the original osm data
osm <- left_join(osm,points.aadt, by = c("osm_id" = "osm_id"))
#clean up two columns
osm$aadt[is.na(osm$aadt)] <- osm$aadt.p[is.na(osm$aadt)]
osm$cycles[is.na(osm$cycles)] <- osm$cycles.p[is.na(osm$cycles)]
osm$aadt.p <- NULL
osm$cycles.p <- NULL


#Plot
library(tmap)
tmap_mode("view")
test <- osm[!is.na(osm$aadt),]
qtm(test, lines.lwd = 10, lines.col = "aadt")


