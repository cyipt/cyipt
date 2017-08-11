# Prepare Traffic Data
library(sf)

traffic <- read.csv("trafficcounts/trafficcounts.csv", stringsAsFactors = F, sep = ",", quote = '"')

#Change Data types to numeric
col.number <- names(traffic)[!(names(traffic) %in% c("westminstername","boroughname","wardname","road","road_type","road_geom"))]

for(a in 1:length(col.number)){
  traffic[,col.number[a]] <- as.numeric(traffic[,col.number[a]])
}
rm(col.number)

#Remove junk columns
traffic$westminstername <-NULL
traffic$boroughname  <- NULL
traffic$wardname <- NULL
traffic$road_geom <- NULL

traffic.lines <- traffic[!is.na(traffic$finishLatitude),]
row.names(traffic.lines) <- 1:nrow(traffic.lines)
traffic.points <- traffic[is.na(traffic$finishLatitude),]
row.names(traffic.points) <- 1:nrow(traffic.points)
rm(traffic)

#remove more junk
traffic.points$startLatitude  <- NULL
traffic.points$startLongitude  <- NULL
traffic.points$finishLatitude  <- NULL
traffic.points$finishLongitude  <- NULL

#make points into spatial objects

points.geom <- st_sfc(st_multipoint(cbind(traffic.points$longitude, traffic.points$latitude)))
points.geom <- st_cast(points.geom, "POINT")
traffic.points <- st_sf(traffic.points, geometry = points.geom, crs = 4326)
rm(points.geom)

#make lines into spatial objects
linelist = vector(mode = "list", length = nrow(traffic.lines))
for(i in 1:length(linelist)) {
  linemat = matrix(c(traffic.lines$startLongitude[i], traffic.lines$startLatitude[i],
                     traffic.lines$finishLongitude[i], traffic.lines$finishLatitude[i]), ncol = 2, byrow = TRUE)
  linelist[[i]] = st_linestring(x = linemat)
}
lines.geom = st_sfc(linelist)
traffic.lines <- st_sf(traffic.lines, geometry = lines.geom, crs = 4326)
rm(i,linemat,lines.geom,linelist)

#Transform to British national grid
traffic.lines <- st_transform(traffic.lines, 27700)
traffic.points <- st_transform(traffic.points, 27700)

#creat a quick summary aadt column
summary(as.factor(traffic.lines$maxyear))
traffic.lines$aadt <- traffic.lines$all_motors_16
traffic.lines$cycles <- traffic.lines$cycles_16

summary(as.factor(traffic.points$maxyear))
traffic.points$aadt <- NA
traffic.lines$cycles
traffic.points.df <- as.data.frame(traffic.points)
for (c in 1:nrow(traffic.points)){
  maxyr <- as.character(traffic.points$maxyear[c] - 2000)
  if(nchar(maxyr) == 1){
    maxyr <- paste0("0",maxyr)
  }
  colname1 <- paste0("all_motors_",maxyr)
  colname2 <- paste0("cycles_",maxyr)
  traffic.points$aadt[c] <- traffic.points.df[c,colname1]
  traffic.points$cycles[c] <- traffic.points.df[c,colname2]
}
rm(c,maxyr,colname1, colname2,traffic.points.df)

#save results
saveRDS(traffic.lines,"../cyipt-bigdata/traffic/trafficlines.Rds")
saveRDS(traffic.points,"../cyipt-bigdata/traffic/trafficpoints.Rds")

