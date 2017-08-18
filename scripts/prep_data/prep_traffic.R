# Prepare Traffic Data
#New method only focusing on points and road names

library(sf)

traffic <- read.csv("trafficcounts/trafficcounts.csv", stringsAsFactors = F, sep = ",", quote = '"')

#Remove junk columns
traffic <- traffic[,names(traffic)[!names(traffic) %in% c("westminstername","boroughname","wardname","road_geom","startLatitude","startLongitude","finishLatitude","finishLongitude")]]

#Change Data types to numeric
col.number <- names(traffic)[!(names(traffic) %in% c("westminstername","boroughname","wardname","road","road_type","road_geom"))]

for(a in 1:length(col.number)){
  traffic[,col.number[a]] <- as.numeric(traffic[,col.number[a]])
}
rm(col.number)

#make points into spatial objects
points.geom <- st_sfc(st_multipoint(cbind(traffic$longitude, traffic$latitude)))
points.geom <- st_cast(points.geom, "POINT")
traffic <- st_sf(traffic, geometry = points.geom, crs = 4326)
rm(points.geom)

traffic$latitude <- NULL
traffic$longitude <- NULL

#Transform to British national grid
traffic <- st_transform(traffic, 27700)

#creat a quick summary aadt column
summary(as.factor(traffic$maxyear))

traffic$aadt <- NA
traffic.df <- as.data.frame(traffic)
for (c in 1:nrow(traffic)){
  maxyr <- as.character(traffic$maxyear[c] - 2000)
  if(nchar(maxyr) == 1){
    maxyr <- paste0("0",maxyr)
  }
  colname1 <- paste0("all_motors_",maxyr)
  colname2 <- paste0("cycles_",maxyr)
  traffic$aadt[c] <- traffic.df[c,colname1]
  traffic$ncycles[c] <- traffic.df[c,colname2]
}
rm(c,maxyr,colname1, colname2,traffic.df)

#Dump Unneded Data
traffic <- traffic[,c("cp","road","road_type","maxyear","aadt","ncycles","geometry" )]

#save results
saveRDS(traffic,"../cyipt-bigdata/traffic/traffic.Rds")

