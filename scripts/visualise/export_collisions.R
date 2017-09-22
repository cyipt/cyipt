library(sf)

acc.all <- readRDS("../cyipt-bigdata/collisions/acc.Rds")
cas.all <- readRDS("../cyipt-bigdata/collisions/cas.Rds")
veh.all <- readRDS("../cyipt-bigdata/collisions/veh.Rds")


#Get bounding box
#pol2 <- data.frame(id = 1, geometry = NA)
#st_geometry(pol2) <- st_sfc(st_polygon(list(rbind(c(0,0),c(700000,0),c(700000,1230000),c(0,1230000),c(c(0,0))))) )
#st_crs(pol2) <- 27700


#Remove Points no in the bounding box
#nrow(acc.all)
#acc.all <- acc.all[st_intersects(pol2,acc.all)[[1]],]
#nrow(acc.all)

#Change Geometry for DB
acc.all <- st_transform(acc.all, 4326)

#Reduce precison of data to reduce file size
acc.all$geometry <- st_as_binary(acc.all$geometry, precision = 1000000)
acc.all$geometry <- st_as_sfc(acc.all$geometry)

#Change to PostGIS WKT
acc.all$geotext <- st_as_text(acc.all$geometry)
acc.all <- as.data.frame(acc.all)
acc.all$geometry <- NULL

#Remove rows where coordinates are not valid
#cc.all <- acc.all[acc.all$geotext != "POINT(NaN NaN)",]
summary(acc.all$geotext == "POINT(NaN NaN)")
summary(acc.all$geotext == "POINT(0 0)")


#Clean Up For DB
acc.all <- acc.all[,c("AccRefGlobal","DateTime","Severity", "nVehicles","nCasualties","RoadClass1","RoadNumber1","RoadType","SpeedLimit",
                      "JunctionDetail","JunctionControl", "RoadClass2", "RoadNumber2","CrossingControl",
                      "CrossingFacilities","Light","Weather","Surface","SpecialConditions","Hazards","geotext")]


classes <- lapply(acc.all, class)
for(b in 1:length(classes)){
  if(classes[b] == "factor"){
    #Create a lookup table
    lookup <- data.frame(code = 1:nlevels(acc.all[,b]), label = levels(acc.all[,b]))
    # Save ou the lookup table
    write.csv(lookup,paste0("../cyipt-bigdata/forDB/lookup/",colnames(acc.all[b]),".csv"), row.names = F)
    acc.all[,b] <- as.integer(acc.all[,b])
  }
}


write.csv(acc.all,"../cyipt-bigdata/forDB/accidents.csv", row.names = F, na = "")
write.csv(cas.all,"../cyipt-bigdata/forDB/casualties.csv", row.names = F, na = "")
write.csv(veh.all,"../cyipt-bigdata/forDB/vehicles.csv", row.names = F, na = "")
