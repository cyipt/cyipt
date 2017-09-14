library(foreign)
library(sf)
library(memisc)
library(stplanr)
library(dplyr)

folder <- "../cyipt-securedata/stats19/"
tmp.folder <- "F:/ziptmp"
files <- list.files(path = folder, full.names = FALSE) # Now get regions from the master file

acc.list <- list()
cas.list <- list()
veh.list <- list()

source("../stats19/R/import_functions.R")

for(a in 1:length(files)){
#for(a in 1:3){
  record <- unzip(paste0(folder,files[a]), exdir = tmp.folder, overwrite = T)
  message("Doing ",record[1]," at ",Sys.time())

  if(length(record) > 3){
    acc <- stats19.import(record[1], "acc")
    cas <- stats19.import(record[2], "cas")
    veh <- stats19.import(record[3], "veh")
  }else{
    acc <- stats19.import(record[2], "acc")
    cas <- stats19.import(record[3], "cas")
    veh <- stats19.import(record[1], "veh")
  }


  acc.list[[a]] <- acc
  cas.list[[a]] <- cas
  veh.list[[a]] <- veh

  unlink(record, recursive = T)
  rm(acc,cas,veh)


}

acc.list[[31]] <- stats19.code2string(dat = acc.list[[31]],"acc")
cas.list[[31]] <- stats19.code2string(dat = cas.list[[31]],"cas")
veh.list[[31]] <- stats19.code2string(dat = veh.list[[31]],"veh")

cas.all <- do.call("rbind",cas.list)
acc.all <- do.call("rbind",acc.list)
veh.all <- do.call("rbind",veh.list)

acc.all$AccRefYear <- paste0(acc.all$AccRef,acc.all$Year)
acc.all$AccRefGlobal <- 1:nrow(acc.all)

cas.all$AccRefYear <- paste0(cas.all$Ref,cas.all$Year)
veh.all$AccRefYear <- paste0(veh.all$AccRef,veh.all$Year)

acc.all.sub <- as.data.frame(acc.all[,c("AccRefGlobal","AccRefYear")])
acc.all.sub$geometry <- NULL


veh.all <- left_join(veh.all,acc.all.sub, by = c("AccRefYear" = "AccRefYear"))
cas.all <- left_join(cas.all,acc.all.sub, by = c("AccRefYear" = "AccRefYear"))


saveRDS(acc.all,"../cyipt-bigdata/collisions/acc-unclean.Rds")
saveRDS(cas.all,"../cyipt-bigdata/collisions/cas-unclean.Rds")
saveRDS(veh.all,"../cyipt-bigdata/collisions/veh-unclean.Rds")

#acc.all <- readRDS("../cyipt-bigdata/collisions/acc-unclean.Rds")
#cas.all <- readRDS("../cyipt-bigdata/collisions/cas-unclean.Rds")
#veh.all <- readRDS("../cyipt-bigdata/collisions/veh-unclean.Rds")




##############################################################
#RoadNumber2 needs fixing contains characters
######################################################

#Recreate Factors
#Change charactors to factors
classes <- lapply(acc.all, class)
for(b in 1:length(classes)){
  if(classes[b] == "character" & (names(acc.all)[b] %in% c("Severity","Week","LA","RoadClass1","RoadType","SpeedLimit","JunctionDetail","JunctionControl","RoadClass2","CrossingControl","CrossingFacilities","Light","Weather","Surface","SpecialConditions","Hazards","PlaceReported","PoliceOfficerAttend","UrbanRural","LSOA","LAHighway"))){
    acc.all[[b]] <- as.factor(acc.all[[b]])
  }
}


classes <- lapply(cas.all, class)
for(b in 1:length(classes)){
  if(classes[b] == "character" & (names(acc.all)[b] %in% c("CasualtyClass","CasSex","AgeBand","Severity","PedestrianLocation","PedestrianMovement","PedestrianDirection","SchoolPupil","SeatBelt","CarPassenger","BusPassenger","CasualtyType","MaintenanceWorker","HomeArea","CasualtyIMD"))){
    cas.all[[b]] <- as.factor(cas.all[[b]])
  }
}


classes <- lapply(veh.all, class)
for(b in 1:length(classes)){
  if(classes[b] == "character" & (names(acc.all)[b] %in% c("VehicleType","TowingArticulation","Manoeuvre","From","To","LocationRoad","LocationRestrictedAway","Junction","SkiddingOverturning","ObjectInCarriageway","LeavingCarriageway","ObjectOffCarriageway","VehicleLetter","PointofImpact","OtherVehicle","CombinedDamage","RoofUndersideDamage","SexDriver","VehAgeBand","BreathTest","HitRun","ForeignVehicle","LeftHandDrive","JourneyPurpose ","EngineSize","Propulsion","AgeVehicle","DriverIMD","DriverArea","VehicleIMD","AccRefYear","AccRefGlobal"))){
    veh.all[[b]] <- as.factor(veh.all[[b]])
  }
}



#Save RDS file for mathcing with OSM
saveRDS(acc.all,"../cyipt-bigdata/collisions/acc.Rds")
saveRDS(cas.all,"../cyipt-bigdata/collisions/cas.Rds")
saveRDS(veh.all,"../cyipt-bigdata/collisions/veh.Rds")

#Get bounding box
pol2 <- data.frame(id = 1, geometry = NA)
st_geometry(pol2) <- st_sfc(st_polygon(list(rbind(c(0,0),c(700000,0),c(700000,1230000),c(0,1230000),c(c(0,0))))) )
st_crs(pol2) <- 27700


#Remove Points no in the bounding box
acc.all <- acc.all[st_intersects(pol2,acc.all)[[1]],]


#Change Geometry for DB
acc.all <- st_transform(acc.all, 4326)
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







