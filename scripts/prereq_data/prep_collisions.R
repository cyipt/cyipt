library(foreign)
library(sf)
library(memisc)
#library(stplanr)
library(dplyr)
library(tmap)
tmap_mode("view")

folder <- "../cyipt-securedata/stats19/"
tmp.folder <- "F:/ziptmp"
files <- list.files(path = folder, full.names = FALSE) # Now get regions from the master file

acc.list <- list()
cas.list <- list()
veh.list <- list()

source("../stats19/R/import_functions.R")

for(a in 1:31){
#for(a in 1:3){
  record <- unzip(paste0(folder,files[a]), exdir = tmp.folder, overwrite = T)
  message("Doing ",record[1]," at ",Sys.time())

  if(length(record) > 3 & a <= 21){
    acc <- stats19.import(record[1], "acc", lables = TRUE)
    cas <- stats19.import(record[2], "cas", lables = TRUE)
    veh <- stats19.import(record[3], "veh", lables = TRUE)

  }else if(length(record) == 3 & a <= 21){
    acc <- stats19.import(record[2], "acc", lables = TRUE)
    cas <- stats19.import(record[3], "cas", lables = TRUE)
    veh <- stats19.import(record[1], "veh", lables = TRUE)

  }else if(length(record) > 3 & a > 21){
    acc <- stats19.import(record[1], "acc", lables = FALSE)
    cas <- stats19.import(record[2], "cas", lables = FALSE)
    veh <- stats19.import(record[3], "veh", lables = FALSE)

    acc <- stats19.code2string(dat = acc, "acc")
    cas <- stats19.code2string(dat = cas, "cas")
    veh <- stats19.code2string(dat = veh, "veh")

  }else if(length(record) == 3 & a > 21){
    acc <- stats19.import(record[2], "acc", lables = FALSE)
    cas <- stats19.import(record[3], "cas", lables = FALSE)
    veh <- stats19.import(record[1], "veh", lables = FALSE)

    acc <- stats19.code2string(dat = acc, "acc")
    cas <- stats19.code2string(dat = cas, "cas")
    veh <- stats19.code2string(dat = veh, "veh")
  }else{
    message("unknown case")
    stop()
  }

  acc <- stats19.clean(dat = acc, "acc")
  cas <- stats19.clean(dat = cas, "cas")
  veh <- stats19.clean(dat = veh, "veh")

  acc$Year <- (1985:2015)[a]
  cas$Year <- (1985:2015)[a]
  veh$Year <- (1985:2015)[a]

  acc.list[[a]] <- acc
  cas.list[[a]] <- cas
  veh.list[[a]] <- veh

  unlink(record, recursive = T)
  rm(acc,cas,veh)


}

#For some later data the lables in the sav files are incorrectly used
#so extract the raw numbers and add lables in manually

cas.all <- do.call("rbind",cas.list)
acc.all <- do.call("rbind",acc.list)
veh.all <- do.call("rbind",veh.list)

#Add ids
acc.all$AccRefYear <- paste0(acc.all$AccRef,acc.all$Year)
acc.all$AccRefGlobal <- 1:nrow(acc.all)

cas.all$AccRefYear <- paste0(cas.all$AccRef,cas.all$Year)
veh.all$AccRefYear <- paste0(veh.all$AccRef,veh.all$Year)

acc.all.sub <- as.data.frame(acc.all[,c("AccRefGlobal","AccRefYear")])
acc.all.sub$geometry <- NULL


summary(acc.all.sub$AccRefGlobal)

veh.all <- left_join(veh.all,acc.all.sub, by = c("AccRefYear" = "AccRefYear"))
cas.all <- left_join(cas.all,acc.all.sub, by = c("AccRefYear" = "AccRefYear"))

summary(is.na(cas.all$AccRefYear))
summary(is.na(veh.all$AccRefYear))

summary(cas.all$AccRefGlobal)
summary(veh.all$AccRefGlobal)

cas.all <- cas.all[!is.na(cas.all$AccRefGlobal),]
veh.all <- veh.all[!is.na(veh.all$AccRefGlobal),]

acc.all$AccRefYear <- NULL
cas.all$AccRefYear <- NULL
veh.all$AccRefYear <- NULL

#Recreate Factors
#Change charactors to factors
classes <- lapply(acc.all, class)
for(b in 1:length(classes)){
  if(classes[b] == "character" & (names(acc.all)[b] %in% c("Severity","Week","LA","RoadClass1","RoadType","SpeedLimit","JunctionDetail","JunctionControl","RoadClass2","CrossingControl","CrossingFacilities","Light","Weather","Surface","SpecialConditions","Hazards","PlaceReported","PoliceOfficerAttend","UrbanRural","LSOA","LAHighway"))){
    acc.all[[b]] <- as.factor(acc.all[[b]])
  }else if(classes[b] == "logical"){
    veh.all[[b]] <- NULL
  }
}


classes <- lapply(cas.all, class)
for(b in 1:length(classes)){
  if(classes[b] == "character" & (names(acc.all)[b] %in% c("CasualtyClass","CasSex","AgeBand","Severity","PedestrianLocation","PedestrianMovement","PedestrianDirection","SchoolPupil","SeatBelt","CarPassenger","BusPassenger","CasualtyType","MaintenanceWorker","HomeArea","CasualtyIMD"))){
    cas.all[[b]] <- as.factor(cas.all[[b]])
  }else if(classes[b] == "logical"){
    veh.all[[b]] <- NULL
  }
}


classes <- lapply(veh.all, class)
for(b in 1:length(classes)){
  if(classes[b] == "character" & (names(acc.all)[b] %in% c("VehicleType","TowingArticulation","Manoeuvre","From","To","LocationRoad","LocationRestrictedAway","Junction","SkiddingOverturning","ObjectInCarriageway","LeavingCarriageway","ObjectOffCarriageway","VehicleLetter","PointofImpact","OtherVehicle","CombinedDamage","RoofUndersideDamage","SexDriver","VehAgeBand","BreathTest","HitRun","ForeignVehicle","LeftHandDrive","JourneyPurpose ","EngineSize","Propulsion","AgeVehicle","DriverIMD","DriverArea","VehicleIMD"))){
    veh.all[[b]] <- as.factor(veh.all[[b]])
  }else if(classes[b] == "logical"){
    veh.all[[b]] <- NULL
  }
}

st_crs(acc.all) <- 27700

#Save RDS file for mathcing with OSM
saveRDS(acc.all,"../cyipt-bigdata/collisions/acc.Rds")
saveRDS(cas.all,"../cyipt-bigdata/collisions/cas.Rds")
saveRDS(veh.all,"../cyipt-bigdata/collisions/veh.Rds")










