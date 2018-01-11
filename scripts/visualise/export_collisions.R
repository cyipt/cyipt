library(sf)

#functions
clean.vars <- function(x,prefix){
  for(b in 1:ncol(x)){
    if(class(x[,b]) == "character" & names(x)[b] != "geotext"){
      x[,b] <- as.factor(x[,b])
      message("Changing ",names(x)[b]," from character to factor")
    }

    if(class(x[,b]) == "factor"){
      message("Changing ",names(x)[b]," from factor to interger")
      #Create a lookup table
      lookup <- data.frame(code = 1:nlevels(x[,b]), label = levels(x[,b]))
      # Save ou the lookup table
      write.csv(lookup,paste0("../cyipt-bigdata/forDB/lookup/",prefix,"_",colnames(x[b]),".csv"), row.names = F)
      x[,b] <- as.integer(x[,b])
    }else if(class(x[,b]) == "numeric"){
      if(all(unique(x[,b]) %in% c(NA, 0:10000) )){
        message("Changing ",names(x)[b]," from numeric to interger")
        x[,b] <- as.integer(x[,b])
      }
    }
  }
  return(x)
}

clean.nas <- function(x){
  togo <- NA
  for(i in 1:ncol(x)){
    if(all(is.na(x[,i]))){
      message(paste0("removing ",names(x)[i]))
      togo <- c(togo,i)

    }
  }
  togo <- togo[!is.na(togo)]
  x[,togo] <- NULL
  return(x)
}


#code
acc.all <- readRDS("../cyipt-bigdata/collisions/acc.Rds")
cas.all <- readRDS("../cyipt-bigdata/collisions/cas.Rds")
veh.all <- readRDS("../cyipt-bigdata/collisions/veh.Rds")

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

cas.all <- cas.all[,c("AccRefGlobal","VehicleRef","CasualtyRef","CasualtyClass","CasSex","Age",
                      "PedestrianMovement","PedestrianDirection","SchoolPupil","SeatBelt","CarPassenger",
                      "BusPassenger","CasualtyType","MaintenanceWorker","HomeArea","CasualtyIMD")]

veh.all <- veh.all[,c("AccRefGlobal","VehicleRef","VehicleType",
                      "TowingArticulation","Manoeuvre","VehFrom","VehTo",
                      "LocationRoad","LocationRestrictedAway","Junction","SkiddingOverturning",
                      "ObjectInCarriageway","LeavingCarriageway","ObjectOffCarriageway","VehicleLetter",
                      "PointofImpact","OtherVehicle","CombinedDamage","RoofUndersideDamage",
                      "SexDriver","AgeDriver","VehAgeBand",
                      "HitRun","ForeignVehicle","LeftHandDrive",
                      "EngineSize","Propulsion","AgeVehicle","DriverIMD",
                      "DriverArea","VehicleIMD","JourneyPurpose")]



#remove data missign lables
#cas.all[cas.all == "Data missing or out of range"] <- NA
#acc.all[acc.all == "Data missing or out of range"] <- NA
#veh.all[veh.all == "Data missing or out of range"] <- NA

#Remove any all NA columns
veh.all <- clean.nas(veh.all)
cas.all <- clean.nas(cas.all)


object.size(acc.all)
acc.all <- clean.vars(acc.all,"acc")
object.size(acc.all)

object.size(veh.all)
veh.all <- clean.vars(veh.all,"veh")
object.size(veh.all)


object.size(cas.all)
cas.all <- clean.vars(cas.all,"cas")
object.size(cas.all)



write.csv(acc.all,"../cyipt-bigdata/forDB/accidents.csv", row.names = F, na = "")
write.csv(cas.all,"../cyipt-bigdata/forDB/casualties.csv", row.names = F, na = "")
write.csv(veh.all,"../cyipt-bigdata/forDB/vehicles.csv", row.names = F, na = "")
