# Divide Collision Data by region
#Prepare necessary varaibles for matching
library(stringr)
library(sf)

#functions
count.bike.cas <- function(b){
  id <- acc$AccRefGlobal[b]
  cas.sub <- cas[cas$AccRefGlobal == id,]
  ncycle <- length(cas.sub$CasualtyType[cas.sub$CasualtyType == "Cyclist"])
  return(ncycle)
}

#code

acc.all <- readRDS("../cyipt-bigdata/collisions/acc.Rds")
acc.all <- acc.all[,c("AccRefGlobal","Severity","nVehicles","nCasualties","JunctionDetail","DateTime")]
cas.all <- readRDS("../cyipt-bigdata/collisions/cas.Rds")
veh.all <- readRDS("../cyipt-bigdata/collisions/veh.Rds")

#Get Boundy
bounds <- readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
bounds <- st_transform(bounds, 27700)

#Loop Over each region
for(a in 1:nrow(bounds)){
  region <- bounds[a,]
  region.name <- region$ttwa11nm[1]
  message(paste0("Doing ",region.name," at ",Sys.time()))
  acc <- acc.all[region,]
  if(nrow(acc) == 0){
    message(paste0("No Data for ",region.name," so skipping"))
  }else{
    cas <- cas.all[cas.all$AccRefGlobal %in% acc$AccRefGlobal,]
    #foo <- sapply(1:nrow(acc),count.bike.cas)
    acc$nCasualtiesBike <- sapply(1:nrow(acc),count.bike.cas)
    if(!dir.exists(paste0("../cyipt-bigdata/collisions/regions/",region.name))){
      dir.create(paste0("../cyipt-bigdata/collisions/regions/",region.name))
    }
    saveRDS(acc, paste0("../cyipt-bigdata/collisions/regions/",region.name,"/collisions-summary.Rds"))
    rm(acc,cas,region,region.name)
  }

}
