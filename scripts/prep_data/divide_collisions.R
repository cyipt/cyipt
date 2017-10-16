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
bounds <- readRDS("../cyipt-bigdata/boundaries/local_authority/local_authority.Rds")

#Subset to england
bounds$lad16cd <- as.character(bounds$lad16cd)
bounds <- bounds[substr(bounds$lad16cd,1,1) == "E",]

#Strip symbols
bounds$lad16nm <- as.character(bounds$lad16nm)
bounds$lad16nm <- str_replace_all(bounds$lad16nm,"[[:punct:]]","")
bounds$lad16nm <- str_replace_all(bounds$lad16nm," ","")


#Loop Over each region
for(a in 21:nrow(bounds)){
  region <- bounds[a,]
  region.name <- region$lad16nm[1]
  message(paste0("Doing ",region.name," at ",Sys.time()))
  acc <- acc.all[region,]
  if(nrow(acc) == 0){
    message(paste0("No Data for ",region.name," so skipping"))
  }else{
    cas <- cas.all[cas.all$AccRefGlobal %in% acc$AccRefGlobal,]
    foo <- sapply(1:nrow(acc),count.bike.cas)
    acc$nCasualtiesBike <- sapply(1:nrow(acc),count.bike.cas)
    if(!dir.exists(paste0("../cyipt-bigdata/collisions/",region.name))){
      dir.create(paste0("../cyipt-bigdata/collisions/",region.name))
    }
    saveRDS(acc, paste0("../cyipt-bigdata/collisions/",region.name,"/collisions-summary.Rds"))
    rm(acc,cas,region,region.name)
  }

}
