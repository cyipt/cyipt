#Prepare PCT data fro use in CyIPT
#Gets all routes
#joins in the PCT estimates
#Removed routes ther there is never any cycling estimated
#Saves out in sf format projected as Brit Nat Grid epsg:27700


library(sp)
library(sf)
library(dplyr)

#Get PCT data

pct4 <- readRDS("D:/Users/earmmor/OneDrive - University of Leeds/Cycling Big Data/LSOA/rf_nat_4plus_fix.Rds")
pct3 <- readRDS("D:/Users/earmmor/OneDrive - University of Leeds/Cycling Big Data/LSOA/rf_nat_less3p_fix.Rds")

#Reproject
pct4 <- spTransform(pct4,CRS("+init=epsg:27700"))
pct3 <- spTransform(pct3,CRS("+init=epsg:27700"))

#convert to sf
pct4 <- st_as_sf(pct4)
pct3 <- st_as_sf(pct3)

#combine
pct.all <- rbind(pct4,pct3)
rm(pct3,pct4)

#Get flow data
flow <- readRDS("D:/Users/earmmor/OneDrive - University of Leeds/Cycling Big Data/LSOA/LSOA_flow.Rds")
flow$motorvehicle <- flow$carorvan + flow$motorcycle + flow$taxi + flow$passenger
flow$publictransport <- flow$bus + flow$train + flow$underground
flow <- flow[,c("id","all_16p","bicycle_16p","onfoot","motorvehicle","publictransport","other","is_two_way")]
flow <- flow[flow$id %in% pct.all$ID,]
flow_data <-  read.csv("D:/Users/earmmor/OneDrive - University of Leeds/Cycling Big Data/LSOA/flow_results_nat_round_170121.csv")
names(flow_data) <- c("id","pct.gov","pct.gen","pct.dutch","pct.ebike")
flow_data$id <- as.character(flow_data$id)

#join data
pct.all <- left_join(pct.all,flow, by = c("ID" = "id"))
pct.all <- left_join(pct.all,flow_data, by = c("ID" = "id"))
rm(flow,flow_data)

#rename some columns
names(pct.all)[names(pct.all) == 'bicycle_16p'] <- 'pct.census'

#Dump unneeded columns
pct.all <- pct.all[,c("ID","length","busyness","av_incline","pct.census","is_two_way","pct.gov","pct.gen","pct.dutch","pct.ebike","onfoot","motorvehicle","publictransport","other","geometry")]
class(pct.all$pct.census)

#find where bike is always 0
rsum <- pct.all$pct.census + pct.all$pct.dutch + pct.all$pct.ebike +pct.all$pct.gov + pct.all$pct.gen
pct.all <- pct.all[rsum > 0,]
rm(rsum)

#save resutls
saveRDS(pct.all,"../cyipt-securedata/pct-routes-all.Rds")
