library(sf)
library(mapview)
library(dplyr)
library(multiplex)

folder <- "D:/Users/earmmor/OneDrive - University of Leeds/Cycling Big Data/OS/MasterMapRoads/MasterMap Highways Network_rami_2408772"
files <- list.files(folder)

files.RoadLink <- files[grepl("RoadLink",files)]
files.RoadLink <- files.RoadLink[!grepl(".gfs",files.RoadLink)]
files.RoadLink <- files.RoadLink[!grepl(".properties",files.RoadLink)]

RoadLink <- list()
for(i in 1:length(files.RoadLink)){
  tmp <- st_read(paste0(folder,"/",files.RoadLink[i]))
  RoadLink[[i]] <- tmp
  rm(tmp)
}

RoadLink <- bind_rows(RoadLink)
RoadLink <- as.data.frame(RoadLink)
RoadLink$geometry <- st_sfc(RoadLink$geometry)
RoadLink <- st_sf(RoadLink)
st_crs(RoadLink) <- 27700

saveRDS(RoadLink,"../cyipt-securedata/osRoads/RoadLink.Rds")

files.Dedication <- files[grepl("Dedication",files)]
files.Dedication <- files.Dedication[!grepl(".gfs",files.Dedication)]
files.Dedication <- files.Dedication[!grepl(".properties",files.Dedication)]

Dedication <- list()
for(i in 1:length(files.Dedication)){
  tmp <- st_read(paste0(folder,"/",files.Dedication[i]))
  Dedication[[i]] <- tmp
  rm(tmp)
}

Dedication <- bind_rows(Dedication)
Dedication <- as.data.frame(Dedication)
Dedication$geometry <- st_sfc(Dedication$geometry)
Dedication <- st_sf(Dedication)
st_crs(Dedication) <- 27700

saveRDS(Dedication,"../cyipt-securedata/osRoads/Dedication.Rds")
summary(Dedication$dedication)


files.Dedication <- files[grepl("Dedication",files)]
files.Dedication <- files.Dedication[!grepl(".gfs",files.Dedication)]
files.Dedication <- files.Dedication[!grepl(".properties",files.Dedication)]

Dedication <- list()
for(i in 1:length(files.Dedication)){
  tmp <- st_read(paste0(folder,"/",files.Dedication[i]))
  Dedication[[i]] <- tmp
  rm(tmp)
}

Dedication <- bind_rows(Dedication)
Dedication <- as.data.frame(Dedication)
Dedication$geometry <- st_sfc(Dedication$geometry)
Dedication <- st_sf(Dedication)
st_crs(Dedication) <- 27700


folder.streets <- "D:/Users/earmmor/OneDrive - University of Leeds/Cycling Big Data/OS/MasterMapRoads/MasterMap Highways Network - Paths_2408771"
files.streets <- list.files(folder.streets)

files.PathLink <- files.streets[grepl("PathLink",files.streets)]
files.PathLink <- files.PathLink[!grepl(".gfs",files.PathLink)]
files.PathLink <- files.PathLink[!grepl(".properties",files.PathLink)]

PathLink <- list()
for(i in 1:3){
#for(i in 1:length(files.PathLink)){
  tmp <- st_read(paste0(folder.streets,"/",files.PathLink[i]))
  tmp <- tmp[,c("gml_id","identifier","beginLifespanVersion","localId","namespace","fictitious","reasonForChange",
                "formOfWay","provenance","surfaceType","matchStatus","length","length_uom","startGradeSeparation",
                "endGradeSeparation","inDirection","inDirection_uom","inOppositeDirection","inOppositeDirection_uom",
                "cycleFacility")]
  #tmp.classes <- sapply(tmp,class)
  #tmp.classes <- lapply(tmp.classes, `[`, 1)
  #tmp <- tmp[,names(tmp)[!tmp.classes == "list"]]
  #tmp$wholeLink <- as.character(tmp$wholeLink)
  PathLink[[i]] <- tmp
  rm(tmp)
}

PathLink <- bind_rows(PathLink)
PathLink <- as.data.frame(PathLink)
PathLink$geometry <- st_sfc(PathLink$geometry)
PathLink <- st_sf(PathLink)
st_crs(PathLink) <- 27700


bounds <- readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
bounds <- bounds[bounds$ttwa11nm == "Leeds",]
bounds <- st_transform(bounds, 27700)
#mapview(bounds)

Dedication.sub <- Dedication[Dedication$dedication == "Cycle Track Or Cycle Way",]
mapview(Dedication.sub)


