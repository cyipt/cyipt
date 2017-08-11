#Clean and prepare OS data
library(sf)

source("R/functions.R")


files <- list.files(path = "../cyipt-securedata/roadwidth", full.names = FALSE, pattern = ".shp")
files <- files[regexpr('xml', files) == -1]

for(a in 1:length(files)){
  message(paste0("Starting ",files[a]," at ",Sys.time()))
  #Get data
  os <- st_read(paste0("../cyipt-securedata/roadwidth/",files[a]))
  os <- os[,c("OBJECTID","DESCGROUP","geometry")]
  os <- st_transform(os, 27700)

  #convert to single polygon
  os <- splitmulti(os,"MULTIPOLYGON","POLYGON")

  #Get bounding box
  ext <- st_bbox(os)
  ext <- st_sfc(st_polygon(list(rbind(c(ext[1],ext[2]),c(ext[3],ext[2]),c(ext[3],ext[4]),c(ext[1],ext[4]),c(c(ext[1],ext[2]))))) )
  pol <- data.frame(id = 1, geometry = NA)
  st_geometry(pol) <- ext
  st_crs(pol) <- 27700
  poi <- st_centroid(pol)
  rm(ext)

  #hack for SE
  if(files[a] == "SEroads.shp"){
    poi <- st_point(c(516951.899174, 139336.188887))
  }

  #get region
  os.region <- readRDS("../cyipt-bigdata/boundaries/regions.Rds")
  os.region <- os.region[st_intersects(poi,os.region)[[1]],]
  plot(os.region[1])
  os.region.name <- as.character(os.region$name)
  rm(os.region)

  #Save File
  saveRDS(os,paste0("../cyipt-securedata/os/",os.region.name,".Rds"))
  message(paste0("Done ",os.region.name," at ",Sys.time()))
  rm(os,os.region.name, pol,poi)
}





