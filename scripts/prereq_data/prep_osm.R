#Prepare  OSM Data
#The code take in OSM data and remove unneeded columns, then splits lines at junctions
#It returns two outputs:
# 1) the split lines with the attributes of the original data
# 2) The points where the lines are split, i.e. junction locations



library(sf)
library(dplyr)

source("R/functions.R")

#FUnction to Splitlines
splitlines <- function(a){
  line_sub <- osm[a,]
  buff_sub <- buff[inter[[a]],]
  if(nrow(buff_sub) == 0){
    line_cut <- line_sub
  }else{
    buff_sub <- st_union(buff_sub)
    line_cut <- st_difference(line_sub, buff_sub)
  }
  return(line_cut)
}

#Settings

#skip <- FALSE #SKIP EXISTING FOLDERS #setting now from master file

#List folders

#regions <- list.dirs(path = "../cyipt-bigdata/osm-clean", full.names = FALSE) #region no from master file
#regions <- regions[2:length(regions)]
regions <- regions.todo

#create directory
if(!dir.exists(paste0("../cyipt-bigdata/osm-prep"))){
  dir.create(paste0("../cyipt-bigdata/osm-prep"))
}

for(a in 1:length(regions)){
  message(paste0("Splitting Lines for ",regions[a]," at ",Sys.time()))
  if(file.exists(paste0("../cyipt-bigdata/osm-clean/",regions[a],"/osm-lines.Rds"))){
    if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-lines.Rds")) & skip){
      message("Skipping as done before")
    }else{
      #Create ouptu dir
      dir.create(paste0("../cyipt-bigdata/osm-prep/",regions[a]))

      #Reading in data
      osm <- readRDS(paste0("../cyipt-bigdata/osm-clean/",regions[a],"/osm-lines.Rds"))
      points <- readRDS(paste0("../cyipt-bigdata/osm-raw/",regions[a],"/osm-junction-points.Rds"))

      #Loop To Split Lines
      buff <- st_buffer(points,0.01)
      inter <- st_intersects(osm,buff)
      cut_list <- lapply(1:nrow(osm), splitlines)
      cut <- do.call("rbind",cut_list)
      cut <- cut[!duplicated(cut$geometry),]
      rm(cut_list)

      #Split MULTILINESTRING into LINESTRING
      result <- splitmulti(cut,"MULTILINESTRING","LINESTRING")
      rm(cut)

      #Add ID
      result$id <- 1:nrow(result)
      row.names(result) <- result$id

      #Save Out Data
      saveRDS(result, paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-lines.Rds"))

      message(paste0("Started with ",nrow(osm)," lines, finished with ",nrow(result)))
      rm(osm, result, bounds,buff,inter, points)
      gc()

      #################################
      #Remove Cleaned Data

      #if(overwrite){
      #  unlink(paste0("../cyipt-bigdata/osm-clean/",regions[a]), recursive = T)
      #}


    }
  }else{
    message(paste0("Input File missing for ",regions[a]," at ",Sys.time()))
  }
}
rm(a,regions)








