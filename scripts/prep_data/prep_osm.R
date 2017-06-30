#Prepare  OSM Data
#The code take in OSM data and remove unneeded columns, then splits lines at junctions
#It returns two outputs:
# 1) the split lines with the attributes of the original data
# 2) The points where the lines are split, i.e. junction locations

library(sf)
library(dplyr)

source("R/functions.R")

#Functions
#Function To get Points and MultiPoints
findpoints <- function(b){
  lines_sub <- lines[c(touch[[b]]),]
  inter_sub <- st_intersection(lines_sub, lines_sub)
  inter_sub <- inter_sub[,c("osm_id","geometry")]
  inter_sub <- inter_sub[st_geometry_type(inter_sub) == "POINT" | st_geometry_type(inter_sub) == "MULTIPOINT",]
  return(inter_sub)
}

#FUnction to Splitlines
splitlines <- function(a){
  line_sub <- lines[a,]
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

skip <- TRUE #SKIP EXISTING FOLDERS

#List folders

regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE)

for(a in 2:length(regions)){
  print(paste0("Doing ",regions[a]," at ",Sys.time()))
  if(file.exists(paste0("../cyipt-bigdata/osm-raw/",regions[a],"/osm-lines.Rds"))){
    if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-lines.Rds")) & skip){
      print("Skipping as done before")
    }else{
      #Create ouptu dir
      dir.create(paste0("../cyipt-bigdata/osm-prep/",regions[a]))

      #Reading in data
      osm <- readRDS(paste0("../cyipt-bigdata/osm-raw/",regions[a],"/osm-lines.Rds"))
      osm <- st_transform(osm, 27700)

      #Create working dataset
      lines <- osm[,c("osm_id","geometry")]
      osm <- as.data.frame(osm)
      osm$geometry <- NULL

      #Find Points
      print(paste0("Find Points at ",Sys.time()))
      touch <- st_intersects(lines)
      points_list <- lapply(1:length(touch), findpoints)
      points <- do.call("rbind",points_list)
      points <- points[!duplicated(points$geometry),]
      rm(points_list)

      #Split multipoints into points
      print(paste0("Split multipoints to points at ",Sys.time()))
      points <- splitmulti(points,"MULTIPOINT","POINT")

      #Remove duplicates
      points <- points[!duplicated(points$geometry),]

      #Loop To Split Lines
      print(paste0("Splitting Lines at ",Sys.time()))
      buff <- st_buffer(points,0.01)
      inter <- st_intersects(lines,buff)
      cut_list <- lapply(1:nrow(lines), splitlines)
      cut <- do.call("rbind",cut_list)
      cut <- cut[!duplicated(cut$geometry),]
      rm(cut_list)

      #Split multipoints into points
      cut_sl <- splitmulti(cut,"MULTILINESTRING","LINESTRING")
      rm(cut)

      #Join Variaibles back togther
      result <- left_join(cut_sl,osm, by = c("osm_id" = "osm_id"))
      rm(cut_sl)
      result$id <- 1:nrow(result)
      rm(bounds,buff,lines,inter,touch)

      res_geom <- result[,c("id","osm_id","geometry")]
      res_val <- as.data.frame(result)
      res_val$geometry <- NULL
      res_val <- res_val[,c("id",names(res_val)[!(names(res_val) %in% "id")])]
      row.names(points) <- 1:nrow(points)

      #Save Out Data
      saveRDS(res_geom, paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-lines.Rds"))
      #st_write(res_geom, paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-lines.geojson"))
      saveRDS(points,  paste0("../cyipt-bigdata/osm-prep/",regions[a],"/junction-points.Rds"))
      #st_write(points, paste0("../cyipt-bigdata/osm-prep/",regions[a],"/junction-points.geojson"))
      write.csv(res_val, paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-variables.csv"), row.names = FALSE)

      print(paste0("Started with ",nrow(osm)," lines, finished with ",nrow(result)," lines and ",nrow(points)," points"))
      rm(osm, result, res_geom,points,res_val)
      gc()
    }
  }else{
    print("Input File Missing")
  }
}








