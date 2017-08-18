#Gets PCT Values for the road segments

############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################

library(sf)
library(dplyr)

#Settings now come from master file
#skip <- FALSE #Skip Files that already have PCT values
#ncores <- 4 #number of cores to use in parallel processing
#overwrite <- FALSE #Overwrite or create new file

#Functions
#Function for classified roads
get.aadt.class <- function(e){
  traffic.sub <- traffic.class[traffic.class$road == roadnames[e],]
  osm.sub <- osm.nona[osm.nona$ref == roadnames[e],]

  #need at least 2 points to make voronoi polygons
  if(nrow(traffic.sub) > 1){
    #Make voronoi polygons and convert to SF
    voronoi <- dismo::voronoi(xy = st_coordinates(traffic.sub))
    voronoi <- as(voronoi, "sf")
    st_crs(voronoi) <- st_crs(traffic.sub)
  }else{
    #Make a big buffer around the point
    voronoi <- st_buffer(traffic.sub, 1000)
  }

  #qtm(traffic.sub) +
  #  qtm(osm.sub) +
  #  qtm(voronoi)

  #Find Intersections of roads with vernoi polygons
  inter <- st_intersects(osm.sub,voronoi)
  #Get aadt and ncycle values
  osm.sub$aadt <- lapply(1:nrow(osm.sub),function(x){as.numeric(round(mean(traffic.sub$aadt[inter[[x]]])),0)})
  osm.sub$ncycles <- lapply(1:nrow(osm.sub),function(x){as.numeric(round(mean(traffic.sub$ncycles[inter[[x]]])),0)})

  #Remove Unneded Data
  osm.sub <- as.data.frame(osm.sub)
  osm.sub <- osm.sub[,c("osm_id","aadt","ncycles")]

  return(osm.sub)
}

#FUnction for unclassified roads
get.aadt.unclass <- function(j){
  traffic.sub <- unclass.buff[j,]
  osm.sub <- osm.unclass[st_intersects(traffic.sub,osm.unclass)[[1]],]
  if(nrow(osm.sub) == 0){
    osm.sub <- NA
  }else{
    osm.sub$aadt <- as.numeric(traffic.sub$aadt[1])
    osm.sub$ncycles <- as.numeric(traffic.sub$ncycles[1])
    #Remove Unneded Data
    osm.sub <- as.data.frame(osm.sub)
    osm.sub <- osm.sub[,c("osm_id","aadt","ncycles")]

  }
  return(osm.sub)
}


#List folders
#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo



for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-clean/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-clean/",regions[b],"/osm-lines.Rds"))
    #Check if PCT values exist in the file
    if(all(c("aadt","ncycles") %in% names(osm)) & skip){
      message(paste0("Traffic values already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting traffic values for ",regions[b]," at ",Sys.time()))

      #If overwriting remove old data
      col.to.keep <- names(osm)[!names(osm) %in% c("aadt","ncycles")]
      osm <- osm[,col.to.keep]
      rm(col.to.keep)

      # Read in Data
      traffic.points <-  readRDS("../cyipt-bigdata/traffic/traffic.Rds")

      #dump unneded columns
      traffic.points <- traffic.points[,c("road","aadt","ncycles")]

      #Get bounding box
      ext <- st_bbox(osm)
      ext <- st_sfc(st_polygon(list(rbind(c(ext[1],ext[2]),c(ext[3],ext[2]),c(ext[3],ext[4]),c(ext[1],ext[4]),c(c(ext[1],ext[2]))))) )
      pol <- data.frame(id = 1, geometry = NA)
      st_geometry(pol) <- ext
      st_crs(pol) <- 27700
      rm(ext)


      #Subset Traffic data to bounding box
      traffic.points <- traffic.points[st_intersects(pol,traffic.points)[[1]],]
      rm(pol)

      #Separate Calssified and Unlassified Roads
      traffic.class <- traffic.points[!substr(traffic.points$road,1,1) %in% c("U","C"),]
      traffic.unclass <- traffic.points[substr(traffic.points$road,1,1) %in% c("U","C"),]
      #traffic.class <- traffic.points[regexpr('U', traffic.points$road) != 1,]
      #traffic.unclass <- traffic.points[regexpr('U', traffic.points$road) == 1,]
      nrow(traffic.class) + nrow(traffic.unclass) == nrow(traffic.points)
      rm(traffic.points)

      #start with the classified
      roadnames <- unique(traffic.class$road)
      roadnames <- roadnames[roadnames %in% osm$ref]
      osm.nona <- osm[!is.na(osm$ref),] #Create a working dataset without nas
      osm.nona <- osm.nona[,c("osm_id","ref")] #Dump unneeded data
      res.class <- lapply(1:length(roadnames),get.aadt.class)
      res.class <- do.call("rbind",res.class)
      res.class <- res.class[!is.na(res.class$osm_id),]
      rm(osm.nona,roadnames)

      #Now do the unclassified
      unclass.buff <- st_buffer(traffic.unclass, 8) #buffer the points in case of poor alignment
      osm.unclass <- osm[unique(unlist(st_intersects(unclass.buff,osm))),]
      osm.unclass <- osm.unclass[,c("osm_id")] #Dump unneeded data
      res.unclass <- lapply(1:nrow(unclass.buff),get.aadt.unclass)
      res.unclass <- do.call("rbind",res.unclass)
      res.unclass <- res.unclass[!is.na(res.unclass$osm_id),]
      rm(osm.unclass, unclass.buff)

      #Put togther
      res <- rbind(res.class,res.unclass)
      rm(res.class,res.unclass, traffic.class, traffic.unclass)

      #remove any duplicates
      res <- res[!duplicated(res$osm_id),]
      res$aadt <- as.numeric(res$aadt)
      res$ncycles <- as.numeric(res$ncycles)

      #Join onto the original osm data
      osm <- left_join(osm,res, by = c("osm_id" = "osm_id"))
      rm(res)

      #Save results
      if(overwrite){
        saveRDS(osm,paste0("../cyipt-bigdata/osm-clean/",regions[b],"/osm-lines.Rds"))
      }else{
        saveRDS(osm,paste0("../cyipt-bigdata/osm-clean/",regions[b],"/osm-lines-traffic.Rds"))
      }
      rm(osm)



    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)


qtm(osm[!is.na(osm$aadt),], lines.col = "aadt", lines.lwd = 10) +
  qtm(traffic.points)
