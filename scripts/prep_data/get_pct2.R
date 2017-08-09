#Gets PCT Values for the road segments

############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################


#Need to fix
#Roads that touch the same road at both ends still get the incorect PCT value

library(sf)
library(dplyr)
library(parallel)
library(pbapply)

#Settings
skip <- FALSE #Skip Files that already have PCT values
ncores <- 6 #number of cores to use in parallel processing

#Functions
source("R/functions.R")

getpctvalues <- function(a){
  osm_sub <- osm[a,]
  #Trim off the ends of the line
  points <- st_cast(osm_sub$geometry, "POINT")
  points <- points[c(1,length(points))] #Get first and last point on the lines
  len <- as.numeric(st_distance(points[1],points[2])) #Change to distance between points to deal with curved roads
  if(len < 4 & len != 0){ # To hanel very small lines
    cutlen <- len/2.5
  }else{
    cutlen <- 2
  }
  buff <- st_buffer(points, cutlen) #Make small circiels around the ends
  buff <- st_union(buff)
  osm_sub <- st_difference(osm_sub, buff) # Cut off the ends
  #rm(points,buff, len, cutlen)
  #Get grid IDS
  gridid <- grid_osm[a][[1]]
  rf_grid <- grid_pct[gridid]
  rf_grid <- unlist(rf_grid)
  rf_grid <- rf_grid[!duplicated(rf_grid)]
  rf_presub <- pct.all[rf_grid,]
  sel <- st_intersects(osm_sub, rf_presub)[[1]]

  #Check that lines intersec with both ends of the road (i.ie they are not just crossing)
  buff <- st_cast(buff,"POLYGON", group_or_split = T)
  if(length(buff) == 1){
    #Do nothing, edge case where a looped road exists whith same start and end point
    count <- data.frame(id = osm_sub$id ,pct.census = 0, pct.gov = 0,pct.gen = 0, pct.dutch = 0 , pct.ebike = 0)
  }else{
    sel2 <- st_intersects(buff[1], rf_presub)[[1]]
    sel3 <- st_intersects(buff[2], rf_presub)[[1]]
    sel4 <- sel2[sel2 %in% sel3]
    if(sum(lengths(sel4)) == 0){
      #Do Nothing
      #message("sel4 lenght == 0")
      count <- data.frame(id = osm_sub$id ,pct.census = 0, pct.gov = 0,pct.gen = 0, pct.dutch = 0 , pct.ebike = 0)
    }else{
      #Check for cul-de-sacs where road touched the same road at both ends
      grd <- grid_osm[[a]]
      osm_other <- osm[sapply(grid_osm,function(x)any(x %in% grd)),] #Needed to hand lines in multiple grids
      osm_other1 <- osm_other[unique(unlist(st_intersects(buff[1],osm_other))),]
      osm_other2 <- osm_other[unique(unlist(st_intersects(buff[2],osm_other))),]
      osm_other1 <- osm_other1[!(osm_other1$id %in% a),]
      osm_other2 <- osm_other2[!(osm_other2$id %in% a),]
      match <- osm_other1$id[osm_other1$id %in% osm_other2$id]
      if(length(match) == 0){
        #No cul-de-sac case
        lenother <- 0
      }else{
        lenother <- as.numeric(st_length(osm_other[osm_other$id == match[1],])) # In some edge cases drops data (i.e. mutiple clu-de-sacs)
      }
      #plot(osm_other1[1], add = T, col = "Green", lwd = 3)
      #plot(osm_other2[1], add = T, col = "Yellow", lwd = 3)
      if(lenother > len){
        #Cul-de-sac
        #Do Nothing
        count <- 0
      }else{
        #Split out the lines that are very close
        rf_sub <- rf_presub[sel4,]
        cuts <- st_difference(rf_sub,buff)
        cutsl <- splitmulti(cuts, "MULTILINESTRING", "LINESTRING")
        cutsl$len <- as.numeric(st_length(cutsl))
        cutsl <- cutsl[cutsl$len > (0.95 * len) & cutsl$len < (1.05 * len),] #Get segments that are withing 5% lenf of the line
        #count <- sum(rf_sub$bicycle_16p)
        count <- data.frame(id = osm_sub$id, pct.census = sum(rf_sub$pct.census), pct.gov = sum(rf_sub$pct.gov),pct.gen = sum(rf_sub$pct.gen), pct.dutch = sum(rf_sub$pct.dutch) , pct.ebike = sum(rf_sub$pct.ebike))
        #count <- rf_sub
        #lengths(cuts_geom)
        #plot(rf_sub, add = T, lwd = 2, col = "Green")
      }
    }
    rm(buff,sel2,sel3,sel4)
  }

  return(count)
}




#List folders
regions <- list.dirs(path = "../cyipt-bigdata/osm-prep", full.names = FALSE)

for(a in 2:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-lines.Rds"))
    #Check if PCT values exist in the file
    if(all(c("pct.census","pct.gov","pct.gen","pct.dutch","pct.ebike") %in% names(osm)) & skip){
      message(paste0("PCT values already calcualted for ",regions[a]," so skipping"))
    }else{
      message(paste0("Getting PCT values for ",regions[a]," at ",Sys.time()))

      #Get bounding box
      ext <- st_bbox(osm)
      ext <- st_sfc(st_polygon(list(rbind(c(ext[1],ext[2]),c(ext[3],ext[2]),c(ext[3],ext[4]),c(ext[1],ext[4]),c(c(ext[1],ext[2]))))) )
      pol <- data.frame(id = 1, geometry = NA)
      st_geometry(pol) <- ext

      rm(ext)

      #Get pct data and subset to bounding box
      pct.all <- readRDS("../cyipt-securedata/pct-routes-all.Rds")
      st_crs(pol) <- st_crs(pct.all) #For some reason the CRS are fractionally different
      pct.all <- pct.all[pol,]
      pct.all <- st_transform(pct.all, st_crs(osm)) #transfor so that crs are idetical

      #Performacne Tweak, Preallocate object to a gid to reduce processing time
      grid <- st_make_grid(osm, n = c(100,100), "polygons")
      grid_osm <- st_intersects(osm, grid) # Which grid is each osm line in?
      grid_pct <- st_intersects(grid, pct.all)# for each grid which pct lines cross it
      rm(grid)


      #Get the PCT Values
      m = 1 #Start
      n = nrow(osm) #End

      message(paste0("Preparations complete, starting data collection at ",Sys.time()))


      ##########################################################
      #Parallel
      start <- Sys.time()
      fun <- function(cl){
        parLapply(cl, m:n,getpctvalues)
      }
      cl <- makeCluster(ncores) #make clusert and set number of cores
      clusterExport(cl=cl, varlist=c("osm", "pct.all","grid_osm","grid_pct"))
      clusterEvalQ(cl, {library(sf); source("R/functions.R")}) #; {splitmulti()}) #Need to load splitmuliin corectly
      respar <- fun(cl)
      stopCluster(cl)
      respar <- do.call("rbind",respar)
      end <- Sys.time()
      message(paste0("Did ",n," lines in ",round(difftime(end,start,units = "secs"),2)," in parallel mode"))
      #identical(res,respar)
      ##########################################################

      #Join togther data
      osm <- left_join(osm,respar, by = c("id" = "id"))

      #Save results
      saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-lines-pct.Rds"))

    }

  }else{
    message(paste0("Input File Missing for ",regions[a]))
  }
}



