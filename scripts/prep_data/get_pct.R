#Gets PCT Values for the road segments

############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################

library(sf)
library(dplyr)
library(parallel)


#Settings now come from master file
#skip <- FALSE #Skip Files that already have PCT values
#ncores <- 4 #number of cores to use in parallel processing
#overwrite <- FALSE #Overwrite or create new file

#Functions
source("R/functions.R")

getpctvalues <- function(a){
  #Get the road line of intrest
  osm_sub <- osm[a,]

  #As pct lines don't always perfectyl align with the osm get 3 points from the osm line to check against
  points <- st_cast(osm_sub$geometry, "POINT") #convert road to points
  if(length(points) >= 3){
    #Get first last and a middle point
    points <- points[c(1,ceiling(length(points)/2),length(points))]
  }else if(length(points) == 2){
    #Need 3 points so double get the last point
    points <- points[c(1,1,length(points))]
  }else{
    #Somethign has gone wrong
    warning(paste0("Line ",a," is made up of less than two points. Points =  ",length(points)))
    stop()
  }

  #Buffer points
  len <- as.numeric(st_distance(points[1],points[3])) #Change to distance between points to deal with curved roads
  if(len < 4 & len != 0){ # To hanel very small lines
    cutlen <- len/2.5
  }else{
    cutlen <- 2
  }
  buff <- st_buffer(points, cutlen) #Make small circles around the points

  #Get grid IDS
  gridid <- grid_osm[a][[1]]
  rf_grid <- grid_pct[gridid]
  rf_grid <- unlist(rf_grid)
  rf_grid <- rf_grid[!duplicated(rf_grid)]
  rf_presub <- pct.all[rf_grid,] #select all the PCT lines in the same grid cell

  if(nrow(rf_presub) == 0){ #need to check for when presub is empty
    #return empty result
    count <- data.frame(id = osm_sub$id ,pct.census = 0, pct.gov = 0,pct.gen = 0, pct.dutch = 0 , pct.ebike = 0)
  }else{

    #Check that lines intersect with all three points
    sel.first <- st_intersects(buff[1], rf_presub)[[1]]
    sel.middle <- st_intersects(buff[2], rf_presub)[[1]]
    sel.last <- st_intersects(buff[3], rf_presub)[[1]]
    sel.all <- sel.first[sel.first %in% sel.last]
    sel.all <- sel.all[sel.all %in% sel.middle]
    rf_sub <- rf_presub[sel.all,]

    #Return resutls
    count <- data.frame(id = osm_sub$id, pct.census = sum(rf_sub$pct.census), pct.gov = sum(rf_sub$pct.gov),pct.gen = sum(rf_sub$pct.gen), pct.dutch = sum(rf_sub$pct.dutch) , pct.ebike = sum(rf_sub$pct.ebike))
  }

  return(count)
}

#List folders
#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
    #Check if PCT values exist in the file
    if(all(c("pct.census","pct.gov","pct.gen","pct.dutch","pct.ebike") %in% names(osm)) & skip){
      message(paste0("PCT values already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting PCT values for ",regions[b]," at ",Sys.time()))

      #If overwriting remove old data
      col.to.keep <- names(osm)[!names(osm) %in% c("pct.census","pct.gov","pct.gen","pct.dutch","pct.ebike")]
      osm <- osm[,col.to.keep]
      rm(col.to.keep)

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

      #Performacne Tweak, Preallocate object to a grid to reduce processing time
      grid <- st_make_grid(osm, n = c(100,100), "polygons")
      grid_osm <- st_intersects(osm, grid) # Which grid is each osm line in?
      grid_pct <- st_intersects(grid, pct.all)# for each grid which pct lines cross it
      rm(grid, pol)


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
      message(paste0("Did ",n," lines in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
      #identical(res,respar)
      ##########################################################
      rm(n,m,cl,grid_osm,grid_pct,start,end)

      #Join togther data
      osm <- left_join(osm,respar, by = c("id" = "id"))

      #Save results
      if(overwrite){
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
      }else{
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines-pct.Rds"))
      }
      rm(osm,pct.all,respar)



    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)


