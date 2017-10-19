#Gets PCT Values for the road segments

############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################

library(sf)
library(dplyr)
library(parallel)
library(foreach)


#Settings now come from master file
#skip <- FALSE #Skip Files that already have PCT values
#ncores <- 4 #number of cores to use in parallel processing
#overwrite <- FALSE #Overwrite or create new file

#Functions


#Get the
get.collisions <- function(a,osm,acc,junc,grid_osm,grid_acc,grid_junc){
  #acc.sub <- acc[a,]
  acc.sub <- acc[a,c("AccRefGlobal","JunctionDetail","geometry")]
  gridid <- grid_acc[a][[1]]

  if(as.character(acc.sub$JunctionDetail) %in% c("Crossroads","Mini-roundabout","More than 4 arms (not roundabout)","Multiple junction","Other junction","Roundabout","T or staggered junction","T, Y or staggered junction")){
    #Treat as a Junction
    junc_grid <- grid_junc[gridid]
    junc_grid <- unlist(junc_grid)
    junc_grid <- junc_grid[!duplicated(junc_grid)]
    junc_presub <- junc[junc_grid,] #select all the osm junctions in the same grid cell

    if(nrow(junc_presub) >= 1){
      #Get distance to lines
      dist <- as.numeric(st_distance(acc.sub,junc_presub))

      #get id number of the closest line
      sel.junc <- junc_presub$osm_id[dist == min(dist)]
      sel.line <- NA
    }else{
      sel.junc <- NA
      sel.line <- NA
    }


  }else{
    #Treat as a road
    osm_grid <- grid_osm[gridid]
    osm_grid <- unlist(osm_grid)
    osm_grid <- osm_grid[!duplicated(osm_grid)]
    osm_presub <- osm[osm_grid,] #select all the osm lines in the same grid cell

    if(nrow(osm_presub) >= 1){
      #Get distance to lines
      dist <- as.numeric(st_distance(acc.sub,osm_presub))

      #get id number of the closest line
      sel.line <- osm_presub$id[dist == min(dist)]
      sel.junc <- NA
    }else{
      sel.junc <- NA
      sel.line <- NA
    }



  }

  result <- data.frame(AccRefGlobal = acc.sub$AccRefGlobal, CollisionLine = sel.line, CollisionJunc = sel.junc)
  return(result)
}


count.collisions.lines <- function(c){
  lineid <- osm$id[c]
  acc.sub <- acc.lines[acc.lines$CollisionLine == lineid,]
  nslight <- length(acc.sub$Severity[acc.sub$Severity == "Slight"])
  nserious <- length(acc.sub$Severity[acc.sub$Severity == "Serious"])
  nfatal <- length(acc.sub$Severity[acc.sub$Severity == "Fatal"])
  result <- data.frame(lineid = lineid, nslight = nslight, nserious = nserious, nfatal= nfatal)
  return(result)
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
    if(all(c("FILL ME IN") %in% names(osm)) & skip){
      message(paste0("Collisions values already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting Collisions values for ",regions[b]," at ",Sys.time()))

      #If overwriting remove old data
      col.to.keep <- names(osm)[!names(osm) %in% c("FILL ME IN")]
      osm <- osm[,col.to.keep]
      rm(col.to.keep)

      #Get collisions data
      acc <- readRDS(paste0("../cyipt-bigdata/collisions/",regions[b],"/collisions-summary.Rds"))

      #Get junction locations
      junc <- readRDS(paste0("../cyipt-bigdata/osm-raw/",regions[b],"/osm-junction-points.Rds"))

      #Performacne Tweak, Preallocate object to a gid to reduce processing time
      grid <- st_make_grid(osm, n = c(100,100), "polygons")
      st_crs(grid) <- st_crs(osm)
      grid_osm <- st_intersects(grid, osm) # for each grid which osm lines cross it
      grid_junc <- st_intersects(grid, junc) # for each grid which osm lines cross it
      grid_acc <- st_intersects(acc, grid)# Which grid is each collision in?
      rm(grid)


      message(paste0("Preparations complete, starting data collection at ",Sys.time()))

      #Get the Collisions Values
      m = 1 #Start
      n = 200
      #n = nrow(acc) #End

      ##########################################################
      #Parallel
      start <- Sys.time()
      #fun <- function(cl){
      #  parLapply(cl, m:n,get.collisions)
      #}
      cl <- makeCluster(ncores) #make clusert and set number of cores
      clusterExport(cl=cl, varlist=c("osm", "acc","junc","grid_osm","grid_acc","grid_junc"))
      clusterEvalQ(cl, {library(sf)})
      #clusterSplit(cl, m:n)
      #respar <- fun(cl)
      respar <- parLapply(cl,m:n,get.collisions,osm = osm,acc = acc,junc = junc,grid_osm = grid_osm,grid_acc = grid_acc,grid_junc = grid_junc)
      stopCluster(cl)
      respar <- do.call("rbind",respar)
      end <- Sys.time()
      message(paste0("Did ",n-m + 1," collisions in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))


      foo <- get.collisions(1,osm,acc,junc,grid_osm,grid_acc,grid_junc)
      start <- Sys.time()
      foo <- lapply(1:10,get.collisions,osm = osm,acc = acc,junc = junc,grid_osm = grid_osm,grid_acc = grid_acc,grid_junc = grid_junc)
      end <- Sys.time()
      message(round(difftime(end,start,units = "secs"),2))
      foo <- do.call("rbind",foo)


      df1 <- data.frame(a = 1:26000, b = rep(letters,1000), c = rep(c("a","b"), 13), stringsAsFactors = F)
      df2 <- data.frame(d = 1:26, e = letters, stringsAsFactors = F)

      func <- function(i){
        df2.sub <- df2[i,]
        letter <- df2.sub$e[[1]]

        df1.sub <- df1[df1$b == letter,]
        numb.a <- length(df1.sub$c[df1.sub$c == "a"])
        numb.b <- length(df1.sub$c[df1.sub$c == "b"])

        result <- data.frame(d = letter, A = numb.a, B = numb.b)
        return(result)
      }

      foo <- func(1)



      ##########################################################
      rm(n,m,cl,grid_osm,start,end)

      #Join togther data
      acc <- left_join(acc,respar, by = c("AccRefGlobal" = "AccRefGlobal"))
      acc.lines <- acc[!is.na(acc$CollisionLine),]
      acc.junc <- acc[!is.na(acc$CollisionJunc),]

      saveRDS(acc,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/collisions.Rds"))

      m = 1
      n = nrow(osm)

      #Summarise Collison Data for Each Road
      ##########################################################
      #Parallel
      start <- Sys.time()
      fun <- function(cl){
        parLapply(cl, m:n,count.collisions.lines)
      }
      cl <- makeCluster(ncores) #make clusert and set number of cores
      clusterExport(cl=cl, varlist=c("osm", "acc.lines"))
      clusterEvalQ(cl, {library(sf)}) #; {splitmulti()}) #Need to load splitmuliin corectly
      respar <- fun(cl)
      stopCluster(cl)
      respar <- do.call("rbind",respar)
      end <- Sys.time()
      message(paste0("Counted collisions for ",n," roads in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
      ##########################################################
      rm(n,m,cl,start,end)

      osm <- left_join(osm,respar, by = c("id" = "lineid"))



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


