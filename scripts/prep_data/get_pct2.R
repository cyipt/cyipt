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

find.pct.lines <- function(i){
  pct_sub <- pct.all[i,]
  pct_id <- pct_sub$ID[1]
  grid_ids <- grid_pct2grid[[i]]
  osm_ids <- unique(unlist(grid_grid2osm[grid_ids]))
  osm_sub <- osm[osm_ids,]
  res <- roadsOnLine(roads = osm_sub, line2check =  pct_sub$geometry, tolerance = 4)
  #res.list <- vector("list", 1)
  #names(res.list) <- pct_id
  #res.list[[pct_id]] <- res
  #qtm(grid[grid_ids]) + qtm(pct.all[i,], lines.lwd = 5, lines.col = "black") + qtm(osm_sub) + qtm(osm_sub[res,], lines.col = "green", lines.lwd = 3)
  #return(res.list)
  return(res)
}

#get pct row numbers for a given osm row number
getpctids <- function(y){
  return(seq_along(respar)[sapply(seq_along(respar),function(x){y %in% respar[[x]]})])
}

#get pct values for a given osm row number
getpctvalues <- function(i){
  pct.sub <- pct.all[osm2pct[[i]],]
  count <- data.frame(id = i,
                      pct.census = sum(pct.sub$pct.census),
                      pct.gov = sum(pct.sub$pct.gov),
                      pct.gen = sum(pct.sub$pct.gen),
                      pct.dutch = sum(pct.sub$pct.dutch) ,
                      pct.ebike = sum(pct.sub$pct.ebike))
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

      #Futher subset using the region boundary
      bounds <- readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
      bounds <- bounds[bounds$ttwa11nm == regions[b],]
      bounds <- st_transform(bounds, st_crs(osm))
      nrow(pct.all)
      pct.all <- pct.all[bounds,]
      nrow(pct.all)
      saveRDS(pct.all,paste0("../cyipt-securedata/pct-regions/",regions[b],".Rds")) #save selection for later use

      #Performacne Tweak, Preallocate object to a grid to reduce processing time
      grid <- st_make_grid(osm, n = c(500,500), "polygons")
      grid_pct2grid <- st_intersects(pct.all, grid) # Which grids is each pct line in?
      grid_grid2osm <- st_intersects(grid, osm)# for each grid which osm lines cross it


      ##########################################################
      #Parallel
      m = 1
      n = nrow(pct.all)
      start <- Sys.time()
      fun <- function(cl){
        parLapply(cl, m:n,find.pct.lines)
      }
      cl <- makeCluster(ncores) #make clusert and set number of cores
      clusterExport(cl=cl, varlist=c("grid_pct2grid", "pct.all","grid_grid2osm","osm"))
      clusterExport(cl=cl, c('find.pct.lines') )
      clusterEvalQ(cl, {library(sf); source("R/functions.R")})
      respar <- fun(cl)
      stopCluster(cl)

      end <- Sys.time()
      message(paste0("Did ",n," lines in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
      #identical(foo,respar)
      ##########################################################

      saveRDS(respar,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct2osm.Rds"))

      # Convert A PCT to OSM lookup to an OSM 2 PCT lookup
      # We do this in a backwards way becuase we need the PCT to OSM one later

      osm2pct <- lapply(1:nrow(osm), getpctids)

      #Now for each osm like get the pct values


      foo <- lapply(1:nrow(osm), getpctvalues)
      foo <- bind_rows(foo)

      osm2 <- left_join(osm, foo, by = c("id" = "id"))




      #qtm(osm[3,], lines.lwd = 6, lines.col = "red" ) + qtm(pct.all[osm2pct[[3]], ], lines.lwd = 2, lines.col = "green")





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


