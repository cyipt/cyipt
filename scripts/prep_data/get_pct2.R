#Gets PCT Values for the road segments

############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################

#Functions
source("R/functions.R")

find.pct.lines <- function(i){
  #message(i)
  pct_sub <- pct.all[i,]
  pct_id <- pct_sub$ID[1]
  grid_ids <- grid_pct2grid[[i]]
  osm_ids <- unique(unlist(grid_grid2osm[grid_ids]))
  osm_sub <- osm[osm_ids,]
  res <- roadsOnLine(roads = osm_sub, line2check =  pct_sub$geometry, tolerance = 4)
  return(res)
}

#get pct row numbers for a given osm row number
getpctids <- function(y){
  return(seq_along(pct2osm)[sapply(seq_along(pct2osm),function(x){y %in% pct2osm[[x]]})])
}

#get pct values for a given osm row number
getpctvalues <- function(i){
  pct.sub <- pct.all[osm2pct[[i]],]
  count <- data.frame(id = i,
                      pct.census = sum(pct.sub$pct.census),
                      pct.gov = sum(pct.sub$pct.gov),
                      pct.gen = sum(pct.sub$pct.gen),
                      pct.dutch = sum(pct.sub$pct.dutch) ,
                      pct.ebike = sum(pct.sub$pct.ebike),
                      pct.total = sum(pct.sub$total))
}


#List folders
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

      #add total column
      pct.all$total <- pct.all$pct.census + pct.all$onfoot + pct.all$workathome + pct.all$underground + pct.all$train + pct.all$bus + pct.all$taxi + pct.all$motorcycle + pct.all$carorvan + pct.all$passenger + pct.all$other


      # Check if pct lines are completly inside the region
      inside <- st_contains_properly(bounds,pct.all, sparse = FALSE)
      inside <- t(inside)
      pct.all$insideRegion <- inside[,1]
      rm(inside)
      message(paste0("Warning: ",sum(pct.all$insideRegion == FALSE)," of ",length(pct.all$ID)," (",round(sum(pct.all$insideRegion == FALSE)/length(pct.all$ID)*100,1)," %)"," of PCT lines cross the region boundary"))
      #qtm(pct.all[!pct.all$insideRegion,])

      saveRDS(pct.all,paste0("../cyipt-securedata/pct-regions/",regions[b],".Rds")) #save selection for later use

      #remove the values we no longer need
      pct.all <- pct.all[,c("ID","pct.census","pct.gov","pct.gen","pct.dutch","pct.ebike","all_16p")]
      names(pct.all) <- c("ID","pct.census","pct.gov","pct.gen","pct.dutch","pct.ebike","pct.total","geometry")

      #Performacne Tweak, Preallocate object to a grid to reduce processing time
      grid <- st_make_grid(osm, n = c(500,500), "polygons")
      grid_pct2grid <- st_intersects(pct.all, grid) # Which grids is each pct line in?
      grid_grid2osm <- st_intersects(grid, osm)# for each grid which osm lines cross it

      #pct2osm <- lapply(1:nrow(pct.all), find.pct.lines)
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
      pct2osm <- fun(cl)
      stopCluster(cl)

      end <- Sys.time()
      message(paste0("Did ",n," lines in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
      ##########################################################

      saveRDS(pct2osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct2osm.Rds"))

      # Convert A PCT to OSM lookup to an OSM 2 PCT lookup
      # We do this in a backwards way becuase we need the PCT to OSM one later

      #osm2pct <- lapply(1:nrow(osm), getpctids)
      ##########################################################
      #Parallel
      m = 1
      n = nrow(osm)
      start <- Sys.time()
      fun <- function(cl){
        parLapply(cl, m:n,getpctids)
      }
      cl <- makeCluster(ncores) #make clusert and set number of cores
      clusterExport(cl=cl, varlist=c("pct2osm"))
      clusterExport(cl=cl, c('getpctids') )
      osm2pct <- fun(cl)
      stopCluster(cl)

      end <- Sys.time()
      message(paste0("Got ",n," lines of PCT ids in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
      ##########################################################

      saveRDS(osm2pct,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm2pct.Rds"))

      #Now for each osm like get the pct values

      ##########################################################
      #Parallel
      m = 1
      n = nrow(osm)
      start <- Sys.time()
      fun <- function(cl){
        parLapply(cl, m:n,getpctvalues)
      }
      cl <- makeCluster(ncores) #make clusert and set number of cores
      clusterExport(cl=cl, varlist=c("osm2pct","pct.all"))
      clusterExport(cl=cl, c('getpctvalues') )

      pct_vals <- fun(cl)
      stopCluster(cl)
      end <- Sys.time()
      message(paste0("Got ",n," lines of PCT values in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
      ##########################################################
      pct_vals <- bind_rows(pct_vals)

      osm <- left_join(osm, pct_vals, by = c("id" = "id"))
      Sys.time()



      #qtm(osm[3,], lines.lwd = 6, lines.col = "red" ) + qtm(pct.all[osm2pct[[3]], ], lines.lwd = 2, lines.col = "green")





      #Save results
      if(overwrite){
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
      }else{
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines-pct.Rds"))
      }
      rm(osm,pct.all,pct_vals,osm2pct,pct2osm,grid,grid_grid2osm,grid_pct2grid)



    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)


