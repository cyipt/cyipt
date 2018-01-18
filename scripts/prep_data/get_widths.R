#Gets road widths from OS data

#Big Function
getroadwidths <- function(a){
  #Get data
  line <- osm[a,] #OSM Line
  osm.id <- line$id[1]

  #Pre subset for speed
  gridno <- grid_osm[[a]]
  os_grids <- grid_os[gridno][[1]]
  os_presub <- os[os_grids,]

  #Get OSM Line and OS Polygons Near By
  line <- line["geometry"]
  AOI <- st_buffer(line, dist = 15, nQuadSegs = 1) # Area intrest around the line, set to 15 m
  AOI <- AOI["geometry"]
  os_sub <- os_presub[st_intersects(AOI,os_presub)[[1]],] #Faster selection from smaller dataset

  rm(gridno,os_grids,os_presub)

  #Create road and roadside polygons
  roadside <- os_sub[os_sub$DESCGROUP == "Roadside", ]
  roadside <- st_intersection(AOI, roadside)
  names(roadside) <- c(names(roadside)[1:2],"geometry")
  st_geometry(roadside) <- "geometry"
  roadside <- roadside[,c("OBJECTID","geometry") ]
  roadside <- splitmulti(roadside,"MULTIPOLYGON","POLYGON")

  road <- os_sub[os_sub$DESCGROUP == "Road Or Track" | os_sub$DESCGROUP == "Path",]
  road <- road[,c("OBJECTID","geometry") ]
  road <- st_intersection(AOI, road)
  names(road) <- c("OBJECTID","geometry")
  st_geometry(road) <- "geometry"
  road <- splitmulti(road,"MULTIPOLYGON","POLYGON")
  if(nrow(road) != 0 ){
    road$id.temp <- 1:nrow(road)
  }


  if(class(road$geometry)[[1]] == "sfc_GEOMETRY"){ #An edge case when the results come out as a geomtry (this ditches some data)
    road <- st_cast(road, "POLYGON")
  }
  road <- road[st_intersects(line,road)[[1]],]
  rm(os_sub, AOI)

  #THe big if statments
  if(nrow(road) == 0 & nrow(roadside) == 0){
    #No Data so skip everthing
    widthpathres <- NA
    widthres <- NA

  }else if(nrow(road) > 0 & nrow(roadside) > 0){
    #################################################################
    #Road and Roadside Approach
    road$width <- width_estimate(road)
    ##Find intersections
    #Get Intersection Points
    road_str <- st_cast(road, "MULTILINESTRING", group_or_split=TRUE)
    osm_inter <- st_intersection(line,road_str)
    rm(road_str)

    #Check if line crosses polygon boundaries, if so then split line and take longest part
    if(length(osm_inter) == 0){
      #Line does not cross polygon boundaries, use whole line
      line_main <- line
    }else{
      #Split Points and Mulitpoints
      osm_inter <- splitmulti(osm_inter,"MULTIPOINT","POINT")
      #If only one polygon use that
      if(nrow(road) == 1){
        line_main <- line
      }else{
        #Buffer Pointsand make into a singe mulipolygon
        osm_buff <- st_buffer(osm_inter, dist = 0.01)
        buff_geom <- osm_buff$geom
        buff_geom <- st_union(buff_geom)
        rm(osm_buff, osm_inter)

        #Cut the line with buffered points
        osm_diff <- st_cast(st_difference(line,buff_geom), "LINESTRING")
        osm_diff$length <- as.numeric(st_length(osm_diff, dist_fun = geosphere::distGeo))

        #Select the rigth segment of the line
        line_main <- osm_diff[osm_diff$length == max(osm_diff$length),]
        rm(osm_diff, buff_geom)
      }
    }

    #Join line and poyglon to get the road width
    line_main <- line_main[,"geometry", drop = FALSE]
    line_main$sub <- 1:nrow(line_main) #Can't do the join without a variaible
    osm_join <- st_join(line_main,road, join = st_intersects, left = TRUE)

    #Update Table
    widthres <- unlist(osm_join$width[1])

    #Select roaddside touching the road section
    road_main <- road[road$id.temp == osm_join$id.temp[1], ] # check agaist the temp id
    road_main <- road_main[!is.na(road_main$id.temp),]
    rm(line_main, osm_join)
    if(nrow(road_main) == 0){
      #Can't do anything
      widthpathres <- NA
    }else{
      touch <- st_touches(road_main, roadside, sparse = FALSE)
      #touch2 <- st_touches(roadside, road_main, sparse = FALSE)
      #Get the paths that touch the road and trim off any traling edges at 5m
      #Not this means that paths wider than 5 meters are capped at 5m
      roadside_touch <- roadside[touch,]
      if(nrow(roadside_touch) == 0){
        #Can't do anything
        widthpathres <- NA
      }else{
        roadside_one <- st_union(roadside_touch)
        roadside_touch <- roadside_touch[1,]
        roadside_touch$geometry <- roadside_one
        rm(roadside_one)
        road_buff <- st_buffer(road_main, dist = 5)
        roadside_touch <- st_intersection(roadside_touch, road_buff)
        comb <- st_union(roadside_touch, road_main)
        comb <- comb[,c("OBJECTID","geometry")]
        comb <- splitmulti(comb, "MULTIPOLYGON", "POLYGON")
        comb <- comb[1,] #Union can create multiple copies just take first
        #comb <- st_buffer(comb, dist = 0.001) #To deal with mulipolgons

        #Get final width
        widthpathres <- unlist(width_estimate(comb))
        rm(roadside_touch, road_buff, comb, touch)
      }

    }
    rm(road_main)

  }else if(nrow(road) > 0 & nrow(roadside) == 0){
    ########################################################
    #Road Only approach
    widthpathres <- NA
    road$width <- width_estimate(road)
    ##Find intersections
    #Get Intersection Points
    road_str <- st_cast(road, "MULTILINESTRING", group_or_split=TRUE)
    osm_inter <- st_intersection(line,road_str)
    rm(road_str)

    #Check if line crosses polygon boundaries, if so then split line and take longest part
    if(length(osm_inter) == 0){
      #Line does not cross polygon boundaries, use whole line
      line_main <- line
    }else{
      #Split Points and Mulitpoints
      osm_inter <- splitmulti(osm_inter,"MULTIPOINT","POINT")
      #If only one polygon  use that
      if(nrow(road) == 1 ){
        line_main <- line
      }else{
        #Buffer Pointsand make into a singe mulipolygon
        osm_buff <- st_buffer(osm_inter, dist = 0.01)
        buff_geom <- osm_buff$geom
        buff_geom <- st_union(buff_geom)
        rm(osm_buff, osm_inter)

        #Cut the line with buffered points
        osm_diff <- st_cast(st_difference(line,buff_geom), "LINESTRING")
        osm_diff$length <- as.numeric(st_length(osm_diff, dist_fun = geosphere::distGeo))

        #Select the rigth segment of the line
        line_main <- osm_diff[osm_diff$length == max(osm_diff$length),]
        rm(osm_diff, buff_geom)
      }
    }

    #Join line and polygon to get the road width
    line_main <- line_main[,"geometry", drop = FALSE]
    line_main$sub <- 1:nrow(line_main) #Can't do the join without a variaible
    osm_join <- st_join(line_main,road, join = st_intersects, left = TRUE)

    #Update Table
    widthres <- unlist(osm_join$width[1])
    rm(line_main, osm_join)
    #endint <- Sys.time()
    #warning(paste0("Did road only ", round(difftime(endint, startint, units = "secs"),2), " seconds"))
  }else if(nrow(road) == 0 & nrow(roadside) > 0){
    ######################################################
    #Roadside only approauch
    startint <- Sys.time()
    widthres <- NA
    roadside <- splitmulti(roadside,"MULTIPOLYGON","POLYGON")
    roadside <- roadside[st_intersects(line,roadside)[[1]],] #Find Roadside that intersects the line
    if(nrow(roadside) == 0){
      widthpathres <- NA #None found
    }else{
      roadside$width <- width_estimate(roadside)
      widthpathres <- unlist(roadside$width[1])
    }
    endint <- Sys.time()
    #warning(paste0("Did roadside only in ", round(difftime(endint, startint, units = "secs"),2), " seconds"))

  }else{
    #######################################################
    #Something has gone wrong
    warning("Oh my god, a horrible failure has occured")
    stop()
  }

  #Produce the Final Result
  #clean out nulls
  if(is.null(widthres)){
    widthres <- NA
  }
  if(is.null(widthpathres)){
    widthpathres <- NA
  }

  #put togther
  finalres <- data.frame("id" = osm.id, "width" = widthres, "widthpath" = widthpathres)

  #Check intergrity
  if(class(finalres) == "data.frame"){
    #do nothing
  }else{
    message(paste0("Fiddlesticks, something wrong with ",osm.id))
  }


  #finalres <- c(widthres,widthpathres)
  return(finalres)
  rm(finalres,widthres,widthpathres, road, roadside, line)



}

#####################################
#Start of Code
####################################


regions <- regions.todo

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
    #Check if width values exist in the file
    if(all(c("width","widthpath") %in% names(osm)) & skip){
      message(paste0("Road width values already calcualted for ",regions[b]," so skipping"))
      rm(osm)
    }else{
      message(paste0("Getting road width values for ",regions[b]," at ",Sys.time()))

      #If overwriting remove old data
      col.to.keep <- names(osm)[!names(osm) %in% c("width","widthpath")]
      osm <- osm[,col.to.keep]
      rm(col.to.keep)

      #Get bounding box
      ext <- st_bbox(osm)
      ext <- st_sfc(st_polygon(list(rbind(c(ext[1],ext[2]),c(ext[3],ext[2]),c(ext[3],ext[4]),c(ext[1],ext[4]),c(c(ext[1],ext[2]))))) )
      pol <- data.frame(id = 1, geometry = NA)
      st_geometry(pol) <- ext
      st_crs(pol) <- 27700
      poi <- st_centroid(pol)
      rm(ext)

      #get bounds
      bounds <- readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
      bounds <- bounds[bounds$ttwa11nm == regions[b],]
      bounds <- st_transform(bounds, st_crs(osm))

      #get region
      os.region <- readRDS("../cyipt-bigdata/boundaries/regions.Rds")
      os.region <- os.region[bounds,]
      os.region.name <- as.character(os.region$name)

      # Read in the OS region(S)
      os.list <- list()
      for(i in seq(1,length(os.region.name))){
        os.sub <- readRDS(paste0("../cyipt-securedata/os/",os.region.name[i],".Rds"))
        os.sub$DESCGROUP <- as.character(os.sub$DESCGROUP)
        os.sub <- st_cast(os.sub, "POLYGON")
        os.list[[i]] <- os.sub
        rm(os.sub)
      }
      rm(i)

      #Bind the list togther
      os <- bind_rows(os.list) #much faster than rbind but mangle the sf format, all geometies must be same type
      rm(os.list)

      #rebuild the sf object
      os <- as.data.frame(os)
      os$geometry <- st_sfc(os$geometry)
      os <- st_sf(os)
      st_crs(os) <- 27700

      #subset back to the region
      os <- os[bounds,]

      #Performacne Tweak, Preallocate object to a gid to reduce processing time
      os_cent <- st_centroid(os)
      osm_cent <- st_centroid(osm)
      grid <- st_make_grid(osm, n = c(100,100), "polygons")
      grid_osm <- st_intersects(osm_cent, grid)
      grid_os <- st_intersects(grid, os_cent)
      rm(grid, os_cent, osm_cent)

      if(verbose){message(paste0("Preparations complete, starting data collection at ",Sys.time()))}

      ##########################################################
      #Parallel

      m = 1 #Start
      n = nrow(osm) #End
      start <- Sys.time()
      fun <- function(cl){
        parLapply(cl, m:n,getroadwidths)
      }
      cl <- makeCluster(ncores) #make clusert and set number of cores
      clusterExport(cl=cl, varlist=c("osm", "os","grid_osm","grid_os"))
      clusterEvalQ(cl, {library(sf); source("R/functions.R")}) #; {splitmulti()}) #Need to load splitmuliin corectly
      respar <- fun(cl)
      stopCluster(cl)
      respar <- bind_rows(respar)
      end <- Sys.time()
      if(verbose){message(paste0("Did ",(n-m)+1," lines in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))}
      ##########################################################

      #Join togther data
      osm <- left_join(osm,respar, by = c("id" = "id"))
      rm(start,cl,respar,end)

      #Save results
      if(overwrite){
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
      }else{
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines-width.Rds"))
      }
      rm(osm, os, grid_os, grid_osm,n,m,os.region.name, poi, pol, os.region, bounds)


    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)


