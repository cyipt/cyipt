##########################
# rewrite for performance


############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################

library(sf)
library(dplyr)
library(tmap)
library(pbapply)
library(parallel)
source("R/functions.R")
tmap_mode("view")

#osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",region,"/osm-lines.Rds"))


#Settings now come from master file
#skip <- FALSE #Skip Files that already have PCT values
#ncores <- 4 #number of cores to use in parallel processing
#overwrite <- FALSE #Overwrite or create new file

#Functions
roadsOnLine2 <- function(roads,line2check){
  #points <- st_cast(osm.sub$geometry[a], "POINT") #convert road to points
  #message(paste0("Class of roads is = ", class(roads)))
  #message(paste0("Class of line2check is = ", class(line2check)))
  roads <- st_sfc(roads)
  line2check <- st_sfc(line2check)
  st_crs(roads) <- st_crs(line2check) # Assuming that they are the same as crs for roads get lost in lapply
  points <- st_cast(roads, "POINT") #convert road to points
  points.len <- length(points)
  #message(paste0("Points.len = ", points.len))
  if(points.len >= 3){
    #Get first last and a middle point
    points <- points[c(1,ceiling(points.len/2),points.len)]
    p3 <- TRUE
  }else if(points.len == 2){
    #Need 3 points so double get the last point
    points <- points[c(1,1,points.len)]
    p3 <- FALSE
  }else{
    #Somethign has gone wrong
    warning(paste0("Line ",a," is made up of less than two points. Points =  ",points.len))
    stop()
  }
  #message(paste0("Class of points is = ", class(points)))
  #message(paste0("p3 = ", p3))
  #message(paste0("Points lenght is now = ", length(points)))
  #message(points)
  #Buffer points
  #len <- as.numeric(st_distance(points[1],points[3])) #Change to distance between points to deal with curved roads
  len <- sqrt((points[[1]][1] - points[[3]][1])**2 + (points[[1]][2] - points[[3]][2])**2) # for sort distances on projected coordinates faster than st_distance with same answer

  #message(paste0("len = ", len))
  if(len < 8 & len != 0){ # To hanel very small lines
    cutlen <- len/2.5
  }else{
    cutlen <- 4
  }
  buff <- st_buffer(points, cutlen, nQuadSegs = 2) #Make small circles around the points # Reduce number of segments for speed
  if(p3){
    buff[2] <- st_buffer(points[2], cutlen/1.2, nQuadSegs = 2) # replace middle buffer with smaller value but only if it is a unique point
  }


  #Check that lines intersect with all three points
  #sel <- st_intersects(buff, line)
  sel <- st_intersects(buff, line2check)
  sel.first <- sel[[1]]
  sel.middle <- sel[[2]]
  sel.last <- sel[[3]]


  sel.all <- sel.first[sel.first %in% sel.last]
  sel.all <- sel.all[sel.all %in% sel.middle]

  if(length(sel.all) == 0){
    return(FALSE)
  }else{
    return(TRUE)
  }

}


##########################################################################################
calcChangeBusy <- function(k){
  #pct_.inter.id <- inter[k]
  message(paste0("Doing Line ",k))

  line <- pct.scheme[k,]

  if(line$todo[1]){
    #line2 <- pct.all[inter[[j]][k],]
    #qtm(line)

    # Select the object of intrest in df1 and then find nearby objects in df2
    grid_ids <- unlist(grid_pct[ inter[[j]][k] ]) #compex because first of pct.scheme is not first of pct.all on which grid is made
    ids_grid <- unique(unlist(grid_osm[grid_ids]))
    osm.sub <- osm[ids_grid,]
    #qtm(grid[grid_ids]) + qtm(osm.schemes[j,], lines.lwd = 5, lines.col = "black") + qtm(line, lines.lwd = 3,lines.col = "green") + qtm(line2, lines.lwd = 3,lines.col = "red")

    #Now check for actual intersection between the two objects
    # use buffer because pct lines don always perfectyl match osm roads
    line.buff <- st_buffer(line,4,nQuadSegs = 2)
    line.buff <- line.buff[,"ID"]
    osm.sub <- osm.sub[line.buff,]

    #always end up loosing the start and end roads so clip the raods to the buffer
    osm.sub <- st_intersection(line.buff, osm.sub)
    osm.sub$ID <- 1:nrow(osm.sub)

    #Check for 3 point intersection
    #match = sapply(1:nrow(osm.sub), roadsOnLine)
    #bm <- microbenchmark::microbenchmark(mapply(roadsOnLine2,osm.sub$geometry,line$geometry))
    match = mapply(roadsOnLine2,osm.sub$geometry,line$geometry) #internatised the function
    #match <- unlist(match)
    osm.sub <- osm.sub[match,]

    # CHang in busyness
    busyance.new <- vector(mode= "numeric", length = nrow(osm.sub))
    for(l in 1:nrow(osm.sub)){
      #Check for NA first
      if(is.na(osm.sub$group_id[l])){
        busyance.new[l] <- osm.sub$busyBefore[l]
      }else if(osm.sub$group_id[l] == j){
        busyance.new[l] <- osm.sub$busyAfter[l]
      }else{
        busyance.new[l] <- osm.sub$busyBefore[l]
      }
    }

    s1 <- sum(osm.sub$busyBefore, na.rm = T)
    s2 <- sum(busyance.new, na.rm = T)

    #message(paste0("Line ",k," has had its busyance changed by ",round((s1-s2)/s1 * 100,2)," %" ))
    result <- data.frame(id = line$ID, before = s1, after = s2)
    return(result)
  }else{
    return(NULL)
  }


}



#List folders
#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo
#regions <- "Bristol"

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
    #Check if PCT values exist in the file
    if(all(c("FILL ME IN") %in% names(osm)) & skip){
      message(paste0("Uptake numbers already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting uptake values for ",regions[b]," at ",Sys.time()))

      #If overwriting remove old data
      col.to.keep <- names(osm)[!names(osm) %in% c("FILL ME IN")]
      osm <- osm[,col.to.keep]
      rm(col.to.keep)

      ###################################################################
      # Move this to the get_pct and save out each regions resutls

      #Get bounding box
      ext <- st_bbox(osm)
      ext <- st_sfc(st_polygon(list(rbind(c(ext[1],ext[2]),c(ext[3],ext[2]),c(ext[3],ext[4]),c(ext[1],ext[4]),c(c(ext[1],ext[2]))))) )
      pol <- data.frame(id = 1, geometry = NA)
      st_geometry(pol) <- ext
      rm(ext)

      #Get pct data and subset to bounding box
      pct.all <- readRDS("../cyipt-securedata/pct-routes-all.Rds")

      #dump unneeded data
      pct.all <- pct.all[,c("ID","length","busyness","av_incline","pct.census","onfoot","motorvehicle","publictransport","other")]
      st_crs(pol) <- st_crs(pct.all) #For some reason the CRS are fractionally different
      pct.all <- pct.all[pol,]
      pct.all <- st_transform(pct.all, st_crs(osm)) #transfor so that crs are idetical
      rm(pol)

      #######################################################################

      #calc busyness score
      osm$busyBefore <- osm$length / (osm$quietness/100)

      #Update Busyness scaore based on Reccomended infra
      # simple test make quietness always equal to 100
      osm$busyAfter <- osm$busy
      for(i in 1:nrow(osm)){
        if(osm$Recommended[i] != "None"){
          osm$busyAfter[i] <- osm$length[i]
        }
      }

      #get the list of scheme_nos
      scheme_nos <- unique(osm$group_id)
      scheme_nos <- scheme_nos[!is.na(scheme_nos)]
      scheme_nos <- scheme_nos[order(scheme_nos)]

      if(!all(scheme_nos == 1:length(scheme_nos))){
        message("Problem with scheme numbering")
        stop()
      }


      osm.schemes <- osm[!is.na(osm$group_id),]

      ##########################################################################
      # Make a master PCT line to OSM line intersection table

      #Make all PCT route into one big geometry
      pct.merge <- st_combine(pct.all)
      pct.merge <- st_buffer(pct.all, dist = 1, nQuadSegs = 2)
      object.size(pct.merge)
      pct.merge <- st_simplify(pct.merge, dTolerance = 0.1)
      object.size(pct.merge)
      t1 <- Sys.time()
      osm.pct <- osm[pct.merge,]
      t2 <- Sys.time()
      difftime(t2,t1) # Time difference of 2.839759 hour

      # Work out the interesctions between schemes and roads
      osm.schemes <- osm.schemes %>%
        group_by(group_id) %>%
        summarise()

      inter <- st_intersects(osm.schemes, pct.all)


      #t1 = Sys.time()
      #inter2 <- st_intersects(osm, pct.all)
      #t2 = Sys.time()
      #difftime(t2,t1)

      #t1 = Sys.time()
      #Performacne Tweak, Preallocate object to a grid to reduce processing time
      grid <- st_make_grid(osm, n = c(100,100), "polygons")
      grid_osm <- st_intersects(grid, osm) #  for each grid which osm lines cross it?
      grid_pct <- st_intersects(pct.all, grid)# which grids are each pct line in?
      #rm(grid)
      #t2 = Sys.time()
      #difftime(t2,t1)


















      # Loop over schemes
      uptake.list <- list()
      #for(j in 1:2){
      for(j in scheme_nos){
        message(paste0("Doing Scheme ",j))
        #Get the roads in the schemes
        osm.scheme <- osm.schemes[osm.schemes$group_id == j,]

        # Make a simple buffer around the scheme
        osm.scheme.buf <- st_buffer(osm.scheme,1)
        osm.scheme.buf <- osm.scheme.buf %>%
          group_by(group_id) %>%
          summarise()

        # Get the PCT lines that intersect the scheme
        pct.scheme <- pct.all[inter[[j]],]

        #check the interesctions
        inter.scheme <- st_intersection(osm.scheme.buf, pct.scheme)
        #inter.scheme <- st_cast(inter.scheme, "LINESTRING")
        inter.scheme <- splitmulti(inter.scheme, "MULTILINESTRING", "LINESTRING")

        #remove interecsion less than 10 m
        # this removes routes that just cross the scheme or onyl travel of it for trival part of the journey
        #It also massivel reduced the number of lines to worry about
        inter.scheme$inter_length <- as.numeric(st_length(inter.scheme))
        inter.scheme <- inter.scheme[inter.scheme$inter_length > 10,]
        pct.scheme$todo <- pct.scheme$ID %in% unique(inter.scheme$ID)
        rm(inter.scheme)

        #qtm(osm.scheme.buf) +
        #  qtm(pct.scheme)

        #Now loop over each pct route and find the change in busyness
        #res <- lapply(pct.scheme,calcChangeBusy)
        #res <- lapply(1:2,calcChangeBusy)
        #res <- pblapply(1:nrow(pct.scheme),calcChangeBusy)
        #res <- do.call("rbind",res)
        #message(paste0("DOne Scheme ",j," at ",Sys.time()))

        message("Preparation complete starting data gathering")

        ##########################################################
        #Parallel
        start <- Sys.time()
        fun <- function(cl){
          parLapply(cl, 1:nrow(pct.scheme),calcChangeBusy)
        }
        cl <- makeCluster(4, outfile = paste0("parlog-",Sys.Date(),".txt")) #make clusert and set number of cores
        clusterExport(cl=cl, varlist=c("osm", "pct.scheme","j","inter","grid_osm","grid_pct"))
        clusterExport(cl=cl, c('roadsOnLine2', 'calcChangeBusy') )
        clusterEvalQ(cl, {library(sf)})
        respar <- fun(cl)
        stopCluster(cl)
        respar <- do.call("rbind",respar)
        end <- Sys.time()
        message(paste0("Did in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
        #identical(res,respar)
        ##########################################################



        pct.up <- left_join(pct.scheme, respar, by = c("ID" = "id"))
        pct.up$total <- pct.up$pct.census + pct.up$onfoot + pct.up$motorvehicle + pct.up$publictransport + pct.up$other
        pct.up$model.before <-  round(exp(-2.2679333871 -0.0001207765 * pct.up$length + 0.0242742120 * sqrt(pct.up$length) -0.0075675068 * sqrt(pct.up$before) -1.9347971306 * pct.up$av_incline + -6.8377711996 * sqrt(pct.up$av_incline)),1) * pct.up$total
        pct.up$model.after <-  round(exp(-2.2679333871 -0.0001207765 * pct.up$length + 0.0242742120 * sqrt(pct.up$length) -0.0075675068 * sqrt(pct.up$after) -1.9347971306 * pct.up$av_incline + -6.8377711996 * sqrt(pct.up$av_incline)),1) * pct.up$total

        #Uptake for scheme is
        uptake <- data.frame(scheme = j, census = sum(pct.up$pct.census), model.now = sum(pct.up$model.before), model.future = sum(pct.up$model.after))
        uptake.list[[j]] <- uptake

        message(paste0("Done scheme ",j," at ",Sys.time()))

      }

      uptake.fin <- do.call("rbind",uptake.list)
      uptake.fin$pup <- uptake.fin$model.future / uptake.fin$model.now
      uptake.fin$expect <- round(uptake.fin$pup * uptake.fin$census,0)

      #saveRDS(pct.up,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct-up.Rds"))
      saveRDS(uptake.fin,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct-up.Rds"))

      #Save results
      #if(overwrite){
        #saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
        #saveRDS(schemes,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes.Rds"))
      #}else{
        #saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines-schemes.Rds"))
        #saveRDS(schemes,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes.Rds"))
      #}
      rm(osm,schemes)

    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)




















# Test of calcualting uptake for each scenario in bristol

# set up


















