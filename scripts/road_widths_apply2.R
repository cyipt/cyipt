#Estimate road widths
library(sf)
library(dplyr)

#Load In functions
source("R/functions.R")

#Read in data
os <- readRDS("../example-data/bristol/os_data/roads.Rds")
osm <- readRDS("../example-data/bristol/osm_data/osm-split-pct-2011.Rds")
osm <- osm[osm$pct_census >= 10,]

os_cent <- st_centroid(os)
osm_cent <- st_centroid(osm)
grid <- st_make_grid(osm, n = c(10,10), "polygons")

grid_osm <- st_intersects(osm_cent, grid)
grid_os <- st_intersects(grid, os_cent)

options(nwarnings = 10000)


#Big Function
getroadwidths <- function(a){
  #warning(paste0("Doing Line ",a))
  line <- osm[a,] #OSM Line

  #Pre subset for speed
  gridno <- grid_osm[[a]]
  os_grids <- grid_os[gridno][[1]]
  os_presub <- os[os_grids,]

  #plot(grid)
  #plot(line[1], col = "Red", lwd = 4, add = T)
  #plot(grid[gridno], lwd = 2, col = "Blue", add = T)
  #plot(os_presub[1], add = T, col = "Black")
  #plot(osm_cent[1,], add = T, col = "Green", lwd = 5)

  #Get OSM Line and OS Polygons Near By

  line <- line["geometry"]
  AOI <- st_buffer(line, dist = 15, nQuadSegs = 1) # Area intrest around the line, set to 15 m
  AOI <- AOI["geometry"]
  os_sub <- os_presub[st_intersects(AOI,os_presub)[[1]],] #Faster selection from smaller dataset
  rm(gridno,os_grids,os_presub)
  #system.time(os_sub <- os[st_intersects(AOI,os)[[1]],]) # Get OS Polys that intersect the AOI

  roadside <- os_sub[os_sub$DESCGROUP == "Roadside", ]
  roadside <- st_intersection(AOI, roadside)
  names(roadside) <- c(names(roadside)[1:2],"geometry")
  st_geometry(roadside) <- "geometry"
  roadside <- roadside[,c("OBJECTID","geometry") ]
  road <- os_sub[os_sub$DESCGROUP == "Road Or Track" | os_sub$DESCGROUP == "Path",]
  road <- road[,c("OBJECTID","geometry") ]
  road <- st_intersection(AOI, road)
  names(road) <- c("OBJECTID","geometry")
  st_geometry(road) <- "geometry"

  if(class(road$geometry)[[1]] == "sfc_GEOMETRY"){ #An edge case when the results come out as a geomtry (this ditches some data)
    road <- st_cast(road, "POLYGON")
  }
  road <- road[st_intersects(line,road)[[1]],]
  rm(os_sub, AOI)

  #THe big if statments
  if(nrow(road) == 0 & nrow(roadside) == 0){
    startint <- Sys.time()
    #No Data so skip everthing
    widthpathres <- NA
    widthres <- NA
    endint <- Sys.time()
    #warning(paste0("Did no data in ", round(difftime(endint, startint, units = "secs"),2), " seconds"))
  }else if(nrow(road) > 0 & nrow(roadside) > 0){
    #################################################################
    #Road and Roadside Approach
    startint <- Sys.time()
    roadside <- multi2single(roadside,"MULTIPOLYGON","POLYGON")
    road <- multi2single(road,"MULTIPOLYGON","POLYGON")

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

      #Buffer Pointsand make into a singe mulipolygon
      osm_buff <- st_buffer(osm_inter, dist = 0.01)
      buff_geom <- osm_buff$geom
      buff_geom <- st_union(buff_geom)
      rm(osm_buff, osm_inter)

      #Cut the line with buffered points
      osm_diff <- st_cast(st_difference(line,buff_geom), "LINESTRING")
      osm_diff$length <- as.numeric(st_length(osm_diff, dist_fun = geosphere::distGeo))

      #Select the right segment of the line
      line_main <- osm_diff[osm_diff$length == max(osm_diff$length),]
      rm(osm_diff, buff_geom)
    }

    #Join line and poyglon to get the road width
    line_main <- line_main[,"geometry", drop = FALSE]
    line_main$sub <- 1:nrow(line_main) #Can't do the join without a variaible
    osm_join <- st_join(line_main,road, join = st_intersects, left = TRUE)

    #Update Table
    widthres <- osm_join$width[1]

    #Select roaddside touching the road section
    road_main <- road[road$OBJECTID == osm_join$OBJECTID[1], ] # Changed from id to OBJECTID don't know why it used to be id
    road_main <- road_main[!is.na(road_main$OBJECTID),]
    rm(line_main, osm_join)
    if(nrow(road_main) == 0){
      #Can't do anything
      widthpathres <- NA
    }else{
      touch <- st_touches(road_main, roadside, sparse = FALSE)
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
        widthpathres <- width_estimate(comb)
        rm(roadside_touch, road_buff, comb, touch)
      }

    }
    rm(road_main)

    endint <- Sys.time()
    #warning(paste0("Did road and roadside in ", round(difftime(endint, startint, units = "secs"),2), " seconds"))
  }else if(nrow(road) > 0 & nrow(roadside) == 0){
    ########################################################
    #Road Only approach
    startint <- Sys.time()
    widthpathres <- NA
    road <- multi2single(road,"MULTIPOLYGON","POLYGON")
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

      #Buffer Pointsand make into a singe mulipolygon
      osm_buff <- st_buffer(osm_inter, dist = 0.01)
      buff_geom <- osm_buff$geom
      buff_geom <- st_union(buff_geom)
      rm(osm_buff, osm_inter)

      #Cut the line with buffered points
      osm_diff <- st_cast(st_difference(line,buff_geom), "LINESTRING")
      osm_diff$length <- as.numeric(st_length(osm_diff, dist_fun = geosphere::distGeo))

      #Select the right segment of the line
      line_main <- osm_diff[osm_diff$length == max(osm_diff$length),]
      rm(osm_diff, buff_geom)
    }

    #Join line and polygon to get the road width
    line_main <- line_main[,"geometry", drop = FALSE]
    line_main$sub <- 1:nrow(line_main) #Can't do the join without a variaible
    osm_join <- st_join(line_main,road, join = st_intersects, left = TRUE)

    #Update Table
    widthres <- osm_join$width[1]
    rm(line_main, osm_join)
    endint <- Sys.time()
    #warning(paste0("Did road only ", round(difftime(endint, startint, units = "secs"),2), " seconds"))
  }else if(nrow(road) == 0 & nrow(roadside) > 0){
    ######################################################
    #Roadside only approauch
    startint <- Sys.time()
    widthres <- NA
    roadside <- multi2single(roadside,"MULTIPOLYGON","POLYGON")
    roadside <- roadside[st_intersects(line,roadside)[[1]],] #Find Roadside that intersects the line
    if(nrow(roadside) == 0){
      widthpathres <- NA #None found
    }else{
      roadside$width <- width_estimate(roadside)
      widthpathres <- roadside$width[1]
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
  finalres <- c(widthres,widthpathres)
  return(finalres)
  rm(finalres,widthres,widthpathres, road, roadside, line)
}

#profvis({
#Apply FUnction
starttime <- Sys.time()
res <- lapply(1:nrow(osm), getroadwidths)
res <- do.call("rbind", res)

#Add Results Column
osm$width <- NA
osm$widthpath <- NA

for(b in 1:nrow(osm)){
  width <- res[b,1][[1]]
  widthpath <- res[b,2][[1]]
  if(is.null(width)){
    width <- NA
  }
  if(is.null(widthpath)){
    widthpath <- NA
  }
  osm$width[b] <- width
  osm$widthpath[b] <- widthpath
}


endtime <- Sys.time()
print(paste0("Did ",nrow(osm)," rows in ", round(difftime(endtime, starttime, units = "secs"),2), " seconds"))

#})

warnms <- warnings()
warnms <- names(warnms)
warnms <- warnms[warnms != "attribute variables are assumed to be spatially constant throughout all geometries"]


saveRDS(osm, "../example-data/bristol/osm_data/osm-split-roadwidths.Rds")
sub <- as.data.frame(osm)
sub <- sub[,c("id","osm_id","name","width","widthpath")]
write.csv(sub,"../example-data/bristol/osm_data/osm-split-roadwiths.csv")
st_write(osm[c("id","osm_id","name","width","widthpath")], "../example-data/bristol/for_checking/roadwidths8.shp")
