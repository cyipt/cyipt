#Estimate road widths
library(sf)
library(dplyr)

#Read in data
os <- readRDS("../example-data/bristol/os_data/roads.Rds")
osm <- readRDS("../example-data/bristol/osm_data/roads_osm.Rds")

#New Combined Loop
for(a in 1:nrow(osm)){
  #Get OSM Line and OS Polygons Near By
  line <- osm[a,] #OSM Line
  line <- line["geometry"]
  AOI <- st_buffer(line, dist = 15) # Area intrest around the line, set to 15 m
  AOI <- AOI["geometry"]
  os_sub <- os[st_intersects(AOI,os)[[1]],] # Get OS Polys that intersect the AOI
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

  #Clean Up Roadside Polygons
  if(nrow(roadside) == 0){
    print("No Roadside")
  }
  else{
    #Split MulitPolygons into Single Polygons
    roadside_geom <- roadside$geometry
    pORmp <- vector(mode = "logical",length = length(roadside_geom))
    for(d in 1:length(roadside_geom)){
      pORmp[[d]] <- any(class(roadside_geom[[d]]) == "MULTIPOLYGON")
    }
    roadside_mp <- roadside_geom[pORmp]
    roadside_p <- roadside_geom[!pORmp]
    rm(pORmp, roadside, roadside_geom, d)

    #Convert Multipolygons into single polygons
    roadside_mp <- st_cast(st_sfc(roadside_mp), "POLYGON", group_or_split = TRUE)
    roadside_p <- st_cast(st_sfc(roadside_p), "POLYGON", group_or_split = TRUE)

    #Put polygons back togther
    roadside_geom <- c(roadside_p,roadside_mp)
    roadside_geom <- st_cast(st_sfc(roadside_geom), "POLYGON", group_or_split = TRUE) #Incase mp or p is empty have to run again
    rm(roadside_p, roadside_mp)

    #Remove Duplicates
    roadside_geom <- roadside_geom[!duplicated(roadside_geom)]
    roadside <- data.frame(id = c(1:length(roadside_geom)))
    roadside$geometry <- roadside_geom
    roadside <- st_sf(roadside)
    remove(roadside_geom)
  }

  #Clean Up Road Polygons
  if(nrow(road) == 0){
    print("No Road")
  }
  else{
    #Split MulitPolygons into Single Polygons
    road_geom <- road$geometry
    pORmp <- vector(mode = "logical",length = length(road_geom))
    for(f in 1:length(road_geom)){
      pORmp[[f]] <- any(class(road_geom[[f]]) == "MULTIPOLYGON")
    }
    road_mp <- road_geom[pORmp]
    road_p <- road_geom[!pORmp]
    rm(pORmp, road, road_geom, f)

    #Convert Multipolygons into single polygons
    road_mp <- st_cast(st_sfc(road_mp), "POLYGON", group_or_split = TRUE)
    road_p <- st_cast(st_sfc(road_p), "POLYGON", group_or_split = TRUE)

    #Put polygons back togther
    road_geom <- c(road_p,road_mp)
    road_geom <- st_cast(st_sfc(road_geom), "POLYGON", group_or_split = TRUE) #Incase mp or p is empty have to run again
    rm(road_p, road_mp)

    #Remove Duplicates
    road_geom <- road_geom[!duplicated(road_geom)]
    road <- data.frame(id = c(1:length(road_geom)))
    road$geometry <- road_geom
    road <- st_sf(road)
    remove(road_geom)
  }





  print(paste0("Doing Line ",a," of ",nrow(osm)," with ",nrow(road)," roads and ",nrow(roadside)," roadsides at ", Sys.time()))

  #Get Road Width
  if(nrow(road) == 0){
    print("Number of Roads Polygons is Zero")
    #Make some empty objects as they get checked aginst later
    touch <- list()
    road_main <- data.frame()
  }
  else{
    #Get Road Dimentions
    road$area <- as.numeric(st_area(road))
    road$perimeter <- as.numeric(st_length(road, dist_fun = geosphere::distGeo))
    road$width <- NA

    #Use accuate road width if possible lese use approximate
    for(c in 1:nrow(road)){
      if((road$perimeter[c] ^ 2) > (16 * road$area[c])){
        road$width[c] <- round(0.25 * (road$perimeter[c] - sqrt((road$perimeter[c] ^ 2) - (16 * road$area[c]))), 1)
      }else{
        road$width[c] <- round(2 * road$area[c] / road$perimeter[c], 1)
      }
    }
    rm(c)

    ##Find intersections
    #Get Intersection Points
    road_str <- st_cast(road, "MULTILINESTRING", group_or_split=TRUE)
    osm_inter <- st_intersection(line,road_str)
    osm_inter <- osm_inter$geoms
    rm(road_str)

    #Check if line crosses polygon boundaries, if so then split line and take longest part
    if(length(osm_inter) == 0){
      #print("Line does not cross polygon boundaries")
      line_main <- line
    }else{
      #Split Points and Mulitpoints
      print("Splitting Line")
      #SF can't hangle mix of points and mulitpoints so have to be split out and then rejoined
      pORmp <- vector(mode = "logical",length = length(osm_inter))
      for(b in 1:length(osm_inter)){
        pORmp[[b]] <- any(class(osm_inter[[b]]) == "MULTIPOINT")
      }
      rm(b)
      osm_inter_mp <- osm_inter[pORmp]
      osm_inter_p <- osm_inter[!pORmp]
      rm(pORmp, osm_inter,road_str)

      #Convert Multipoints into single points
      osm_inter_mp <- st_cast(st_sfc(osm_inter_mp), "POINT", group_or_split = TRUE)
      osm_inter_p <- st_cast(st_sfc(osm_inter_p), "POINT", group_or_split = TRUE)

      #Put points back togther
      osm_inter <- c(osm_inter_p,osm_inter_mp)
      osm_inter <- st_cast(st_sfc(osm_inter), "POINT", group_or_split = TRUE) #Incase mp or p is empty have to run again

      #Remove Duplicates
      inter_dup <- duplicated(osm_inter)
      osm_inter <- osm_inter[!inter_dup]
      inter_df <- data.frame(id = c(1:length(osm_inter)))
      inter_df$geom <- osm_inter
      inter_df <- st_sf(inter_df)
      rm(osm_inter,osm_inter_p,osm_inter_mp,inter_dup)

      #Buffer Pointsand make into a singe mulipolygon
      osm_buff <- st_buffer(inter_df, dist = 0.01)
      buff_geom <- osm_buff$geom
      buff_geom <- st_union(buff_geom)
      rm(osm_buff, inter_df)

      #Cut the line with buffered points
      osm_diff <- st_cast(st_difference(line,buff_geom), "LINESTRING")
      osm_diff$length <- as.numeric(st_length(osm_diff, dist_fun = geosphere::distGeo))

      #Select the right segment of the line
      line_main <- osm_diff[osm_diff$length == max(osm_diff$length),]
      rm(osm_diff, buff_geom)
    }

    #Join line and poyglon to get the road width
    line_main <- line_main[,"geometry"]
    line_main$sub <- 1:nrow(line_main)
    osm_join <- st_join(line_main,road, join = st_intersects, left = TRUE)

    #Update Table
    print(paste0("Road width is ",osm_join$width[1]," of ",nrow(osm_join)," possible values"))
    osm$width[a] <- osm_join$width[1]

    #Select roaddside touching the road section
    road_main <- road[road$OBJECTID == osm_join$OBJECTID[1], ]
    road_main <- road_main[!is.na(road_main$OBJECTID),]
    touch <- st_touches(road_main, roadside)
    if(length(touch) > 0){ #To deal with an Edge Case when Returns a empty list with lenght one
      if(sum(touch[[1]]) == 0 ){
        rm(touch)
        touch <- list()
      }
    }

    rm(line_main, osm_join)
  }

  ###################################
  # Get Path Width
  ###################################
  if(length(touch) > 0 & nrow(road_main) > 0){
    print("Getting Roadside width with simple case")

    roadside_touch <- roadside[st_touches(road_main, roadside)[[1]],]
    #If Roadside is made up of multiple polygons merge them alltogther
    roadside_one <- st_union(roadside_touch)
    roadside_touch <- roadside_touch[1,]
    roadside_touch$geometry <- roadside_one
    rm(roadside_one)

    #Get Path width
    roadside_touch$area <- as.numeric(st_area(roadside_touch))
    roadside_touch$perimeter <- sum(as.numeric(st_length(roadside_touch, dist_fun = geosphere::distGeo)))
    roadside_touch$width <- NA

    #Use accuate road width if possible lese use approximate
    for(e in 1:nrow(roadside_touch)){
      if((roadside_touch$perimeter[e] ^ 2) > (16 * roadside_touch$area[e])){
        roadside_touch$width[e] <- round(0.25 * (roadside_touch$perimeter[e] - sqrt((roadside_touch$perimeter[e] ^ 2) - (16 * roadside_touch$area[e]))), 1)
      }else{
        roadside_touch$width[e] <- round(2 * roadside_touch$area[e] / roadside_touch$perimeter[e], 1)
      }
    }

    road_buff <- st_buffer(road_main, dist = (1.1 * roadside_touch$width[1]))
    roadside_touch <- st_intersection(road_buff, roadside_touch)
    comb <- st_union(roadside_touch, road_main)
    comb <- comb[,c("id","geoms")]
    comb <- st_buffer(comb,dist = 0.0001) #More robust way to get a single polygon
    #comb <- st_cast(comb, "POLYGON", group_or_split=TRUE)

    #Get final width
    comb$area <- as.numeric(st_area(comb))
    comb$perimeter <- sum(as.numeric(st_length(comb, dist_fun = geosphere::distGeo)))
    comb$width <- NA

    #Use accuate road width if possible else use approximate
    for(e in 1:nrow(comb)){
      if((comb$perimeter[e] ^ 2) > (16 * comb$area[e])){
          comb$width[e] <- round(0.25 * (comb$perimeter[e] - sqrt((comb$perimeter[e] ^ 2) - (16 * comb$area[e]))), 1)
      }else{
          comb$width[e] <- round(2 * comb$area[e] / comb$perimeter[e], 1)
      }
    }

    #Update table
    print(paste0("Road and roadise combined width is ",comb$width[1]," of ",nrow(comb)," possible values"))
    osm$widthpath[a] <- comb$width[1]
    rm(roadside_touch, roadside, road_main, road_buff, road, comb, line, e, touch)
    }
  else if(nrow(roadside) > 0 & nrow(road_main) == 0){
    #For cases where there is no road but their is roadside e.g. off road cycle path
    print("Check for roadside only situation")
    roadside_only <- roadside[st_intersects(line,roadside)[[1]],]
    if(nrow(roadside_only) == 0){
      print("No roadside only situation")
    }
    else{
      roadside_only$area <- as.numeric(st_area(roadside_only))
      roadside_only$perimeter <- sum(as.numeric(st_length(roadside_only, dist_fun = geosphere::distGeo)))
      roadside_only$width <- NA

      #Use accuate road width if possible else use approximate
      for(e in 1:nrow(roadside_only)){
        if((roadside_only$perimeter[e] ^ 2) > (16 * roadside_only$area[e])){
          roadside_only$width[e] <- round(0.25 * (roadside_only$perimeter[e] - sqrt((roadside_only$perimeter[e] ^ 2) - (16 * roadside_only$area[e]))), 1)
        }else{
          roadside_only$width[e] <- round(2 * roadside_only$area[e] / roadside_only$perimeter[e], 1)
        }
      }

      #Update table
      osm$widthpath[a] <- roadside_only$width[1]
      print(paste0("Roadside only width is ",roadside_only$width[1]," of ",nrow(roadside_only)," possible values"))
      rm(touch, roadside, road_main, road, line)
    }

  }
  else{
    print("Unable to find roadside width")
    rm(touch, roadside, road_main, road, line)
  }
  rm(osm_inter)

}

saveRDS(osm, "../example-data/bristol/osm_data/roads_osm_widths.Rds")
