#Estimate road widths
library(sf)
library(dplyr)

#Before Running Check and Repair Geometry of OS data using ArcGIS Repair Geometry Tool
#Before runing Delete duplicate geometrys
#Before running convert mulipart to single part

#Read in data drop unneded values
boundary <- st_read(dsn = "areas/bristol-poly.geojson")
boundary <- st_transform(boundary, 27700) #Change to British Nat Grid
os <- st_read(dsn = "D:/roadwidth", layer = "SWroads") #1118955
os <- os[,c("OBJECTID","DESCGROUP","geometry")] #Dump Uneeded Columns
os <- st_transform(os, 27700) #Change to British Nat Grid
os <- os[boundary,] #18985 lines
os <- os[st_is_valid(os),] #remove invalid geometry #18944

#Read in OSM Lines
#Can't figure out how to split lines at each intersection in R so did it in ArcGIS
osm <- st_read(dsn ="../example-data/bristol",layer = "osm_split")
osm <- st_transform(osm, 27700) #Change to British Nat Grid
osm <- osm[boundary,] #35257
osm <- osm[,c("osm_id","name","geometry")]
osm <- osm[st_is_valid(osm),] #remove invalid geometry #18944
remove(boundary)

#Add in a unique idea for each segment of and OSM line
osm$id <- c(1:nrow(osm))
osm$width <- NA
osm$widthpath <- NA

#New Combined Loop
for(a in 1:nrow(osm)){
  linemarker <- 34
  #Get OSM Line and OS Polygons Near By
  osm_sub <- osm[a,] #OSM Line
  AOI <- st_buffer(osm_sub, dist = 15) # Area intrest around the line, set to 15 m
  os_sub <- os[st_intersects(AOI,os)[[1]],] # Get OS Polys that intersect the AOI
  plot(AOI[,1], col = "White")
  plot(os_sub[,1], add = T, col = "White")
  plot(osm_sub[,1], add = T, col = "Blue", lwd = 3)
  print(paste0("Doing Line ",a," of ",nrow(osm)," at ", Sys.time()))
  Sys.sleep(0.5)
  if(nrow(os_sub) > 0){
    ####Find Road Width
    ##Select Road that interset the line
    road <- os_sub[os_sub$DESCGROUP == "Road Or Track" | os_sub$DESCGROUP == "Path",]
    road <- road[st_intersects(osm_sub,road)[[1]],]

    #Get Road Dimentions
    road$area <- as.numeric(st_area(road))
    road$perimeter <- as.numeric(st_length(road, dist_fun = geosphere::distGeo))
    road$width <- NA
    linemarker <- 52
    #Use accuate raod width if possible lese use approximate
    for(c in 1:nrow(road)){
      if((road$perimeter[c] ^ 2) > (16 * road$area[c])){
        road$width[c] <- round(0.25 * (road$perimeter[c] - sqrt((road$perimeter[c] ^ 2) - (16 * road$area[c]))), 1)
      }else{
        road$width[c] <- round(2 * road$area[c] / road$perimeter[c], 1)
      }
    }
    #plot(road[,1], add = T, col = "Green")

    ##Find intersections
    #Get Intersection Points
    road_str <- st_cast(road, "MULTILINESTRING", group_or_split=TRUE)
    osm_inter <- st_intersection(osm_sub,road_str)
    osm_inter <- osm_inter$geoms
    linemarker <- 68
    if(length(osm_inter) > 0){ #If no interactions do nothing
      #Split Points and Mulitpoints
      #SF can't hangle mix of points and mulitpoints so have to be split out and then rejoined
      pORmp <- vector(mode = "logical",length = length(osm_inter))
      for(b in 1:length(osm_inter)){
        pORmp[[b]] <- any(class(osm_inter[[b]]) == "MULTIPOINT")
      }
      osm_inter_mp <- osm_inter[pORmp]
      osm_inter_p <- osm_inter[!pORmp]
      rm(pORmp, osm_inter,road_str)

      #Convert Multipoints into single points
      osm_inter_mp <- st_cast(st_sfc(osm_inter_mp), "POINT", group_or_split = TRUE)
      osm_inter_p <- st_cast(st_sfc(osm_inter_p), "POINT", group_or_split = TRUE)
      linemarker <- 83
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
      linemarker <- 95
      #Buffer Pointsand make into a singe mulipolygon
      osm_buff <- st_buffer(inter_df, dist = 0.01)
      buff_geom <- osm_buff$geom
      buff_geom <- st_union(buff_geom)
      rm(osm_buff, inter_df)
      plot(buff_geom, add = T, col = "Red")

      #Cut the line with buffered points
      osm_diff <- st_cast(st_difference(osm_sub,buff_geom), "LINESTRING")
      osm_diff$length <- as.numeric(st_length(osm_diff, dist_fun = geosphere::distGeo))
      linemarker <- 106
      #Select the right segment of the line
      osm_right <- osm_diff[osm_diff$length == max(osm_diff$length),]
      osm_right <- osm_right[,c("id","geometry")]
      osm_join <- st_join(osm_right,road, join = st_intersects, left = TRUE)
      plot(osm_join, add = T, col = "Blue")

      if(!is.na(osm_join$width[1])){
        #Update Table
        osm$width[a] <- osm_join$width[1]

        linemarker <- 116
        ###################################
        #### Get Path Width



        #Select raodside touching the road section
        road_main <- road[road$OBJECTID == osm_join$OBJECTID[1], ]
        #rm(osm_diff, osm_right, osm_join)

        roadside <- os_sub[os_sub$DESCGROUP == "Roadside", ]
        roadside <- st_intersection(AOI, roadside)
        if(nrow(roadside) > 0){
          roadside_geom <- roadside$geoms
          linemarker <- 127
          #Split MulitPolygons into Single Polygons
          #SF can't hangle mix of points and mulitpoints so have to be split out and then rejoined
          pORmp <- vector(mode = "logical",length = length(roadside_geom))
          for(d in 1:length(roadside_geom)){
            pORmp[[d]] <- any(class(roadside_geom[[d]]) == "MULTIPOLYGON")
          }
          roadside_mp <- roadside_geom[pORmp]
          roadside_p <- roadside_geom[!pORmp]
          rm(pORmp, roadside, roadside_geom)
          linemarker <- 137
          #Convert Multipolygons into single polygons
          roadside_mp <- st_cast(st_sfc(roadside_mp), "POLYGON", group_or_split = TRUE)
          roadside_p <- st_cast(st_sfc(roadside_p), "POLYGON", group_or_split = TRUE)

          #Put polygons back togther
          roadside_geom <- c(roadside_p,roadside_mp)
          roadside_geom <- st_cast(st_sfc(roadside_geom), "POLYGON", group_or_split = TRUE) #Incase mp or p is empty have to run again
          rm(roadside_p, roadside_mp)
          linemarker <- 146
          #Remove Duplicates
          roadside_geom <- roadside_geom[!duplicated(roadside_geom)]
          roadside <- data.frame(id = c(1:length(roadside_geom)))
          roadside$geom <- roadside_geom
          roadside <- st_sf(roadside)
          #rm(roadside_geom)

          #Remove Roadside that does not touch the road
          roadside <- roadside[st_touches(road_main, roadside)[[1]],]
          linemarker <- 156

          if(nrow(roadside) > 0){
            #If Roadside is made up of multiple polygons merge them alltogther
            roadside_one <- st_union(roadside)
            roadside <- roadside[1,]
            roadside$geom <- roadside_one

            #Get Path width
            roadside$area <- as.numeric(st_area(roadside))
            roadside$perimeter <- sum(as.numeric(st_length(roadside, dist_fun = geosphere::distGeo)))
            roadside$width <- NA
            linemarker <- 166
            #Use accuate raod width if possible lese use approximate
            for(e in 1:nrow(roadside)){
              if((road$perimeter[e] ^ 2) > (16 * roadside$area[e])){
                roadside$width[e] <- round(0.25 * (roadside$perimeter[e] - sqrt((roadside$perimeter[e] ^ 2) - (16 * roadside$area[e]))), 1)
              }else{
                roadside$width[e] <- round(2 * roadside$area[e] / roadside$perimeter[e], 1)
              }
            }

            road_buff <- st_buffer(road_main, dist = (1.1 * roadside$width[1]))
            roadside <- st_intersection(road_buff, roadside)
            comb <- st_union(roadside, road_main)
            comb <- comb[,c("id","geoms")]
            linemarker <- 180
            #Get final width
            comb$area <- as.numeric(st_area(comb))
            comb$perimeter <- sum(as.numeric(st_length(comb, dist_fun = geosphere::distGeo)))
            comb$width <- NA

            #Use accuate raod width if possible lese use approximate
            for(e in 1:nrow(comb)){
              if((road$perimeter[e] ^ 2) > (16 * comb$area[e])){
                comb$width[e] <- round(0.25 * (comb$perimeter[e] - sqrt((comb$perimeter[e] ^ 2) - (16 * comb$area[e]))), 1)
              }else{
                comb$width[e] <- round(2 * comb$area[e] / comb$perimeter[e], 1)
              }
            }
            linemarker <- 194-/
            #Update table
            osm$widthpath[a] <- comb$width[1]
          }
        }


      }
    }
  }

}

