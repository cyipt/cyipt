#Code has stagne bug where when no os polygons roads are sometimes given widht of 11.4 and width path 22.5

#Load In functions
source("R/functions.R")



#Estimate road widths
library(sf)
library(dplyr)

#Read in data
os <- readRDS("../example-data/bristol/os_data/roads.Rds")
osm <- readRDS("../example-data/bristol/osm_data/osm-split.Rds")

#test subsetting
#osm <- osm[1:1000,]

#Big Function
getroadwidths <- function(a){
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

  #Split Muilpart into single part
  roadside <- multi2single(roadside,"MULTIPOLYGON","POLYGON")
  road <- multi2single(road,"MULTIPOLYGON","POLYGON")

  #print(paste0("Doing Line ",a," of ",nrow(osm)," with ",nrow(road)," roads and ",nrow(roadside)," roadsides at ", Sys.time()))

  #Get Road Width
  if(nrow(road) == 0){
    #print("Number of Roads Polygons is Zero")
    #Make some empty objects as they get checked aginst later
    widthres <- NA
    touch <- list()
    road_main <- data.frame()
    #line_main <- line
  }
  else{

    road$width <- width_estimate(road)

    ##Find intersections
    #Get Intersection Points
    road_str <- st_cast(road, "MULTILINESTRING", group_or_split=TRUE)
    osm_inter <- st_intersection(line,road_str)
    #osm_inter <- osm_inter$geometry
    rm(road_str)

    #Check if line crosses polygon boundaries, if so then split line and take longest part
    if(length(osm_inter) == 0){
      #print("Line does not cross polygon boundaries")
      line_main <- line
    }else{
      #Split Points and Mulitpoints
      #print("Splitting Line")
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
    #print(paste0("Road width is ",osm_join$width[1]," of ",nrow(osm_join)," possible values"))
    widthres <- osm_join$width[1]

    #Select roaddside touching the road section
    road_main <- road[road$id == osm_join$id[1], ]
    road_main <- road_main[!is.na(road_main$id),]
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
    #print("Getting Roadside width with simple case")

    roadside_touch <- roadside[st_touches(road_main, roadside)[[1]],]
    #If Roadside is made up of multiple polygons merge them alltogther
    roadside_one <- st_union(roadside_touch)
    roadside_touch <- roadside_touch[1,]
    roadside_touch$geometry <- roadside_one
    rm(roadside_one)

    #Get Path width
    roadside_touch$width <- width_estimate(roadside_touch)

    road_buff <- st_buffer(road_main, dist = (1.1 * roadside_touch$width[1]))
    roadside_touch <- st_intersection(road_buff, roadside_touch)
    comb <- st_union(roadside_touch, road_main)
    comb <- comb[,c("id","geometry")]
    comb <- st_buffer(comb,dist = 0.0001) #More robust way to get a single polygon
    #comb <- st_cast(comb, "POLYGON", group_or_split=TRUE)

    #Get final width
    comb$width <- width_estimate(comb)

    #Update table
    #print(paste0("Road and roadside combined width is ",comb$width[1]," of ",nrow(comb)," possible values"))
    widthpathres <- comb$width[1]
    rm(roadside_touch, roadside, road_main, road_buff, road, comb, line, e, touch)
  }
  else if(nrow(roadside) > 0 & nrow(road_main) == 0){
    #For cases where there is no road but their is roadside e.g. off road cycle path
    #print("Check for roadside only situation")
    roadside_only <- roadside[st_intersects(line,roadside)[[1]],]
    if(nrow(roadside_only) == 0){
      #print("No roadside only situation")
      widthpathres <- NA
    }
    else{
      roadside_only$width <- width_estimate(roadside_only)

      #Update table
      widthpathres <- roadside_only$width[1]
      #print(paste0("Roadside only width is ",roadside_only$width[1]," of ",nrow(roadside_only)," possible values"))
      rm(touch, roadside, road_main, road, line)
    }

  }
  else{
    #print("Unable to find roadside width")
    widthpathres <- NA
    rm(touch, roadside, road_main, road, line)
  }

  finalres <- c(widthres,widthpathres)
  return(finalres)
  rm(finalres,widthres,widthpathres)
}

#Apply FUnction

starttime <- Sys.time()

res <- lapply(1:nrow(osm), getroadwidths)
res <- do.call("rbind", res)

#Add Results Column
osm$width <- res[,1]
osm$widthpath <- res[,2]

endtime <- Sys.time()

print(paste0("Did ",nrow(osm)," rows in ", round(difftime(endtime, starttime, units = "secs"),2), " seconds"))





saveRDS(osm, "../example-data/bristol/osm_data/osm-split-roadwidths.Rds")
#sub <- as.data.frame(osm)
#sub <- sub[,c("id","osm_id","name","width","widthpath")]
#write.csv(sub,"../example-data/bristol/osm_data/osm-split-roadwiths.csv")
