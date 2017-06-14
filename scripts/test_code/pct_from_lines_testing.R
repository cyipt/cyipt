a = 28568
#a = 6094

for(a in 300:350){
  osm_sub <- osm[a,]
  #Trim off the ends of the line
  points <- st_cast(osm_sub$geometry, "POINT")
  points <- points[c(1,length(points))] #Get first and last point on the lines
  len <- len <- as.numeric(st_distance(points[1],points[2])) #Change to distance between points to deal with curved roads
  if(len < 4){ # To hanel very small lines
    cutlen <- len/2.5
  }else{
    cutlen <- 2
  }
  buff <- st_buffer(points, cutlen) #Make small circiels around the ends
  buff <- st_union(buff)
  osm_sub <- st_difference(osm_sub, buff) # Cut off the ends
  #rm(points,buff, len, cutlen)
  #Get grid IDS
  gridid <- grid_osm[a][[1]]
  rf_grid <- grid_rf[gridid]
  rf_grid <- unlist(rf_grid)
  rf_grid <- rf_grid[!duplicated(rf_grid)]
  rf_presub <- rf[rf_grid,]
  sel <- st_intersects(osm_sub, rf_presub)[[1]]

  #Test Plotting###########
  testbuf <- st_buffer(osm_sub, 1)
  plot(testbuf[1], col = "White", lwd = 0.1)
  plot(osm_sub[1], add = T, col = "Black", lwd = 8)
  plot(rf_presub[1], add = T, col = "Red")


  if(sum(lengths(sel)) == 0){
    #SOmething the lines run paralelle very colse to each other
    #split the buff and check for intersection at both ends
    buff <- st_cast(buff,"POLYGON", group_or_split = T)
    if(length(buff) == 1){
      #Do nothing, edge case where a looped road exists whith same start and end point
      count <- 0
    }else{
      sel2 <- st_intersects(buff[1], rf_presub)[[1]]
      sel3 <- st_intersects(buff[2], rf_presub)[[1]]
      sel4 <- sel2[sel2 %in% sel3]
      if(sum(lengths(sel4)) == 0){
        #Do Nothing
        count <- 0
      }else{
        #Check for cul-de-sacs where road toched the same road at both ends
        grd <- grid_osm[[a]]
        #osm_other <- osm[grid_osm %in% grd,]
        osm_other <- osm[sapply(grid_osm,function(x)any(x %in% grd)),] #Needed to hand lines in multiple grids
        osm_other1 <- osm_other[unique(unlist(st_intersects(buff[1],osm_other))),]
        osm_other2 <- osm_other[unique(unlist(st_intersects(buff[2],osm_other))),]
        osm_other1 <- osm_other1[!(osm_other1$id %in% a),]
        osm_other2 <- osm_other2[!(osm_other2$id %in% a),]
        match <- osm_other1$id[osm_other1$id %in% osm_other2$id]
        if(length(match) == 0){
          #No cul-de-sac case
          lenother <- 0
        }else{
          lenother <- as.numeric(st_length(osm_other[osm_other$id == match,]))
        }
        #plot(osm_other1[1], add = T, col = "Green", lwd = 3)
        #plot(osm_other2[1], add = T, col = "Yellow", lwd = 3)
        if(lenother > len){
          #Cul-de-sac
          #Do Nothing
          count <- 0
        }else{
          #Split out the lines that are very close
          rf_sub <- rf_presub[sel4,]
          cuts <- st_difference(rf_sub,buff)
          cutsl <- splitmulti(cuts, "MULTILINESTRING", "LINESTRING")
          cutsl$len <- as.numeric(st_length(cutsl))
          cutsl <- cutsl[cutsl$len > (0.95 * len) & cutsl$len < (1.05 * len),] #Get segments that are withing 5% lenf of the line
          count <- sum(rf_sub$bicycle_16p)
          #lengths(cuts_geom)
          plot(rf_sub, add = T, lwd = 2, col = "Green")
        }
      }
      rm(buff,sel2,sel3,sel4)
    }

  }else{
    plot(rf_presub[sel,], add = T, lwd = 2, col = "Green")
    rf_sub <- rf_presub[sel,]
    count <- sum(rf_sub$bicycle_16p)
  }
  Sys.sleep(1)
  print(paste0("For ",a," count is ",count))
}

names(rf_presub) <- c("ID","busyness","bicycle","geometry")

st_write(osm_sub,"../example-data/bristol/for_checking/osm_sub5.shp")
st_write(rf_presub, "../example-data/bristol/for_checking/rf_presub5.shp")
st_write(buff,"../example-data/bristol/for_checking/buff5.shp")


