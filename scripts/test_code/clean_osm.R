#Clean Up OSM Description

osm <- readRDS("../example-data/bristol/results/osm-select-infra.Rds")
osm <- osm[,c("id","bicycle","bicycle.oneway","bus_lane","busway","busway.left","busway.right",
              "cycleway", "cycleway.left","cycleway.otherside","cycleway.right","designation" ,
              "foot","footway","highway","junction","oneway","oneway.bicycle","roundabout","segregated","service"  ,
              "shared","sidewalk",
              "geometry")]

summary(osm)

osm$cycleway <- as.character(osm$cycleway)
osm$cycleway.left <- as.character(osm$cycleway.left)
osm$busway.left <- as.character(osm$busway.left)
osm$cycleway.right <- as.character(osm$cycleway.right)
osm$busway.right <- as.character(osm$busway.right)


osm$leftside <- NA



for(a in 1:nrow(osm)){
  if(!is.na(osm$cycleway.left[a])){
    osm$leftside[a] <- osm$cycleway.left[a]
  }else if(!is.na(osm$cycleway[a])){
    if(osm$cycleway[a] == "No"){
      #Do Nothing
    }else if(osm$cycleway[a] == "opposite" | osm$cycleway[a] == "opposite_lane" | osm$cycleway[a] == "opposite_track"){
      #Do nothing as this is on the right side
    }else{
      osm$leftside[a] <- osm$cycleway[a]
    }
  }else if(!is.na(osm$busway.left[a])){
    osm$leftside[a] <- paste0("bus - ",osm$cycleway.left[a])
  }
}

osm$rightside <- NA

for(a in 1:nrow(osm)){
  if(!is.na(osm$cycleway.right[a])){
    osm$rightside[a] <- osm$cycleway.right[a]
  }else if(!is.na(osm$cycleway[a])){
    if(osm$cycleway[a] == "No"){
      #Do Nothing
    }else if(osm$cycleway[a] == "opposite" | osm$cycleway[a] == "opposite_lane" | osm$cycleway[a] == "opposite_track"){
      #Do nothing as this is on the right side
      osm$rightside[a] <- osm$cycleway[a]
    }else{
      osm$rightside[a] <- osm$cycleway[a]
    }
  }else if(!is.na(osm$busway.right[a])){
    osm$rightside[a] <- paste0("bus - ",osm$cycleway.right[a])
  }
}

#Cleanup Results
osm$rightside[is.na(osm$rightside)] <- "None"
osm$rightside[osm$rightside == "no"] <- "None"
osm$rightside[osm$rightside == "lane" | osm$rightside == "opposite" | osm$rightside == "opposite_lane" | osm$rightside == "yes" | osm$leftside == "designated"] <- "Lane"
osm$rightside[osm$rightside == "share_busway" | osm$rightside == "shared"] <- "Shared"
osm$rightside[osm$rightside == "track" | osm$rightside == "opposite_track" ] <- "Track"

osm$leftside[is.na(osm$leftside)] <- "None"
osm$leftside[osm$leftside == "no"] <- "None"
osm$leftside[osm$leftside == "lane" | osm$leftside == "opposite" | osm$leftside == "opposite_lane" | osm$leftside == "yes" | osm$leftside == "designated"] <- "Lane"
osm$leftside[osm$leftside == "share_busway" | osm$leftside == "shared"] <- "Shared"
osm$leftside[osm$leftside == "track" | osm$leftside == "opposite_track" ] <- "Track"

osm$roadtype <- NA

for(a in 1:nrow(osm)){
  #No Cycling on motorways
  if(osm$highway[a] == "motorway" |
     osm$highway[a] == "motorway_link"){
    osm$roadtype[a] <- "Road - Cycling Forbidden"
  #Normal Roads
  }else if(osm$highway[a] == "primary" |
           osm$highway[a] == "primary_link" |
           osm$highway[a] == "secondary" |
           osm$highway[a] == "secondary_link" |
           osm$highway[a] == "tertiary" |
           osm$highway[a] == "tertiary_link" |
           osm$highway[a] == "trunk" |
           osm$highway[a] == "trunk_link" |
           osm$highway[a] == "service" |
           osm$highway[a] == "road" |
           osm$highway[a] == "residential" |
           osm$highway[a] == "unclassified" ){
    osm$roadtype[a] <- "Road - Cycling Allowed"
  #Living Streets
  }else if(osm$highway[a] == "living_street"){
    osm$roadtype[a] <- "Living Street"
  #Cycleways
  }else if(osm$highway[a] == "cycleway"){
    osm$roadtype[a] <- "Cycleway"
  #Off Road - Check if cycling is allowed
  }else if(osm$highway[a] == "bridleway" |
           osm$highway[a] == "construction" |
           osm$highway[a] == "demolished" |
           osm$highway[a] == "escalator" |
           osm$highway[a] == "footway" |
           osm$highway[a] == "path" |
           osm$highway[a] == "pedestrian" |
           osm$highway[a] == "steps" |
           osm$highway[a] == "track")
    if(is.na(osm$bicycle[a])){
      osm$roadtype[a] <- "Shared Path"
    }else if(osm$bicycle[a] == "No" | osm$bicycle[a] == "dismount" ){
      osm$roadtype[a] <- "Path - Cycling Forbidden"
    }else{
      osm$roadtype[a] <- "Shared Path"
    }
}

osm$roadtype2 <- paste0(osm$roadtype," ",osm$leftside," ",osm$rightside)



sumry <- osm[,c("id","roadtype","leftside","rightside","roadtype2")]
st_write(sumry,"../example-data_old/bristol/simpleroad2.shp")


