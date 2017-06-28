#Demo Infrastrucutre data
library(sf)
library(dplyr)
#library(gdata)

#Read in Data
osm <- readRDS("../example-data/cambridge/results/osm-lines.Rds")
vars <- read.csv("../example-data/cambridge/results/osm-variables.csv")
pct <- read.csv("../example-data/cambridge/results/pct-census.csv")
#width <- read.csv("../example-data/cambridge/results/widths.csv")
#traffic <- read.csv("../cyipt/trafficcounts/trafficcounts-osm.csv")
rules <- read.csv("../cyipt/input-data/InfraSelectionRules.csv", stringsAsFactors = FALSE)

#Remove Unneded Columns
vars$X <- NULL
vars$osm_id <- NULL
pct$osm_id <- NULL
pct$X <- NULL
#width$osm_id <- NULL
#width$X <- NULL

#Join Togther
osm <- left_join(osm,vars, by = c("id" = "id"))
osm <- left_join(osm,pct, by = c("id" = "id"))
#osm <- left_join(osm,width, by = c("id" = "id"))
#osm <- left_join(osm,traffic, by = c("osm_id" = "osm_id"))

rm(vars,pct,width,traffic)

#Step 1: Remove Roads with low propencity to cycle
#osm <- osm[osm$pct_census > 9,]

#############################################################################################
#Step 2: Guess Road Speed if one not provided
summary(osm$maxspeed)
for(a in 1:nrow(osm)){
  if(is.na(osm$maxspeed[a])){
    type <- osm$highway[a]
    if(type == "motorway" | type == "motorway_link"){
      osm$maxspeed[a] <- "70 mph"
    }else if(type == "trunk" | type == "trunk_link"){
      osm$maxspeed[a] <- "60 mph"
    }else if(type == "primary" | type == "residential" | type == "road" | type == "primary_link" | type == "secondary" | type == "secondary_link" | type == "tertiary" | type == "tertiary_link"){
      osm$maxspeed[a] <- "30 mph"
    }else if(type == "service" ){
      osm$maxspeed[a] <- "20 mph"
    }else if(type == "bridleway" | type ==  "construction" | type ==  "cycleway" | type ==  "demolished" | type ==  "escalator" | type ==  "footway" | type ==  "living_street" | type ==  "steps" | type ==  "track" | type ==  "unclassified"){
      osm$maxspeed[a] <- "10 mph"
    }else{
      osm$maxspeed[a] <- "60 mph"
    }
  }
}
summary(osm$maxspeed)

osm$speed <- NA
for(c in 1:nrow(osm)){
  if(osm$maxspeed[c] == "70 mph" | osm$maxspeed[c] == "70" ){
    osm$speed[c] <- 70
  }else if(osm$maxspeed[c] == "60 mph"| osm$maxspeed[c] == "60" ){
    osm$speed[c] <- 60
  }else if(osm$maxspeed[c] == "50 mph"| osm$maxspeed[c] == "50" ){
    osm$speed[c] <- 50
  }else if(osm$maxspeed[c] == "40 mph"| osm$maxspeed[c] == "40" ){
    osm$speed[c] <- 40
  }else if(osm$maxspeed[c] == "30 mph"| osm$maxspeed[c] == "30" ){
    osm$speed[c] <- 30
  }else if(osm$maxspeed[c] == "20 mph"| osm$maxspeed[c] == "20" ){
    osm$speed[c] <- 20
  }else if(osm$maxspeed[c] == "10 mph"| osm$maxspeed[c] == "10" ){
    osm$speed[c] <- 10
  }else if(osm$maxspeed[c] == "15 mph"| osm$maxspeed[c] == "15" ){
    osm$speed[c] <- 15
  }else if(osm$maxspeed[c] == "5 mph"| osm$maxspeed[c] == "5" ){
    osm$speed[c] <- 5
  }else{
    osm$speed[c] <- 30
  }
}

rm(a,c,type)
###########################################################################
#Step 3: Remove NA values
osm$aadt[is.na(osm$aadt)] <- 0

#######################################################################
#Step 4: Summarise Existing Infrastructure
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
  else{
    osm$roadtype[a] <- "Other Road Type"
  }
}

osm$roadtype2 <- paste0(osm$roadtype," ",osm$leftside," ",osm$rightside)

###########################################################################################################
#Step 5; Compare Against Rules Table
osm$infra_score <- NA
not_road <- c("bridleway","construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track")
for(b in 1:nrow(osm)){
  if(osm$highway[b] %in% not_road){
    if(osm$pct_census[b] > 50){
      osm$infra_score[b] <- "Track/Path"
    }else{
      osm$infra_score[b] <- "None"
    }

  }else{
    osm$infra_score[b] <- rules$Cycle.Route.Provision[rules$speed_min < osm$speed[b] &
                                                        rules$speed_max >= osm$speed[b] & #Nb equals different for speed as speedlimits are usually at maximum end
                                                        rules$pct_min <= osm$pct_census[b] &
                                                        rules$pct_max > osm$pct_census[b] &
                                                        rules$AADT_min <= osm$aadt[b] &
                                                        rules$AADT_max > osm$aadt[b]
                                                      ]
  }

}



#Step 6: Compare existing and proposed
osm$action <- paste0(osm$roadtype2," -> ",osm$infra_score)
costs <- read.csv("../cyipt/input-data/costs3.csv", stringsAsFactors = FALSE)
#costs$id <- NULL

#stop()

osm <- left_join(osm,costs, by = c("action" = "type"))
#Step 7: Find lenghts and total costs
osm$length <- as.numeric(st_length(osm))
osm$cost.total <- as.integer(osm$cost.per.m * osm$length)

#test <- osm[osm$infra_score != "None",]




tm_shape(osm[osm$change != "no need",]) +
  tm_lines(col = "change", lwd = 5, alpha = 1,
           title.col = "Change",
           popup.vars = c("infra_score","roadtype2", "cost.total"))




saveRDS(osm,"../example-data/cambridge/results/osm-select-infra.Rds")
#plot(test[test$infra_score == "None",])
