#Demo Infrastrucutre data
library(sf)
library(dplyr)
#library(gdata)

#Read in Data
osm <- readRDS("../example-data/bristol/results/osm-lines.Rds")
vars <- read.csv("../example-data/bristol/results/osm-variables.csv")
pct <- read.csv("../example-data/bristol/results/pct-census.csv")
width <- read.csv("../example-data/bristol/results/widths.csv")
traffic <- read.csv("../cyipt/trafficcounts/trafficcounts-osm.csv")
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
osm <- left_join(osm,traffic, by = c("osm_id" = "osm_id"))

rm(vars,pct,width,traffic)

#Step 1: Remove Reads with low propencity to cycle
osm <- osm[osm$pct_census > 9,]

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

#Step 3: Remove NA values
osm$aadt[is.na(osm$aadt)] <- 0


#Step 4; Compare Against Rules Table
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



saveRDS(osm,"../example-data/bristol/results/osm-select-infra.Rds")

