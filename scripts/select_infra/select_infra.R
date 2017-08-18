#Gets PCT Values for the road segments

############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################

library(sf)
library(dplyr)
library(parallel)


#Settings now come from master file
#skip <- FALSE #Skip Files that already have PCT values
#ncores <- 4 #number of cores to use in parallel processing
#overwrite <- FALSE #Overwrite or create new file

#Functions




#List folders
#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo

rules <- read.csv("../cyipt/input-data/InfraSelectionRules.csv", stringsAsFactors = FALSE)

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
    #Check if PCT values exist in the file
    if(all(c("FILL ME IN") %in% names(osm)) & skip){
      message(paste0("infrastructure types values already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting infrastructure types values for ",regions[b]," at ",Sys.time()))

      #If overwriting remove old data
      col.to.keep <- names(osm)[!names(osm) %in% c("FILL ME IN")]
      osm <- osm[,col.to.keep]
      rm(col.to.keep)

      #Step 1: Remove Roads with low propencity to cycle
      osm.working <- osm
      osm.working <- osm.working[osm.working$pct.census > 9,]




      #Save results
      if(overwrite){
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
      }else{
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines-pct.Rds"))
      }
      rm(osm,pct.all,respar)



    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)




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
