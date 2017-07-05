#Clean Up OSM Description

skip <- TRUE

#testing
osm <- readRDS("../example-data/bristol/results/osm-select-infra.Rds")
####

time <- 24*60*60

Sys.sleep(time)

#List folders

regions <- list.dirs(path = "../cyipt-bigdata/osm-prep", full.names = FALSE)

for(a in 2:length(regions)){
  print(paste0("Doing ",regions[a]," at ",Sys.time()))
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-variables.csv"))){
    if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-variables-clean.csv")) & skip){
      print("Skipping as already done")
    }else{
      osm <- read.csv(paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-variables.csv"))

      ################################################################################
      #Step 8: clean junctions
      osm$junction <- as.character(osm$junction)
      osm$junction[is.na(osm$junction)] <- "no"

      #################################################################################
      #Step 3: One way

      osm$onewaysummary <- NA
      osm$oneway.bicycle <- as.character(osm$oneway.bicycle)
      for(g in 1:nrow(osm)){
        if(osm$junction[g]  == "roundabout"){
          #Roundabouts are one way
          osm$onewaysummary[g] <- "One Way"
        }else{
          if(is.na(osm$oneway[g])){
            #If no direction info given
            osm$onewaysummary[g] <- "Two Way"
          }else{
            if(osm$oneway[g] == "Yes" | osm$oneway[g] == "yes" | osm$oneway[g] == "-1"){
              if(is.na(osm$oneway.bicycle[g])){
                osm$onewaysummary[g] <- "One Way"
              }else{
                if(osm$oneway.bicycle[g] == "no" | osm$oneway.bicycle[g] == "No"){
                  osm$onewaysummary[g] <- "One Way - Two Way Cycling"
                }else{
                  osm$onewaysummary[g] <- "One Way"
                }
              }
            }else if(osm$highway[g] %in% c("motorway","motorway_link","primary_link","secondary_link","tertiary_link","trunk_link")){
              #some road types are one way
              osm$onewaysummary[g] <- "One Way"
            }else{
              osm$onewaysummary[g] <- "Two Way"
            }
          }
        }
      }




      #############################################################################################
      #Step 1: Guess Road Speed if one not provided
      osm$maxspeed <- as.character(osm$maxspeed)

      for(b in 1:nrow(osm)){
        if(is.na(osm$maxspeed[b])){
          type <- osm$highway[b]
          if(type == "motorway" | type == "motorway_link"){
            osm$maxspeed[b] <- "70 mph"
          }else if(type == "trunk" | type == "trunk_link"){
            osm$maxspeed[b] <- "60 mph"
          }else if(type == "primary" | type == "residential" | type == "road" | type == "primary_link" | type == "secondary" | type == "secondary_link" | type == "tertiary" | type == "tertiary_link"){
            osm$maxspeed[b] <- "30 mph"
          }else if(type == "service" ){
            osm$maxspeed[b] <- "20 mph"
          }else if(type == "path" | type == "bridleway" | type ==  "construction" | type ==  "cycleway" | type ==  "demolished" | type ==  "escalator" | type ==  "footway" | type ==  "living_street" | type ==  "steps" | type ==  "track" | type ==  "unclassified"){
            osm$maxspeed[b] <- "10 mph"
          }else{
            osm$maxspeed[b] <- "60 mph"
          }
        }
      }


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

      #############################################################################
      #Step 7: Bus Lanes
      osm$busway.left <- as.character(osm$busway.left)
      osm$busway.right <- as.character(osm$busway.right)
      osm$busway <- as.character(osm$busway)

      for(l in 1:nrow(osm)){
        #Left side
        if(is.na(osm$busway.left[l])){
          if(!is.na(osm$busway[l])){
            osm$busway.left[l] <- osm$busway[l]
          }else{
            osm$busway.left[l] <- "no"
          }
        }else if(osm$busway.left[l] == "1"){
          osm$busway.left[l] <- "lane"
        }
        #Right side
        if(is.na(osm$busway.right[l])){
          if(!is.na(osm$busway[l])){
            osm$busway.right[l] <- osm$busway[l]
          }
          else{
            osm$busway.right[l] <- "no"
          }
        }else if(osm$busway.left[l] == "1"){
          osm$busway.right[l] <- "lane"
        }
      }



      #Check for missing bus lanes, where cycle lane is share_busway
      for(o in 1:nrow(osm)){
        if(!is.na(osm$cycleway.left[o])){
          if(osm$busway.left[o] == "no" & osm$cycleway.left[o] == "share_busway"){
            osm$busway.left[o] <- "lane"
          }
        }
        if(!is.na(osm$cycleway.right[o])){
          if(osm$busway.right[o] == "no" & osm$cycleway.right[o] == "share_busway"){
            osm$busway.right[o] <- "lane"
          }
        }
      }

      osm$busway.left <- as.factor(osm$busway.left)
      osm$busway.right <- as.factor(osm$busway.right)


      ##########################################################################
      # Step 2: Summaris Cycling Infrastrure

      osm$cycleway <- as.character(osm$cycleway)
      osm$cycleway.left <- as.character(osm$cycleway.left)
      osm$busway.left <- as.character(osm$busway.left)
      osm$cycleway.right <- as.character(osm$cycleway.right)
      osm$busway.right <- as.character(osm$busway.right)
      osm$leftside <- NA

      for(d in 1:nrow(osm)){
        if(osm$highway[d] %in% c("bridleway", "construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track")){
          #Left and right lanes are not a valid concept
          osm$leftside[d] <- "Not Applicable"
        }else{
          if(!is.na(osm$cycleway.left[d])){
            #Take the existing value
            osm$leftside[d] <- osm$cycleway.left[d]
          }else if(!is.na(osm$cycleway[d])){
            if(osm$cycleway[d] == "No"){
              #Do Nothing
            }else if(osm$cycleway[d] == "opposite" | osm$cycleway[d] == "opposite_lane" | osm$cycleway[d] == "opposite_track"){
              #Do nothing as this is on the right side
            }else{
              osm$leftside[d] <- osm$cycleway[d]
            }
          }#else if(!osm$busway.left[d] == "no"){
         #   osm$leftside[d] <- paste0("bus - ",osm$cycleway.left[d])
         # }
        }
      }

      osm$rightside <- NA

      for(e in 1:nrow(osm)){
        if(osm$highway[e] %in% c("bridleway", "construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track")){
          #Left and right lanes are not a valid concept
          osm$rightside[e] <- "Not Applicable"
        }else{
          if(!is.na(osm$cycleway.right[e])){
            osm$rightside[e] <- osm$cycleway.right[e]
          }else if(!is.na(osm$cycleway[e])){
            if(osm$cycleway[e] == "No"){
              #Do Nothing
            }else if(osm$cycleway[e] == "opposite" | osm$cycleway[e] == "opposite_lane" | osm$cycleway[e] == "opposite_track"){
              #Do something as this is on the right side
              osm$rightside[e] <- osm$cycleway[e]
            }else{
              osm$rightside[e] <- osm$cycleway[e]
            }
          }#else if(!osm$busway.right[e] == "no"){
          #  osm$rightside[e] <- paste0("bus - ",osm$cycleway.right[e])
          #}
        }
      }

      #Cleanup Results
      osm$rightside[is.na(osm$rightside)] <- "None"
      osm$rightside[osm$rightside == "no"] <- "None"
      osm$rightside[osm$rightside == "lane" | osm$rightside == "opposite" | osm$rightside == "opposite_lane" | osm$rightside == "yes" | osm$leftside == "designated"] <- "Lane"
      #osm$rightside[osm$rightside == "share_busway" | osm$rightside == "shared"] <- "Shared"
      osm$rightside[osm$rightside == "track" | osm$rightside == "opposite_track" ] <- "Track"

      osm$leftside[is.na(osm$leftside)] <- "None"
      osm$leftside[osm$leftside == "no"] <- "None"
      osm$leftside[osm$leftside == "lane" | osm$leftside == "opposite" | osm$leftside == "opposite_lane" | osm$leftside == "yes" | osm$leftside == "designated"] <- "Lane"
      #osm$leftside[osm$leftside == "share_busway" | osm$leftside == "shared"] <- "Shared"
      osm$leftside[osm$leftside == "track" | osm$leftside == "opposite_track" ] <- "Track"

      osm$roadtype <- NA

      for(f in 1:nrow(osm)){
        #No Cycling on motorways
        if(osm$highway[f] == "motorway" |
           osm$highway[f] == "motorway_link"){
          osm$roadtype[f] <- "Road - Cycling Forbidden"
          #Normal Roads
        }else if(osm$highway[f] == "primary" |
                 osm$highway[f] == "primary_link" |
                 osm$highway[f] == "secondary" |
                 osm$highway[f] == "secondary_link" |
                 osm$highway[f] == "tertiary" |
                 osm$highway[f] == "tertiary_link" |
                 osm$highway[f] == "trunk" |
                 osm$highway[f] == "trunk_link" |
                 osm$highway[f] == "service" |
                 osm$highway[f] == "road" |
                 osm$highway[f] == "residential" |
                 osm$highway[f] == "unclassified" ){
          osm$roadtype[f] <- "Road - Cycling Allowed"
          #Living Streets
        }else if(osm$highway[f] == "living_street"){
          osm$roadtype[f] <- "Living Street"
          #Cycleways
        }else if(osm$highway[f] == "cycleway"){
          osm$roadtype[f] <- "Cycleway"
          #Off Road - Check if cycling is allowed
        }else if(osm$highway[f] %in% c("bridleway","construction","footway","path","pedestrian","track")){
          if(is.na(osm$bicycle[f])){
            osm$roadtype[f] <- "Shared Path"
          }else if(osm$bicycle[f] == "No" | osm$bicycle[f] == "dismount" ){
            osm$roadtype[f] <- "Path - Cycling Forbidden"
          }else{
            osm$roadtype[f] <- "Shared Path"
          }
        }else if(osm$highway[f] %in% c("demolished","escalator","steps")){
          osm$roadtype[f] <- "Path - Cycling Forbidden"
        }

      }

      #
      #########################################################
      #New lanes Method
      ##########################################################
      for(i in 1:nrow(osm)){
        #Exclude Road types where lanes are no relevant
        if(osm$highway[i] %in% c("bridleway", "construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track","service")){
          osm$lanes.forward[i] <- 0
        }else{
          #All other road types
          if(is.na(osm$lanes.forward[i])){
            if(osm$onewaysummary[i] == "Two Way"){
              ###############################################
              #Two Way roads
              #Check for other lane info
              if(!is.na(osm$lanes[i]) & !is.na(osm$lanes.backward[i])){
                #Have total number of lanes and number in opposite direction so subtract
                osm$lanes.forward[i] <- osm$lanes[i] - osm$lanes.backward[i]
              }else if(!is.na(osm$lanes[i]) & is.na(osm$lanes.backward[i])){
                #Have total number of lanes but no other direction
                if(osm$lanes[i] %% 2 == 0){
                  #Even lanes so divide
                  osm$lanes.forward[i] <- osm$lanes[i]/2
                }else{
                  #No data so assign larger number
                  osm$lanes.forward[i] <- ceiling(osm$lanes[i]/2)
                }
              }else{
                #Insufficent info so guess
                if(osm$highway[i] == "motoway"){
                  osm$lanes.forward[i] <- 3
                }else if(osm$highway[i] == "trunk"){
                  osm$lanes.forward[i] <- 2
                }else if(osm$highway[i] %in% c("residential","living_street","tertiary","tertiary_link","living_street","motorway_link","primary","primary_link","road","secondary","secondary_link","trunk_link","unclassified")){
                  osm$lanes.forward[i] <- 1
                }else{
                  osm$lanes.forward[i] <- 0
                }
              }
            }else{
              #############################################
              #One way roads
              if(is.na(osm$lanes[i])){
                #Insufficent info so guess
                if(osm$highway[i] == "motoway"){
                  osm$lanes.forward[i] <- 3
                }else if(osm$highway[i] == "trunk"){
                  osm$lanes.forward[i] <- 2
                }else if(osm$highway[i] %in% c("residential","living_street","tertiary","tertiary_link","living_street","motorway_link","primary","primary_link","road","secondary","secondary_link","trunk_link","unclassified")){
                  osm$lanes.forward[i] <- 1
                }else{
                  osm$lanes.forward[i] <- 0
                }
              }else{
                #One way so all lanes are forward
                #n.b. ignoring oneway = -1
                osm$lanes.forward[i] <- osm$lanes[i]
              }
            }
          }
        }
      }

      #Lanes Backward

      for(i in 1:nrow(osm)){
        #Exclude Road types where lanes are no relevant
        if(osm$highway[i] %in% c("bridleway", "construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track","service")){
          osm$lanes.backward[i] <- 0
        }else{
          #All other road types
          if(is.na(osm$lanes.backward[i])){
            if(osm$onewaysummary[i] == "Two Way"){
              ###############################################
              #Two Way roads
              #Check for other lane info
              if(!is.na(osm$lanes[i]) & !is.na(osm$lanes.forward[i])){
                #Have total number of lanes and number in opposite direction so subtract
                osm$lanes.backward[i] <- osm$lanes[i] - osm$lanes.forward[i]
              }else if(!is.na(osm$lanes[i]) & is.na(osm$lanes.forward[i])){
                #Have total number of lanes but no other direction
                if(osm$lanes[i] %% 2 == 0){
                  #Even lanes so divide
                  osm$lanes.backward[i] <- osm$lanes[i]/2
                }else{
                  #No data so assign smaller number
                  osm$lanes.backward[i] <- floor(osm$lanes[i]/2)
                }
              }else{
                #Insufficent info so guess
                if(osm$highway[i] == "motoway"){
                  osm$lanes.backward[i] <- 3
                }else if(osm$highway[i] == "trunk"){
                  osm$lanes.backward[i] <- 2
                }else if(osm$highway[i] %in% c("residential","living_street","tertiary","tertiary_link","living_street","motorway_link","primary","primary_link","road","secondary","secondary_link","trunk_link","unclassified")){
                  osm$lanes.backward[i] <- 1
                }else{
                  osm$lanes.backward[i] <- 0
                }
              }
            }else{
              #############################################
              #One way roads
              #Bakcward so no lanes on one way road
              osm$lanes.backward[i] <- 0

              }
            }
          }
        }
      }

    #summary(osm$lanes == osm$lanes.backward + osm$lanes.forward)
    #test <- osm[!is.na(osm$lanes),]
    #test <- test[!(test$lanes == test$lanes.backward + test$lanes.forward),]

      #############################################################
      ############################################################





      #Step 5: Pavements
      summary(osm$sidewalk)
      osm$sidewalk <- as.character(osm$sidewalk)
      for(j in 1:nrow(osm)){
        if(osm$highway[j] %in% c("bridleway", "construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track")){
          #Paths ect that don't have pavements to the side
          osm$sidewalk[j] <- "Not Applicable"
        }else if(is.na(osm$sidewalk[j])){
          if(osm$highway[j] %in% c("motorway","motorway_link","service")){
            #Road types that don't have pavements
            osm$sidewalk[j] <- "no"
          }else if(osm$junction == "roundabout"){
            #Roundabouts only have pavement on the outside
            osm$sidewalk[j] <- "left"
          }else{
            #assume that other roads have pavement on both sides
            osm$sidewalk[j] <- "both"
          }
        }
      }
      osm$sidewalk[osm$sidewalk == "none"] <- "no"
      osm$sidewalk <- as.factor(osm$sidewalk)
      summary(osm$sidewalk)


      #Step 6: Bridges and Tunnels
      osm$elevation <- NA
      for(k in 1:nrow(osm)){
        if(!is.na(osm$bridge[k])){
          if(!osm$bridge[k] == "no"){
            osm$elevation[k] <- "bridge"
          }else{
            osm$elevation[k] <- "ground"
          }
        }else if(!is.na(osm$tunnel[k])){
          if(!osm$tunnel[k] == "no"){
            osm$elevation[k] <- "tunnel"
          }else{
            osm$elevation[k] <- "ground"
          }
        }else{
          osm$elevation[k] <- "ground"
        }
      }
      osm$elevation <- as.factor(osm$elevation)
      summary(osm$elevation)




      #Get the unique combinations of characteristics




      uni <- as.data.frame(osm[,c("roadtype","onewaysummary","junction","sidewalk","lanes.forward","lanes.backward","leftside","rightside","busway.left","busway.right")])
      uni <- uni[,c("roadtype","onewaysummary","junction","sidewalk","lanes.forward","lanes.backward","leftside","rightside","busway.left","busway.right")]
      uni <- uni[!duplicated(uni),]
      uni$count <- NA
      for(m in 1:nrow(uni)){
        uni$count[m] <- nrow(osm[osm$roadtype == uni$roadtype[m] &
                                 osm$onewaysummary == uni$onewaysummary[m] &
                                 osm$junction == uni$junction[m] &
                                 osm$sidewalk == uni$sidewalk[m] &
                                 osm$lanes.forward == uni$lanes.forward[m] &
                                 osm$lanes.backward == uni$lanes.backward[m] &
                                 osm$leftside == uni$leftside[m] &
                                 osm$rightside == uni$rightside[m] &
                                 osm$busway.left == uni$busway.left[m] &
                                 osm$busway.right == uni$busway.right[m],])
      }


      osm$roadtype2 <- paste0(osm$roadtype," ",osm$leftside," ",osm$rightside)

      write.csv(osm,paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-variables-clean.csv"), row.names = FALSE)
    }

  }else{
    print("Input File Missing")
  }
}
