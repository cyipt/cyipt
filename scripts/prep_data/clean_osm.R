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

      ##########################################################################
      # Step 2: Summaris Cycling Infrastrure

      osm$cycleway <- as.character(osm$cycleway)
      osm$cycleway.left <- as.character(osm$cycleway.left)
      osm$busway.left <- as.character(osm$busway.left)
      osm$cycleway.right <- as.character(osm$cycleway.right)
      osm$busway.right <- as.character(osm$busway.right)
      osm$leftside <- NA

      for(d in 1:nrow(osm)){
        if(!is.na(osm$cycleway.left[d])){
          osm$leftside[d] <- osm$cycleway.left[d]
        }else if(!is.na(osm$cycleway[d])){
          if(osm$cycleway[d] == "No"){
            #Do Nothing
          }else if(osm$cycleway[d] == "opposite" | osm$cycleway[d] == "opposite_lane" | osm$cycleway[d] == "opposite_track"){
            #Do nothing as this is on the right side
          }else{
            osm$leftside[d] <- osm$cycleway[d]
          }
        }else if(!is.na(osm$busway.left[d])){
          osm$leftside[d] <- paste0("bus - ",osm$cycleway.left[d])
        }
      }

      osm$rightside <- NA

      for(e in 1:nrow(osm)){
        if(!is.na(osm$cycleway.right[e])){
          osm$rightside[e] <- osm$cycleway.right[e]
        }else if(!is.na(osm$cycleway[e])){
          if(osm$cycleway[e] == "No"){
            #Do Nothing
          }else if(osm$cycleway[e] == "opposite" | osm$cycleway[e] == "opposite_lane" | osm$cycleway[e] == "opposite_track"){
            #Do nothing as this is on the right side
            osm$rightside[e] <- osm$cycleway[e]
          }else{
            osm$rightside[e] <- osm$cycleway[e]
          }
        }else if(!is.na(osm$busway.right[e])){
          osm$rightside[e] <- paste0("bus - ",osm$cycleway.right[e])
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
        }else if(osm$highway[f] == "bridleway" |
                 osm$highway[f] == "construction" |
                 osm$highway[f] == "demolished" |
                 osm$highway[f] == "escalator" |
                 osm$highway[f] == "footway" |
                 osm$highway[f] == "path" |
                 osm$highway[f] == "pedestrian" |
                 osm$highway[f] == "steps" |
                 osm$highway[f] == "track")
          if(is.na(osm$bicycle[f])){
            osm$roadtype[f] <- "Shared Path"
          }else if(osm$bicycle[f] == "No" | osm$bicycle[f] == "dismount" ){
            osm$roadtype[f] <- "Path - Cycling Forbidden"
          }else{
            osm$roadtype[f] <- "Shared Path"
          }
      }


      #Step 3: One way
      osm$onewaysummary <- NA
      osm$oneway.bicycle <- as.character(osm$oneway.bicycle)
      for(g in 1:nrow(osm)){
        if(is.na(osm$oneway[g])){
          osm$onewaysummary[g] <- "Two Way Road"
        }else{
          if(osm$oneway[g] == "Yes" | osm$oneway[g] == "yes" |osm$oneway[g] == "-1"){
            if(is.na(osm$oneway.bicycle[g])){
              osm$onewaysummary[g] <- "One Way Road"
            }else{
              if(osm$oneway.bicycle[g] == "no" | osm$oneway.bicycle[g] == "No"){
                osm$onewaysummary[g] <- "One Way Road - two way cycling"
              }else{
                osm$onewaysummary[g] <- "One Way Road"
              }
            }
          }else if(osm$highway[g] == "motorway"  | osm$highway[g] == "motorway_link"){
            osm$onewaysummary[g] <- "One Way Road"
          }else{
            osm$onewaysummary[g] <- "Two Way Road"
          }
        }

      }


      #Step 4: Lanes in each direction
      for(i in 1:nrow(osm)){
        if(is.na(osm$lanes.forward[i])){
          if(!is.na(osm$oneway[i])){
            if(osm$oneway[i] == "-1"){
              #Oneway so no forward lanes
              osm$lanes.forward[i] <- 0
            }else if(osm$oneway[i] == "yes" | osm$oneway[i] == "Yes"){
              #One way in this direction
              if(is.na(osm$lanes[i])){
                #No general lanes info
                if(osm$highway[i] == "motoway"){
                  osm$lanes.forward[i] <- 3
                }else if(osm$highway[i] == "trunk"){
                  osm$lanes.forward[i] <- 2
                }else if(osm$highway[i] == "residential" |
                         osm$highway[i] == "living_street" |
                         osm$highway[i] == "tertiary" |
                         osm$highway[i] == "tertiary_link" |
                         osm$highway[i] == "living_street" |
                         osm$highway[i] == "motorway_link" |
                         osm$highway[i] == "primary" |
                         osm$highway[i] == "primary_link" |
                         osm$highway[i] == "road" |
                         osm$highway[i] == "secondary" |
                         osm$highway[i] == "secondary_link" |
                         osm$highway[i] == "service" |
                         osm$highway[i] == "trunk_link" |
                         osm$highway[i] == "unclassified"){
                  osm$lanes.forward[i] <- 1
                }else{
                  osm$lanes.forward[i] <- 0
                }
              }else{
                #one way so lanes forward equals lanes
                osm$lanes.forward[i] <- osm$lanes[i]

              }

            }else{
              #Not one way so may have backwards lanes
              if(is.na(osm$lanes[i])){
                #No general lanes info
                if(osm$highway[i] == "motoway"){
                  osm$lanes.forward[i] <- 3
                }else if(osm$highway[i] == "trunk"){
                  osm$lanes.forward[i] <- 2
                }else if(osm$highway[i] == "residential" |
                         osm$highway[i] == "living_street" |
                         osm$highway[i] == "tertiary" |
                         osm$highway[i] == "tertiary_link" |
                         osm$highway[i] == "living_street" |
                         osm$highway[i] == "motorway_link" |
                         osm$highway[i] == "primary" |
                         osm$highway[i] == "primary_link" |
                         osm$highway[i] == "road" |
                         osm$highway[i] == "secondary" |
                         osm$highway[i] == "secondary_link" |
                         osm$highway[i] == "service" |
                         osm$highway[i] == "trunk_link" |
                         osm$highway[i] == "unclassified"){
                  osm$lanes.forward[i] <- 1
                }else{
                  osm$lanes.forward[i] <- 0
                }
              }else{
                #Some General Lanes information
                if(osm$lanes[i] == 1){
                  #One lane so assign to forward
                  osm$lanes.forward[i] <- 1
                }else if(osm$lanes[i] %% 2 == 0) {
                  #Even lanes so divide
                  osm$lanes.forward[i] <- osm$lanes[i]/2
                }else{
                  #Odd number of lanes so check
                  if(is.na(osm$lanes.backward[i])){
                    #No data so assign smaller number
                    osm$lanes.forward[i] <- ceiling(osm$lanes[i]/2)
                  }else{
                    #Subtract one from other
                    osm$lanes.forward[i] <- osm$lanes[i] - osm$lanes.backward[i]
                  }
                }

              }

            }
          }else{
            #Not one way so may have backwards lanes
            if(is.na(osm$lanes[i])){
              #No general lanes info
              if(osm$highway[i] == "motoway"){
                osm$lanes.forward[i] <- 3
              }else if(osm$highway[i] == "trunk"){
                osm$lanes.forward[i] <- 2
              }else if(osm$highway[i] == "residential" |
                       osm$highway[i] == "living_street" |
                       osm$highway[i] == "tertiary" |
                       osm$highway[i] == "tertiary_link" |
                       osm$highway[i] == "living_street" |
                       osm$highway[i] == "motorway_link" |
                       osm$highway[i] == "primary" |
                       osm$highway[i] == "primary_link" |
                       osm$highway[i] == "road" |
                       osm$highway[i] == "secondary" |
                       osm$highway[i] == "secondary_link" |
                       osm$highway[i] == "service" |
                       osm$highway[i] == "trunk_link" |
                       osm$highway[i] == "unclassified"){
                osm$lanes.forward[i] <- 1
              }else{
                osm$lanes.forward[i] <- 0
              }
            }else{
              #Some General Lanes information
              if(osm$lanes[i] == 1){
                #One lane so assign to forward
                osm$lanes.forward[i] <- 1
              }else if(osm$lanes[i] %% 2 == 0) {
                #Even lanes so divide
                osm$lanes.forward[i] <- osm$lanes[i]/2
              }else{
                #Odd number of lanes so check
                if(is.na(osm$lanes.backward[i])){
                  #No data so assign smaller number
                  osm$lanes.forward[i] <- ceiling(osm$lanes[i]/2)
                }else{
                  #Subtract one from other
                  osm$lanes.forward[i] <- osm$lanes[i] - osm$lanes.backward[i]
                }
              }

            }
          }
        }
      }




      for(i in 1:nrow(osm)){
        if(is.na(osm$lanes.backward[i])){
          if(!is.na(osm$oneway[i])){
            if(osm$oneway[i] == "yes" | osm$oneway[i] == "Yes"){
              #Oneway so no backwards lanes
              osm$lanes.backward[i] <- 0
            }else{
              #Not one way so may have backwards lanes
              if(is.na(osm$lanes[i])){
                #No general lanes info
                if(osm$highway[i] == "motoway"){
                  osm$lanes.backward[i] <- 3
                }else if(osm$highway[i] == "trunk"){
                  osm$lanes.backward[i] <- 2
                }else if(osm$highway[i] == "residential" |
                         osm$highway[i] == "living_street" |
                         osm$highway[i] == "tertiary" |
                         osm$highway[i] == "tertiary_link" |
                         osm$highway[i] == "living_street" |
                         osm$highway[i] == "motorway_link" |
                         osm$highway[i] == "primary" |
                         osm$highway[i] == "primary_link" |
                         osm$highway[i] == "road" |
                         osm$highway[i] == "secondary" |
                         osm$highway[i] == "secondary_link" |
                         osm$highway[i] == "service" |
                         osm$highway[i] == "trunk_link" |
                         osm$highway[i] == "unclassified"){
                  osm$lanes.backward[i] <- 1
                }else{
                  osm$lanes.backward[i] <- 0
                }
              }else{
                #Some General Lanes information
                if(osm$lanes[i] == 1){
                  #One lane so assign to forward
                  osm$lanes.backward[i] <- 0
                }else if(osm$lanes[i] %% 2 == 0) {
                  #Even lanes so divide
                  osm$lanes.backward[i] <- osm$lanes[i]/2
                }else{
                  #Odd number of lanes so check
                  if(is.na(osm$lanes.forward[i])){
                    #No data so assign smaller number
                    osm$lanes.backward[i] <- floor(osm$lanes[i]/2)
                  }else{
                    #Subtract one from other
                    osm$lanes.backward[i] <- osm$lanes[i] - osm$lanes.forward[i]
                  }
                }

              }

            }
          }else{
            #Not one way so may have backwards lanes
            if(is.na(osm$lanes[i])){
              #No general lanes info
              if(osm$highway[i] == "motoway"){
                osm$lanes.backward[i] <- 3
              }else if(osm$highway[i] == "trunk"){
                osm$lanes.backward[i] <- 2
              }else if(osm$highway[i] == "residential" |
                       osm$highway[i] == "living_street" |
                       osm$highway[i] == "tertiary" |
                       osm$highway[i] == "tertiary_link" |
                       osm$highway[i] == "living_street" |
                       osm$highway[i] == "motorway_link" |
                       osm$highway[i] == "primary" |
                       osm$highway[i] == "primary_link" |
                       osm$highway[i] == "road" |
                       osm$highway[i] == "secondary" |
                       osm$highway[i] == "secondary_link" |
                       osm$highway[i] == "service" |
                       osm$highway[i] == "trunk_link" |
                       osm$highway[i] == "unclassified"){
                osm$lanes.backward[i] <- 1
              }else{
                osm$lanes.backward[i] <- 0
              }
            }else{
              #Some General Lanes information
              if(osm$lanes[i] == 1){
                #One lane so assign to forward
                osm$lanes.backward[i] <- 0
              }else if(osm$lanes[i] %% 2 == 0) {
                #Even lanes so divide
                osm$lanes.backward[i] <- osm$lanes[i]/2
              }else{
                #Odd number of lanes so check
                if(is.na(osm$lanes.forward[i])){
                  #No data so assign smaller number
                  osm$lanes.backward[i] <- floor(osm$lanes[i]/2)
                }else{
                  #Subtract one from other
                  osm$lanes.backward[i] <- osm$lanes[i] - osm$lanes.forward[i]
                }
              }

            }
          }
        }
      }




      osm$roadtype2 <- paste0(osm$roadtype," ",osm$leftside," ",osm$rightside)

      write.csv(osm,paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-variables-clean.csv"), row.names = FALSE)
    }

  }else{
    print("Input File Missing")
  }
}
