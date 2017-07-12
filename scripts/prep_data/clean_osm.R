#Clean Up OSM Description

skip <- TRUE

#testing
osm <- readRDS("../cyipt-bigdata/osm-raw/BristolCityof/osm-lines.Rds")
####


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
        }else if(osm$highway[g] %in% c("bridleway","corridor", "construction","demolished","escalator","footway","path","pedestrian","steps","track")){
          #The types don't have a direction
          osm$onewaysummary[g] <- "Not Applicable"
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

      #remove oneway as nolonger needed
      osm$oneway <- NULL
      osm$oneway.bicycle <- NULL




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

      #remove maxspeed as no longer needed
      osm$maxspeed <- NULL

      #Step 5: footways
      summary(osm$sidewalk)
      osm$sidewalk <- as.character(osm$sidewalk)
      osm$sidewalk[is.na(osm$sidewalk) | !(osm$sidewalk %in% c("left","right","both","no","none","separate"))] <- "no"
      for(j in 1:nrow(osm)){
        if(osm$highway[j] %in% c("bridleway","corridor", "construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track")){
          #Paths ect that don't have pavements to the side
          osm$sidewalk[j] <- "Not Applicable"
        }else if(is.na(osm$sidewalk[j])){
          if(osm$highway[j] %in% c("motorway","motorway_link","service","trunk","trunk_link")){
            #Road types that don't have pavements
            osm$sidewalk[j] <- "no"
          }else if(osm$junction[j] == "roundabout"){
            #Roundabouts only have pavement on the outside
            osm$sidewalk[j] <- "left"
          }else{
            #assume that other roads have pavement on both sides
            osm$sidewalk[j] <- "both"
          }
        }
      }
      osm$sidewalk[osm$sidewalk == "none"] <- "no"
      osm$sidewalk[osm$sidewalk == "separate"] <- "both"
      osm$sidewalk <- as.factor(osm$sidewalk)



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

      #remove bridge and tunnle as no longer needed
      osm$bridge <- NULL
      osm$tunnel <- NULL

      #Step 9: Clean Up segregated
      osm$segregated[is.na(osm$segregated)] <- "no"

      #Step 10: Road Type

      osm$roadtype <- NA

      for(f in 1:nrow(osm)){
        #No Cycling on motorways
        if(osm$highway[f] == "motorway" |
           osm$highway[f] == "motorway_link"){
          osm$roadtype[f] <- paste0(osm$highway[f]," - Cycling Forbidden")
          #Normal Roads
        }else if(osm$highway[f] %in% c("primary","secondary","tertiary")){
          osm$roadtype[f] <- paste0("Main Road - Cycling Allowed")
        }else if(osm$highway[f] %in% c("primary_link","secondary_link","tertiary_link")){
          osm$roadtype[f] <- paste0("Main Road Link - Cycling Allowed")
        }else if(osm$highway[f] == "trunk"){
          osm$roadtype[f] <- paste0("Trunk Road - Cycling Allowed")
        }else if(osm$highway[f] == "trunk_link"){
          osm$roadtype[f] <- paste0("Trunk Road Link - Cycling Allowed")
        }else if(osm$highway[f] == "residential"){
          osm$roadtype[f] <- paste0("Residential Road - Cycling Allowed")
        }else if(osm$highway[f] %in% c("service","road","unclassified")){
          osm$roadtype[f] <- paste0("Minor Road - Cycling Allowed")
          #Living Streets
        }else if(osm$highway[f] == "living_street"){
          osm$roadtype[f] <- "Living Street"
          #Cycleways
        }else if(osm$highway[f] == "cycleway"){
          #Check if segregated or not
          if(osm$segregated[f] == "yes"){
            osm$roadtype[f] <- "Segregated Cycleway"
          }else{
            osm$roadtype[f] <- "Cycleway"
          }
          #Off Road - Check if cycling is allowed
        }else if(osm$highway[f] %in% c("bridleway","footway","path","pedestrian","track")){
          if(is.na(osm$bicycle[f])){
            #Check if segregated or not
            if(osm$segregated[f] == "yes"){
              osm$roadtype[f] <- "Segregated Shared Path"
            }else{
              osm$roadtype[f] <- "Shared Path"
            }
          }else if(osm$bicycle[f] == "No" | osm$bicycle[f] == "dismount" ){
            osm$roadtype[f] <- "Path - Cycling Forbidden"
          }else{
            #Check if segregated or not
            if(osm$segregated[f] == "yes"){
              osm$roadtype[f] <- "Segregated Shared Path"
            }else{
              osm$roadtype[f] <- "Shared Path"
            }
          }
        }else if(osm$highway[f] %in% c("demolished","corridor","construction","escalator","steps")){
          osm$roadtype[f] <- "Path - Cycling Forbidden"
        }

      }

      #############################################################################
      #Step 7: Lanes
      #New method convert bus lanes to PSV lanes

      #Convert to intergers
      osm$lanes <- as.integer(osm$lanes)
      osm$lanes.backward <- as.integer(osm$lanes.backward)
      osm$lanes.forward <- as.integer(osm$lanes.forward)
      osm$lanes.bus.forward <- as.integer(osm$lanes.bus.forward)
      osm$lanes.left <- as.integer(osm$lanes.left)
      osm$lanes.right <- as.integer(osm$lanes.right)
      osm$lanes.psv <- as.integer(osm$lanes.psv)
      osm$lanes.psv.backward <- as.integer(osm$lanes.psv.backward)
      osm$lanes.psv.forward <- as.integer(osm$lanes.psv.forward)

      #Step 7b: Bus Lanes
      for(l in 1:nrow(osm)){
        #left side
        if(is.na(osm$lanes.psv.forward[l])){
          #If no psv lane info
          if(!is.na(osm$busway.left[l])){
            #Has busway left info
            if(osm$busway.left[l] == "lane"){
              #Has lane
              osm$lanes.psv.forward[l] <- 1
            }else{
              #Some other values
              osm$lanes.psv.forward[l] <- 0
            }
          }else if(!is.na(osm$busway[l])){
            #Has busway general info
            if(osm$busway[l] == "lane"){
              #Has lane
              osm$lanes.psv.forward[l] <- 1
            }else{
              #Some other values
              osm$lanes.psv.forward[l] <- 0
            }
          }else if(!is.na(osm$lanes.psv[l])){
            #Non Specific psv lanes
            if(osm$lanes.psv[l] == 1){
              #Has lane
              osm$lanes.psv.forward[l] <- 1
            }else{
              #Some other values
              osm$lanes.psv.forward[l] <- 0
            }
          }else if(!is.na(osm$psv[l])){
            #unusual tag sometimes used
            if(osm$psv[l] %in% c("designated","yes")){
              #Has lane
              osm$lanes.psv.forward[l] <- 1
            }else{
              #Some other values
              osm$lanes.psv.forward[l] <- 0
            }
          }else{
            #No Busway info
            osm$lanes.psv.forward[l] <- 0
          }
        }

        #right side
        if(is.na(osm$lanes.psv.backward[l])){
          #Has no psv lane info
          if(osm$onewaysummary[l] == "One Way"){
            if(!is.na(osm$psv.backward[l])){
              #unusual tag sometimes used
              if(osm$psv.backward[l] == "yes"){
                #Has lane
                osm$lanes.psv.backward[l] <- 1
              }else{
                #Some other values
                osm$lanes.psv.backward[l] <- 0
              }
            }else if(!is.na(osm$psv[l])){
              #unusual tag sometimes used
              if(osm$psv[l] == "opposite_lane"){
                #Has lane
                osm$lanes.psv.backward[l] <- 1
              }else{
                #Some other values
                osm$lanes.psv.backward[l] <- 0
              }
            }else{
              #One way roads don't have buslanes going backwards
              osm$lanes.psv.backward[l] <- 0
            }
          }else{
            #Two way roads
            if(!is.na(osm$busway.right[l])){
              #Has busway right info
              if(osm$busway.right[l] == "lane"){
                #Has lane
                osm$lanes.psv.backward[l] <- 1
              }else{
                #Some other values
                osm$lanes.psv.backward[l] <- 0
              }
            }else if(!is.na(osm$busway[l])){
              #Has busway general info
              if(osm$busway[l] == "lane"){
                #Has lane
                osm$lanes.psv.backward[l] <- 1
              }else{
                #Some other values
                osm$lanes.psv.backward[l] <- 0
              }
            }else if(!is.na(osm$psv.backward[l])){
              #unusual tag sometimes used
              if(osm$psv.backward[l] == "yes"){
                #Has lane
                osm$lanes.psv.backward[l] <- 1
              }else{
                #Some other values
                osm$lanes.psv.backward[l] <- 0
              }
            }else if(!is.na(osm$psv[l])){
              #unusual tag sometimes used
              if(osm$psv[l] == "opposite_lane"){
                #Has lane
                osm$lanes.psv.backward[l] <- 1
              }else{
                #Some other values
                osm$lanes.psv.backward[l] <- 0
              }

            }else{
              #No Busway info
              osm$lanes.psv.backward[l] <- 0
            }
          }
        }
      }

      #Remove bus lanes from paths etc
      osm$lanes.psv.backward[osm$roadtype %in% c("Path - Cycling Forbidden", "Shared Path")] <- 0
      osm$lanes.psv.forward[osm$roadtype %in% c("Path - Cycling Forbidden", "Shared Path")] <- 0


      #Step 7c: Check for missing bus lanes, where cycle lane is share_busway
      for(o in 1:nrow(osm)){
        if(!is.na(osm$cycleway.left[o])){
          if(osm$lanes.psv.forward[o] == 0 & osm$cycleway.left[o] == "share_busway"){
            osm$lanes.psv.forward[o] <- 1
          }
        }
        if(!is.na(osm$cycleway.right[o])){
          if(osm$lanes.psv.backward[o] == 0 & osm$cycleway.right[o] == "share_busway"){
            osm$lanes.psv.backward[o] <- 1
          }
        }
        if(!is.na(osm$cycleway[o])){
          if(osm$lanes.psv.forward[o] == 0 & osm$cycleway[o] == "share_busway"){
            osm$lanes.psv.forward[o] <- 1
          }else if(osm$lanes.psv.backward[o] == 0 & osm$cycleway[o] == "opposite_share_busway"){
            osm$lanes.psv.backward[o] <- 1
          }
        }
      }

      #Remove unnede columns
      osm$bus_lane <- NULL
      osm$busway <- NULL
      osm$busway.left <- NULL
      osm$busway.right <- NULL
      osm$psv <- NULL
      osm$psv.backward <- NULL
      osm$lanes.psv <- NULL
      osm$lanes.bus.forward <- NULL

      #Step 7d: Clean left and right lanes
      for(q in 1:nrow(osm)){
        if(is.na(osm$lanes.forward[q]) & !is.na(osm$lanes.left[q])){
          osm$lanes.forward[q] <- osm$lanes.left[q]
        }
        if(is.na(osm$lanes.backward[q]) & !is.na(osm$lanes.right[q])){
          osm$lanes.backward[q] <- osm$lanes.right[q]
        }
      }

      osm$lanes.left <- NULL
      osm$lanes.right <- NULL


      #Step 7e: Lanes main part

      ############################################################
      #Super new method
      ###########################################################

      for(i in 1:nrow(osm)){
        lt <- osm$lanes[i]
        lf <- osm$lanes.forward[i]
        lb <- osm$lanes.backward[i]
        lpf <- osm$lanes.psv.forward[i]
        lpb <- osm$lanes.psv.backward[i]

        #Cases
        if(is.na(lb) & is.na(lf)){
          #Both missing
          #Check for total lanes number
          if(is.na(lt)){
            #No total lanes number
            #Guess based on road type
            if(osm$highway[i] %in% c("bridleway","corridor","construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track","service")){
              #These kinds of road don't have lanes
              osm$lanes.forward[i] <- 0
              osm$lanes.backward[i] <- 0
            }else if(osm$highway[i] == "motoway"){
              #Guessing number of lanes based on road type
              if(osm$onewaysummary[i] == "One Way"){
                osm$lanes.forward[i] <- 3
                osm$lanes.backward[i] <- 0
              }else{
                osm$lanes.forward[i] <- 3
                osm$lanes.backward[i] <- 3
              }
            }else if(osm$highway[i] == "trunk"){
              if(osm$onewaysummary[i] == "One Way"){
                osm$lanes.forward[i] <- 2
                osm$lanes.backward[i] <- 0
              }else{
                osm$lanes.forward[i] <- 2
                osm$lanes.backward[i] <- 2
              }
            }else if(osm$highway[i] %in% c("residential","living_street","tertiary","tertiary_link","living_street","motorway_link","primary","primary_link","road","secondary","secondary_link","trunk_link","unclassified")){
              if(osm$onewaysummary[i] == "One Way"){
                osm$lanes.forward[i] <- 1
                osm$lanes.backward[i] <- 0
              }else{
                osm$lanes.forward[i] <- 1
                osm$lanes.backward[i] <- 1
              }
            }else{
              print(paste0("Error: Ran out of road types. For line ",i," values: ",lt," ",lf," ",lb," ",lpf," ",lpb))
            }

          }else{
            #Have total lanes
            lrem <- lt - lpf - lpb #Remaining lanes after removing bus lanes
            if(osm$onewaysummary[i] == "One Way"){
              #One way so all remaining lanes are forward
              osm$lanes.forward[i] <- lrem
              osm$lanes.backward[i] <- 0
            }else{
              #Two way to divide up remaining
              osm$lanes.forward[i] <- ceiling(lrem/2)
              osm$lanes.backward[i] <- floor(lrem/2)
            }
            rm(lrem)
          }
        }else if( xor(is.na(lb), is.na(lf))){
          #One missing
          #Check for total lanes number
          if(!is.na(lt)){
            #Have total number so subtract from other values
            lrem <- lt - lpf - lpb #Remaining lanes after removing bus lanes
            if(is.na(lb)){
              #Backward missing
              osm$lanes.backward[i] <- if(lrem - lf > 0){lrem - lf}else{1} #to avoid negative numbers of lanes
            }else{
              #forward missing
              osm$lanes.forward[i] <- if(lrem - lb > 0){lrem - lb}else{1} #to avoid negative numbers of lanes
            }
            rm(lrem)
          }else{
            #No total number of lanes
            if(is.na(lb)){
              #Backward missing
              #Guess based on road type
              if(osm$highway[i] %in% c("bridleway","corridor", "construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track","service")){
                #These kinds of road don't have lanes
                osm$lanes.backward[i] <- 0
              }else if(osm$highway[i] == "motoway"){
                #Guessing number of lanes based on road type
                if(osm$onewaysummary[i] == "One Way"){
                  osm$lanes.backward[i] <- 0
                }else{
                  osm$lanes.backward[i] <- 3
                }
              }else if(osm$highway[i] == "trunk"){
                if(osm$onewaysummary[i] == "One Way"){
                  osm$lanes.backward[i] <- 0
                }else{
                  osm$lanes.backward[i] <- 2
                }
              }else if(osm$highway[i] %in% c("residential","living_street","tertiary","tertiary_link","living_street","motorway_link","primary","primary_link","road","secondary","secondary_link","trunk_link","unclassified")){
                if(osm$onewaysummary[i] == "One Way"){
                  osm$lanes.backward[i] <- 0
                }else{
                  osm$lanes.backward[i] <- 1
                }
              }else{
                print(paste0("Error: Ran out of road types. For line ",i," values: ",lt," ",lf," ",lb," ",lpf," ",lpb))
              }
            }else{
              #forward missing
              #Guess based on road type
              if(osm$highway[i] %in% c("bridleway","corridor", "construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track","service")){
                #These kinds of road don't have lanes
                osm$lanes.forward[i] <- 0
              }else if(osm$highway[i] == "motoway"){
                #Guessing number of lanes based on road type
                if(osm$onewaysummary[i] == "One Way"){
                  osm$lanes.forward[i] <- 3
                }else{
                  osm$lanes.forward[i] <- 3
                }
              }else if(osm$highway[i] == "trunk"){
                if(osm$onewaysummary[i] == "One Way"){
                  osm$lanes.forward[i] <- 2
                }else{
                  osm$lanes.forward[i] <- 2
                }
              }else if(osm$highway[i] %in% c("residential","living_street","tertiary","tertiary_link","living_street","motorway_link","primary","primary_link","road","secondary","secondary_link","trunk_link","unclassified")){
                if(osm$onewaysummary[i] == "One Way"){
                  osm$lanes.forward[i] <- 1
                }else{
                  osm$lanes.forward[i] <- 1
                }
              }else{
                print(paste0("Error: Ran out of road types. For line ",i," values: ",lt," ",lf," ",lb," ",lpf," ",lpb))
              }
            }
          }
        }else if(!is.na(lb) & !is.na(lf)){
          #None Missing
          #Check for consistancy
          if(!is.na(lt) & !is.na(lpf) & !is.na(lpb)){
            if(lt == lf + lb + lpf + lpb){
              #Lane numbering is consitant
              #Do Nothing
            }else if((lt == lf + lb) | (lpf + lpb > 0)){
              #Double couting of bus lanes
              #reduce the number of lanes accordingly
              osm$lanes.forward[i] <- osm$lanes.forward[i] - lpf
              osm$lanes.backward[i] <- osm$lanes.backward[i] - lpb
            }else{
              ###############################
              #OTHER CHECKS GO HERE
              print(paste0("Error: Ran out of ideas. For line ",i," values: ",lt," ",lf," ",lb," ",lpf," ",lpb))
              ##################################
            }
          }
        }else{
          #Something has gone wrong
          print(paste0("Catastrophic Error: Number of lanes in both known and unknow. For line ",i))
          stop()
        }

        #Clean up
        rm(lt,lf,lb,lpf,lpb)
      }



      ##########################################################################
      # Step 2: Summaris Cycling Infrastrure

      osm$cycleway <- as.character(osm$cycleway)
      osm$cycleway.left <- as.character(osm$cycleway.left)
      #osm$busway.left <- as.character(osm$busway.left)
      osm$cycleway.right <- as.character(osm$cycleway.right)
      #osm$busway.right <- as.character(osm$busway.right)
      osm$leftside <- NA

      for(d in 1:nrow(osm)){
        if(osm$highway[d] %in% c("bridleway", "corridor","construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track")){
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
        if(osm$highway[e] %in% c("bridleway", "corridor","construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track")){
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


      #Get the unique combinations of characteristics

      uni <- as.data.frame(osm[,c("roadtype","onewaysummary","junction","sidewalk","leftside","lanes.psv.forward","lanes.forward","lanes.backward","lanes.psv.backward","rightside")])
      uni <- uni[,c("roadtype","onewaysummary","junction","sidewalk","leftside","lanes.psv.forward","lanes.forward","lanes.backward","lanes.psv.backward","rightside")]
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
                                 osm$lanes.psv.forward == uni$lanes.psv.forward[m] &
                                 osm$lanes.psv.backward == uni$lanes.psv.backward[m],])
      }

      write.csv(uni,"../cyipt/input-data/roadtypes4.csv")


      #COmpare expected width to actual width

      osm$width.calc <- NA
      osm$widthpath.calc <- NA

      for(p in 1:nrow(osm)){
        if(osm$highway[p] %in% c("motorway","motorway_link","primary","primary_link","secondary","secondary_link","tertiary","tertiary_link","trunk","trunk_link" )){
          #Busy road with wide lanes
          osm$width.calc[p] <- 3.65 * (osm$lanes.forward[p] + osm$lanes.backward[p])
          if(osm$sidewalk[p] == "both"){
            pathwidth <- 4
          }else if(osm$sidewalk[p] %in% c("left","right")){
            pathwidth <- 2
          }else{
            pathwidth <- 0
          }
          osm$widthpath.calc[p] <- osm$width.calc[p] + pathwidth
        }else if(osm$highway[p] %in% c("living_street", "pedestrian", "residential", "service", "unclassified", "road")){
          #Quiet roads with narrow lanes
          if(osm$sidewalk[p] == "both"){
            pathwidth <- 4
          }else if(osm$sidewalk[p] %in% c("left","right")){
            pathwidth <- 2
          }else{
            pathwidth <- 0
          }
          osm$width.calc[p] <- 2.75 * (osm$lanes.forward[p] + osm$lanes.backward[p])
          osm$widthpath.calc[p] <- osm$width.calc[p] + pathwidth
        }else{
          #Non Road
          osm$width.calc[p] <- 0
          osm$widthpath.calc[p] <- 2
          pathwidth <- 0
        }
        rm(pathwidth)
      }


      osm$roadtype2 <- paste0(osm$roadtype," ",osm$leftside," ",osm$rightside)

      write.csv(osm,paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-variables-clean.csv"), row.names = FALSE)
    }

  }else{
    print("Input File Missing")
  }
}
