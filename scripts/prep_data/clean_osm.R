#Clean Up OSM Description
library(sf)
library(dplyr)

#settings now come from master file
#skip <- FALSE # Skip files alread done

#testing
#osm <- readRDS("../cyipt-bigdata/osm-raw/BristolCityof/osm-lines.Rds")
####


#List folders

#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo

#create directory
if(!dir.exists(paste0("../cyipt-bigdata/osm-clean"))){
  dir.create(paste0("../cyipt-bigdata/osm-clean"))
}

for(a in 1:length(regions)){

  if(file.exists(paste0("../cyipt-bigdata/osm-raw/",regions[a],"/osm-lines.Rds"))){ #Check for input file
    if(file.exists(paste0("../cyipt-bigdata/osm-clean/",regions[a],"/osm-lines.Rds")) & skip){ #check for existing copy of output
      message(paste0("Skipping cleaning OSM tags for ",regions[a]," as already done"))

    }else{
      #create directory
      if(!dir.exists(paste0("../cyipt-bigdata/osm-clean/",regions[a]))){
        dir.create(paste0("../cyipt-bigdata/osm-clean/",regions[a]))
      }

      message(paste0("Cleaning OSM tags for ",regions[a]," at ",Sys.time()))
      osm <- readRDS(paste0("../cyipt-bigdata/osm-raw/",regions[a],"/osm-lines.Rds"))
      #osm <- st_transform(osm, 27700) now done in download-osm

      #remove factors
      osm$highway <- as.character(osm$highway)
      osm$cycleway.left <- as.character(osm$cycleway.left)
      osm$cycleway.right <- as.character(osm$cycleway.right)
      osm$cycleway.otherside <- as.character(osm$cycleway.otherside)
      osm$cycleway <- as.character(osm$cycleway)
      osm$segregated <- as.character(osm$segregated)

      #Convert to intergers
      osm$lanes <- as.integer(as.character(osm$lanes))
      osm$lanes.backward <- as.integer(as.character(osm$lanes.backward))
      osm$lanes.forward <- as.integer(as.character(osm$lanes.forward))
      osm$lanes.bus.forward <- as.integer(as.character(osm$lanes.bus.forward))
      osm$lanes.left <- as.integer(as.character(osm$lanes.left))
      osm$lanes.right <- as.integer(as.character(osm$lanes.right))
      osm$lanes.psv <- as.integer(as.character(osm$lanes.psv))
      osm$lanes.psv.backward <- as.integer(as.character(osm$lanes.psv.backward))
      osm$lanes.psv.forward <- as.integer(as.character(osm$lanes.psv.forward))


      ###############################################################################
      #step 10: remove not allowed roads
      ################################################################################
      osm <- osm[!is.na(osm$highway),] #Must have a highway value for so many reasons
      not.allowed <- c("proposed","planned","escape","demolished","abandoned","dismantled",
                       "crossing","raceway","traffic_island","ohm:military:Trench","junction",
                       "escalator","corridor","construction","services","bus_stop","elevator",
                       "stepping_stones","disused","rest_area","gallop","traffic_signals")
      osm <- osm[!(osm$highway %in% not.allowed),]
      rm(not.allowed)


      ###############################################################################
      #step 11: clean depreciated highway tags
      ################################################################################
      osm$highway[osm$highway %in% c("trail", "byway", "unsurfaced" ) ] <- "track"
      osm$highway[osm$highway %in% c("layby","access","manoeuvring_forecourt")] <- "service"
      osm$highway[osm$highway %in% c("no","none") ] <- "path"
      osm$highway[osm$highway %in% c("mini_roundabout","residentiaal")] <- "residential"
      osm$highway[osm$highway %in% c("yes") ] <- "road"

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
        }else if(osm$highway[g] %in% c("bridleway","corridor", "construction","demolished","escalator","footway","path","pedestrian","steps","track","bus_guideway")){
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
      rm(g)

      #remove oneway as nolonger needed
      osm$oneway <- NULL
      osm$oneway.bicycle <- NULL

      #############################################################################################
      #Step 1: Guess Road Speed if one not provided
      osm$maxspeed <- as.character(osm$maxspeed)

      for(b in 1:nrow(osm)){
        if(is.na(osm$maxspeed[b])){
          type <- osm$highway[b]
          if(type %in% c("motorway","motorway_link")){
            osm$maxspeed[b] <- 70
          }else if(type %in% c("trunk","trunk_link","bus_guideway")) {
            osm$maxspeed[b] <- 60
          }else if(type %in% c("primary","residential","road","primary_link","secondary","secondary_link","tertiary","tertiary_link")){
            osm$maxspeed[b] <- 30
          }else if(type == "service" ){
            osm$maxspeed[b] <- 20
          }else if(type %in% c("path","bridleway","construction","cycleway","demolished","escalator","footway","living_street","steps","track","unclassified")){
            osm$maxspeed[b] <- 10
          }else{
            osm$maxspeed[b] <- 60
          }
        }
      }
      rm(b,type)

      for(c in 1:nrow(osm)){
        if(osm$maxspeed[c] == "70 mph" | osm$maxspeed[c] == "70" ){
          osm$maxspeed[c] <- 70
        }else if(osm$maxspeed[c] == "60 mph"| osm$maxspeed[c] == "60" ){
          osm$maxspeed[c] <- 60
        }else if(osm$maxspeed[c] == "50 mph"| osm$maxspeed[c] == "50" ){
          osm$maxspeed[c] <- 50
        }else if(osm$maxspeed[c] == "40 mph"| osm$maxspeed[c] == "40" ){
          osm$maxspeed[c] <- 40
        }else if(osm$maxspeed[c] == "30 mph"| osm$maxspeed[c] == "30" ){
          osm$maxspeed[c] <- 30
        }else if(osm$maxspeed[c] == "20 mph"| osm$maxspeed[c] == "20" ){
          osm$maxspeed[c] <- 20
        }else if(osm$maxspeed[c] == "10 mph"| osm$maxspeed[c] == "10" ){
          osm$maxspeed[c] <- 10
        }else if(osm$maxspeed[c] == "15 mph"| osm$maxspeed[c] == "15" ){
          osm$maxspeed[c] <- 15
        }else if(osm$maxspeed[c] == "5 mph"| osm$maxspeed[c] == "5" ){
          osm$maxspeed[c] <- 5
        }else{
          osm$maxspeed[c] <- 30
        }
      }
      rm(c)

      #convert to number
      osm$maxspeed <- as.integer(osm$maxspeed)

      ##################################################################
      #Step 5: footways
      summary(osm$sidewalk)
      osm$sidewalk <- as.character(osm$sidewalk)
      osm$sidewalk[is.na(osm$sidewalk) | !(osm$sidewalk %in% c("left","right","both","no","none","separate"))] <- "no"
      for(j in 1:nrow(osm)){
        if(osm$highway[j] %in% c("bridleway","corridor", "construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track")){
          #Paths ect that don't have pavements to the side
          osm$sidewalk[j] <- "Not Applicable"
        }else if(is.na(osm$sidewalk[j])){
          if(osm$highway[j] %in% c("motorway","motorway_link","service","trunk","trunk_link","bus_guideway")){
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
      rm(j)

      osm$sidewalk[osm$sidewalk == "none"] <- "no"
      osm$sidewalk[osm$sidewalk == "separate"] <- "both"
      #osm$sidewalk <- as.factor(osm$sidewalk)

      ###########################################################################################
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
      rm(k)
      #osm$elevation <- as.factor(osm$elevation)

      #remove bridge and tunnle as no longer needed
      osm$bridge <- NULL
      osm$tunnel <- NULL

      ################################################################################
      #Step 9: Clean Up segregated
      osm$segregated[is.na(osm$segregated)] <- "no"
      osm$segregated[osm$segregated != "yes"] <- "no"

      ###############################################################################
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
        }else if(osm$highway[f] %in% c("bus_guideway")){
          osm$roadtype[f] <- "Special Road - Cycling Forbidden"
        }else{
          warning(paste0("unknown road type at ",f," ",osm$highway[f]))
          stop()
        }

      }
      rm(f)

      #############################################################################
      #Step 7: Lanes
      #New method convert bus lanes to PSV lanes



      #Sometimes used odd tag lanes:bus:forward
      for(r in 1:nrow(osm)){
        if(is.na(osm$lanes.psv.forward[r]) & !is.na(osm$lanes.bus.forward[r])){
          osm$lanes.psv.forward[r] <- osm$lanes.bus.forward[r]
        }
      }
      rm(r)

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
      rm(l)

      #Remove bus lanes from paths etc
      osm$lanes.psv.backward[osm$roadtype %in% c("Path - Cycling Forbidden", "Shared Path")] <- 0
      osm$lanes.psv.forward[osm$roadtype %in% c("Path - Cycling Forbidden", "Shared Path")] <- 0

      osm$lanes.psv.backward[osm$roadtype == "Cycleway"] <- 0
      osm$lanes.psv.forward[osm$roadtype == "Cycleway"] <- 0


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
      rm(o)


      #Remove unneeded columns
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
      rm(q)

      osm$lanes.left <- NULL
      osm$lanes.right <- NULL

      #step 7e: One way road don't have back lanes
      osm$lanes.backward[osm$onewaysummary %in% c("One Way", "One Way - Two Way Cycling")] <- 0

      #Step 7f: Lanes main part

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
            }else if(osm$highway[i] == "motorway"){
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
            }else if(osm$highway[i] %in% c("residential","living_street","tertiary","tertiary_link","living_street","motorway_link","primary","primary_link","road","secondary","secondary_link","trunk_link","unclassified","bus_guideway")){
              if(osm$onewaysummary[i] == "One Way"){
                osm$lanes.forward[i] <- 1
                osm$lanes.backward[i] <- 0
              }else{
                osm$lanes.forward[i] <- 1
                osm$lanes.backward[i] <- 1
              }
            }else{
              print(paste0("Error: 5: Ran out of road types. For line ",i," values: ",lt," ",lf," ",lb," ",lpf," ",lpb))
              stop()
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
              }else if(osm$highway[i] == "motorway"){
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
              }else if(osm$highway[i] %in% c("residential","living_street","tertiary","tertiary_link","living_street","motorway_link","primary","primary_link","road","secondary","secondary_link","trunk_link","unclassified","bus_guideway")){
                if(osm$onewaysummary[i] == "One Way"){
                  osm$lanes.backward[i] <- 0
                }else{
                  osm$lanes.backward[i] <- 1
                }
              }else{
                print(paste0("Error: 3: Ran out of road types. For line ",i," values: ",lt," ",lf," ",lb," ",lpf," ",lpb))
              }
            }else{
              #forward missing
              #Guess based on road type
              if(osm$highway[i] %in% c("bridleway","corridor", "construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track","service")){
                #These kinds of road don't have lanes
                osm$lanes.forward[i] <- 0
              }else if(osm$highway[i] == "motorway"){
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
              }else if(osm$highway[i] %in% c("residential","living_street","tertiary","tertiary_link","living_street","motorway_link","primary","primary_link","road","secondary","secondary_link","trunk_link","unclassified","bus_guideway")){
                if(osm$onewaysummary[i] == "One Way"){
                  osm$lanes.forward[i] <- 1
                }else{
                  osm$lanes.forward[i] <- 1
                }
              }else{
                print(paste0("Error: 2: Ran out of road types. For line ",i," values: ",lt," ",lf," ",lb," ",lpf," ",lpb))
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
            }else if((lt == lf + lb) & (lpf + lpb > 0)){
              #Double couting of bus lanes
              #reduce the number of lanes accordingly
              osm$lanes.forward[i] <- osm$lanes.forward[i] - lpf
              osm$lanes.backward[i] <- osm$lanes.backward[i] - lpb
            }else if(lt != (lf + lb + lpb + lpf)){
              #Total lanes is wrong, assume indervidual values are correct
              osm$lanes[i] <- lf + lb + lpb + lpf
            }else{
              ###############################
              #OTHER CHECKS GO HERE
              print(paste0("Error: 1: Ran out of ideas. For line ",i," values: ",lt," ",lf," ",lb," ",lpf," ",lpb))

              stop()
              ##################################
            }
          }
        }else{
          #Something has gone wrong
          print(paste0("Catastrophic Error: 4: Number of lanes in both known and unknow. For line ",i))
          stop()
        }

        #Clean up
        rm(lt,lf,lb,lpf,lpb)
      }
      rm(i)

      ##########################################################################
      # Step 2:  Cycling Infrastrure

      #Import in the sometimes uses cycleway.otherside
      for(e in 1:nrow(osm)){
        if(!is.na(osm$cycleway.otherside[e]) & is.na(osm$cycleway.right[e])){
          osm$cycleway.right[e] <- osm$cycleway.otherside[e]
          osm$cycleway.otherside[e] <- NA
        }else if(!is.na(osm$cycleway.otherside[e]) & !is.na(osm$cycleway.right[e])){
          if(osm$cycleway.otherside[e] == osm$cycleway.right[e]){
            osm$cycleway.otherside[e] <- NA
          }
        }
      }
      rm(e)
      if(!all(is.na(osm$cycleway.otherside))){
        message("cycleway.otherside error")
        error.check <- osm[!is.na(osm$cycleway.otherside),]
        stop()
      }


      # Sometimes we people double tag cycleway rather than using the left and right tags
      for(m in 1:nrow(osm)){
        if(is.na(osm$cycleway.left[m]) & is.na(osm$cycleway.right[m]) & !is.na(osm$cycleway[m]) ){
          #Check for ; character
          if(grepl(";",osm$cycleway[m])){
            spt <- unlist(strsplit(osm$cycleway[m], ";", fixed = TRUE))
            #print(spt)
            osm$cycleway.left[m] <- spt[1]
            osm$cycleway.right[m] <- spt[2]
            rm(spt)
          }

        }
      }
      rm(m)

      means.lane <- c("lane_left","1.25","Lane","yes","bothways","both")
      means.opposite_lane <- c("opposite_late", "opposite_lane","lane_right","opposite","right:lane")
      means.track <- c("trck","segregated","tracks","separate")
      means.opposite_track <- c("opposite_track")
      means.busway <- c("shared_busway")
      means.opposite_busway <- c("opposite_share_busway")
      not.allowed <- c("service","squeezed","highway","road","shared","shared_lane","designated", "proposed",
                       "only_sunday", "crawler_lane","shared use","sidewalk","none", "share_sidewalk",
                       "pavement_left","sidepath","use_sidepath","restaurant route","crossing","permissive")

      #clean up incorect tagging
      osm$cycleway.left[osm$cycleway.left %in% means.lane] <- "lane"
      osm$cycleway.left[osm$cycleway.left %in% means.track] <- "track"
      osm$cycleway.left[osm$cycleway.left %in% means.opposite_lane] <- "opposite_lane"
      osm$cycleway.left[osm$cycleway.left %in% means.opposite_track] <- "opposite_track"
      osm$cycleway.left[osm$cycleway.left %in% means.busway] <- "share_busway"
      osm$cycleway.left[osm$cycleway.left %in% means.opposite_busway] <- "opposite_share_busway"
      osm$cycleway.left[osm$cycleway.left %in% not.allowed ] <- "no"

      osm$cycleway.right[osm$cycleway.right %in% c(means.lane,means.opposite_lane)] <- "lane"
      osm$cycleway.right[osm$cycleway.right %in% c(means.track,means.opposite_track)] <- "track"
      osm$cycleway.right[osm$cycleway.right %in% means.busway] <- "share_busway"
      osm$cycleway.right[osm$cycleway.right %in% means.opposite_busway] <- "opposite_share_busway"
      osm$cycleway.right[osm$cycleway.right %in% not.allowed ] <- "no"

      osm$cycleway[osm$cycleway %in% means.lane] <- "lane"
      osm$cycleway[osm$cycleway %in% means.track] <- "track"
      osm$cycleway[osm$cycleway %in% means.opposite_lane] <- "opposite_lane"
      osm$cycleway[osm$cycleway %in% means.opposite_track] <- "opposite_track"
      osm$cycleway[osm$cycleway %in% means.busway] <- "share_busway"
      osm$cycleway[osm$cycleway %in% means.opposite_busway] <- "opposite_share_busway"
      osm$cycleway[osm$cycleway %in% not.allowed ] <- "no"


      rm(not.allowed, means.opposite_busway, means.busway, means.opposite_track, means.track, means.opposite_lane, means.lane)

      if(!all(c(osm$cycleway,osm$cycleway.left,osm$cycleway.right) %in% c(NA,"no","track","lane","opposite_lane","share_busway","opposite_track","opposite_share_busway"))){
        message("Invalid Cycle Infrastrucutre Tags Before Cleaning")
        print(unique(c(osm$cycleway,osm$cycleway.left,osm$cycleway.right)))
        stop()
      }



      ########################
      # New Cycle Infra Method with one way roads
      #####

      for(d in 1:nrow(osm)){
        #Etablish the Main cases
        highway <- osm$highway[d]
        left <- osm$cycleway.left[d]
        right <- osm$cycleway.right[d]
        main <- osm$cycleway[d]
        oneway <- osm$onewaysummary[d]

        #Some Road types left and right lanes are not a valid concept
        if(highway %in% c("bridleway", "corridor","construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track","bus_guideway")){
          osm$cycleway.left[d] <- "no"
          osm$cycleway.right[d] <- "no"
        }else if( (is.na(main)  | main  == "no") &
            (is.na(left)  | left  == "no") &
            (is.na(right) | right == "no") ){
          # No info assume that there is no cycle infrastrucutre
          osm$cycleway.left[d] <- "no"
          osm$cycleway.right[d] <- "no"
          osm$cycleway[d] <- "no"
        }else if(!is.na(main) & is.na(left) & is.na(right)){
          #Simple Case have main and no left or right data
          #Check for specific opposite side cases
          if(main %in% c("opposite_lane","opposite_track","opposite_share_busway")){
            osm$cycleway.left[d] <- "no"
            if(main == "opposite_lane"){
              osm$cycleway.right[d] <- "lane"
            }else if(main == "opposite_track"){
              osm$cycleway.right[d] <- "track"
            }else if(main == "opposite_share_busway"){
              osm$cycleway.right[d] <- "share_busway"
            }else{
              message("Unsusal Error 3")
              stop()
            }
          }else if(oneway == "Two Way"){
            #Two way road so put cycle infra on both sides
            osm$cycleway.left[d] <- main
            osm$cycleway.right[d] <- main
          }else if(oneway == "One Way - Two Way Cycling"){
            #Check for contra flow cycle infra
            if(main %in% c("opposite","opposite_lane") ){
              osm$cycleway.left[d] <- "no"
              osm$cycleway.right[d] <- "lane"
            }else if(main %in% c("lane","track")){
              osm$cycleway.left[d] <- main
              osm$cycleway.right[d] <- main
            }else if(main %in% c("opposite_track")){
              osm$cycleway.left[d] <- "no"
              osm$cycleway.right[d] <- "track"
            }else{
              message(paste0("Unusual Case: 2  main = ",main," left = ",left," right = ",right," highway = ",highway," oneway = ",oneway," line = ",d))
              qtm(osm[d,], lines.lwd = 4)
              Sys.sleep(3)
              stop()
            }
          }else{
            #One way raod so put cycle infra on the left
            osm$cycleway.left[d] <- main
            osm$cycleway.right[d] <- "no"
          }
        }else if(!is.na(left) & !is.na(right)){
          #We have Full left and right info so do nothing
        }else if( (is.na(main) | main == "no")  & !is.na(left) & is.na(right)){
          #We only have info for the left side assume it is complete
          osm$cycleway.right[d] <- "no"
        }else if( (is.na(main) | main == "no") & is.na(left) & !is.na(right)){
          #We only have info for the right side assume it is complete
          osm$cycleway.left[d] <- "no"
        }else if(main %in% c("opposite_lane","opposite_track","opposite") & (is.na(left) | left == "no" ) & right %in% c("lane","track")){
          #Contra flow with main also tagged as opposite
          osm$cycleway.left[d] <- "no"
        }else if(!is.na(main) & !is.na(left)){
          if(main == left & (!is.na(left) | left == "no") & oneway == "One Way" )  {
            #Oneway road where main and the left have both been tagged
            osm$cycleway.right[d] <- "no"
          }else if(main != left){
            osm$cycleway.right[d] <- main
          }else if(main == left){
            osm$cycleway.right[d] <- "no"
          }else{
            message(paste0("Unusual Case: 5a  main = ",main," left = ",left," right = ",right," highway = ",highway," oneway = ",oneway," line = ",d))
            stop()
          }
        }else if(!is.na(main) & !is.na(right)){
          if(main == right & (!is.na(right) | right == "no") & oneway == "One Way" )  {
            #Oneway road where main and the right have both been tagged
            osm$cycleway.left[d] <- "no"
          }else if(main != right){
            osm$cycleway.left[d] <- main
          }else if(main == right){
            osm$cycleway.left[d] <- "no"
          }else{
            message(paste0("Unusual Case: 5b  main = ",main," left = ",left," right = ",right," highway = ",highway," oneway = ",oneway," line = ",d))
            stop()
          }
        }else{
          message(paste0("Unusual Case: 1  main = ",main," left = ",left," right = ",right," highway = ",highway," oneway = ",oneway," line = ",d))
          qtm(osm[d,], lines.lwd = 4)
          Sys.sleep(3)
          stop()
        }

        if(!osm$cycleway.left[d] %in% c("no","track","lane","share_busway") | !osm$cycleway.right[d] %in% c("no","track","lane","share_busway") ){
          message("testing error")
          message(paste0("Unusual Case: 4  main = ",main," left = ",left," right = ",right," highway = ",highway," oneway = ",oneway," line = ",d))
          stop()
        }



        #End of Loop
        rm(left,right,main,highway,oneway)
      }
      rm(d)

      if(!all(c(osm$cycleway.left,osm$cycleway.right) %in% c("no","track","lane","share_busway"))){
        message("Invalid Cycle Infrastrucutre Tags After Cleaning")
        print(unique(c(osm$cycleway.left,osm$cycleway.right)))
        stop()
      }



      #for(d in 1:nrow(osm)){
      #  if(osm$highway[d] %in% c("bridleway", "corridor","construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track","bus_guideway")){
      #    #Left and right lanes are not a valid concept
      #    osm$cycleway.left[d] <- "no"
      #  }else{
      #    if(!is.na(osm$cycleway.left[d])){
      #      #Keep the existing value
      #
      #    }else if(!is.na(osm$cycleway[d])){
      #      if(osm$cycleway[d] %in% c("No","no")){
      #        #Do Nothing
      #        osm$cycleway.left[d] <- "no"
      #      }else if(osm$cycleway[d] %in% c("opposite","opposite_lane","opposite_track","opposite_share_busway")){
      #        #Do nothing as this is on the right side
      #        osm$cycleway.left[d] <- "no"
      #      }else if(osm$cycleway[d] %in% c("right","right:lane","lane:right")){
      #        osm$cycleway.left[d] <- "no"
      #      }else if(osm$cycleway[d] %in% c("left","lane:left","left:lane")){
      #        osm$cycleway.left[d] <- "lane"
      #      }else{
      #        osm$cycleway.left[d] <- osm$cycleway[d]
      #      }
      #    }else{
      #      #No data
      #      osm$cycleway.left[d] <- "no"
      #    }
      #  }
      #}
      #rm(d)
      #osm$cycleway.left[osm$cycleway.left == "designated"] <- "no" #unclear how this is used so assuming no actual lanes
      ###osm$cycleway.left <- as.factor(osm$cycleway.left)
      #summary(osm$cycleway.left)

      #for(e in 1:nrow(osm)){
      #  if(osm$highway[e] %in% c("bridleway", "corridor","construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track")){
      #    #Left and right lanes are not a valid concept
      #    osm$cycleway.right[e] <- "no"
      #  }else{
      #    if(!is.na(osm$cycleway.right[e])){
      #      #Keep the existing value
      #    }else if(!is.na(osm$cycleway[e])){
      #      if(osm$cycleway[e] %in% c("No","no")){
      #        #Do Nothing
      #        osm$cycleway.right[e] <- "no"
      #      }else if(osm$cycleway[e] %in% c("opposite","opposite_lane")){
      #        #Do something as this is on the right side
      #        osm$cycleway.right[e] <- "lane"
      #      }else if(osm$cycleway[e] == "opposite_track"){
      #        osm$cycleway.right[e] <- "track"
      #      }else if(osm$cycleway[e] %in% c("right","right:lane","lane:right")){
      #        osm$cycleway.right[e] <- "lane"
      #      }else if(osm$cycleway[e] %in% c("left","lane:left","left:lane")){
      #        osm$cycleway.right[e] <- "no"
      #      }else{
      #        osm$cycleway.right[e] <- osm$cycleway[e]
      #      }
      #    }else{
      #      #No data
      #      osm$cycleway.right[e] <- "no"
      #    }
      #  }
      #}
      #rm(e)

      #Cleanup Results
      #osm$cycleway.right[is.na(osm$cycleway.right)] <- "no"
      #osm$cycleway.right[osm$cycleway.right %in% c("opposite","opposite_lane","yes","both")] <- "lane"
      #osm$cycleway.right[osm$cycleway.right %in% c("opposite_track","separate","buffered cycle lane; contraflow to oneway") ] <- "track"
      #osm$cycleway.right[osm$cycleway.right == "opposite_share_busway" ] <- "share_busway"

      #osm$cycleway.left[is.na(osm$cycleway.left)] <- "no"
      #osm$cycleway.left[osm$cycleway.left %in% c("opposite","opposite_lane","yes","both")] <- "lane"
      #osm$cycleway.left[osm$cycleway.left %in% c("opposite_track","separate","buffered cycle lane; contraflow to oneway") ] <- "track"
      #osm$cycleway.left[osm$cycleway.left == "opposite_share_busway" ] <- "share_busway"


      #osm$cycleway.right <- as.factor(osm$cycleway.right)
      #summary(osm$cycleway.right)

      #Check for errors
      #If doubt assume no cycle infrastrucutre
      osm$cycleway.left[osm$cycleway.left == "share_busway" & osm$lanes.psv.forward == 0] <- "no"
      osm$cycleway.right[osm$cycleway.right == "share_busway" & osm$lanes.psv.backward == 0] <- "no"

      #Otherwise assume that all psv lanes are ok for cycling
      osm$cycleway.left[osm$lanes.psv.forward >= 1] <- "share_busway"
      osm$cycleway.right[osm$lanes.psv.backward >= 1] <- "share_busway"

      #Paths Don't have lanes
      osm$lanes.backward[osm$roadtype %in% c("Shared Path","Path - Cycling Forbidden","Segregated Shared Path","Cycleway")] <- 0
      osm$lanes.forward[osm$roadtype %in% c("Shared Path","Path - Cycling Forbidden","Segregated Shared Path","Cycleway")] <- 0

      #One way roads don't have back lanes
      osm$lanes.backward[osm$onewaysummary == "One Way"] <- 0

      #Final checks
      #stop if errors found

      if(length(unique(osm$cycleway.right)[!(unique(osm$cycleway.right) %in% c("no","lane","track","share_busway"))]) != 0){
        message(paste0("Unrecognised values in cycleway.right" ))
        print(unique(osm$cycleway.right))
        stop()
      }

      if(length(unique(osm$cycleway.left)[!(unique(osm$cycleway.left) %in% c("no","lane","track","share_busway"))]) != 0){
        message(paste0("Unrecognised values in cycleway.left" ))
        print(unique(osm$cycleway.left))
        stop()
      }




      ############################################################################################################
      #remove unneeded columns
      osm <- osm[,c("osm_id","name","ref","highway","junction","roadtype","onewaysummary","elevation","maxspeed","segregated","sidewalk","cycleway.left","lanes.psv.forward","lanes.forward","lanes.backward","lanes.psv.backward","cycleway.right","region","geometry")]
      #osm <- osm[,c("osm_id","name","ref","highway","junction","roadtype","onewaysummary","elevation","maxspeed","segregated","sidewalk","cycleway.left","lanes.psv.forward","lanes.forward","lanes.backward","lanes.psv.backward","cycleway.right","geometry")]


      #########################################################
      # add in quietness scores
      #unique(osm$highway)
      quiet.scores <- read.csv("../cyipt/input-data/quietness.csv", stringsAsFactors = F)
      quiet.scores <- quiet.scores[quiet.scores$highway != "",]

      #check that all possabilities are covered
      osm.check <- as.data.frame(osm[,c("highway","cycleway.left","cycleway.right")])
      osm.check$geometry <- NULL
      osm.check <- unique(osm.check)
      check <- do.call(paste0, osm.check) %in% do.call(paste0, quiet.scores[,c("highway","cycleway.left","cycleway.right")])
      if(!all(check)){
        osm.check <- osm.check[!check,]
        message(paste0("Warning: ",nrow(osm.check)," types are missing from the quietness input please fix, see osm.check for details"))
        stop()
      }
      rm(check)

      osm <- left_join(osm,quiet.scores, by = c("highway" = "highway","cycleway.left" = "cycleway.left","cycleway.right" = "cycleway.right"))
      osm$quietness <- as.integer(osm$quietness)

      saveRDS(osm,paste0("../cyipt-bigdata/osm-clean/",regions[a],"/osm-lines.Rds"))
      rm(osm, quiet.scores, osm.check)



    }

  }else{
    print("Input File Missing")
  }
}
rm(a,regions)
