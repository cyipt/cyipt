# COmpare the Width of thr road required and availiable

############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################

library(sf)
library(dplyr)
library(parallel)
library(igraph)


#Settings now come from master file
#skip <- FALSE #Skip Files that already have PCT values
#ncores <- 4 #number of cores to use in parallel processing
#overwrite <- FALSE #Overwrite or create new file



#List folders
#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
    #Check if PCT values exist in the file
    if(all(c("calcwidthnow","calcwidthrec") %in% names(osm)) & skip){
      message(paste0("Scheme numbers already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting infrastructure types values for ",regions[b]," at ",Sys.time()))

      #If overwriting remove old data
      col.to.keep <- names(osm)[!names(osm) %in% c("calcwidthnow","calcwidthrec")]
      osm <- osm[,col.to.keep]
      rm(col.to.keep)

      #Add Main Roadwidths
      osm$calcwidthnow <- NA

      for(c in 1:nrow(osm)){
        if(osm$highway[c] %in% c("residential")){
          #Rules for Residential Roads

          # Lane Widths
          w.lanes <- 3 * sum(osm$lanes.backward[c], osm$lanes.forward[c])
          w.psv <- 4.5 * sum(osm$lanes.psv.forward[c] , osm$lanes.psv.backward[c])

          # footway
          footway <- osm$sidewalk[c]
          if(footway %in% c("no","Not Applicable")){
            w.pave <- 0
          }else if(footway %in% c("left","right")){
            w.pave <- 1.8
          }else if(footway == "both"){
            w.pave <- 3.6
          }else{
            message(paste0("Footway error on line",c))
            stop()
          }

          #cycleways
          cycleleft <- osm$cycleway.left[c]
          if(cycleleft %in% c("no","share_busway")){
            w.cycleleft <- 0
          }else if(cycleleft == "lane"){
            w.cycleleft <- 1.5
          }else if(cycleleft == "track"){
            w.cycleleft <- 2
          }else{
            message(paste0("cycleleft error on line",c))
            stop()
          }

          cycleright <- osm$cycleway.right[c]
          if(cycleright %in% c("no","share_busway")){
            w.cycleright <- 0
          }else if(cycleright == "lane"){
            w.cycleright <- 1.5
          }else if(cycleright == "track"){
            w.cycleright <- 2
          }else{
            message(paste0("cycleright error on line",c))
            stop()
          }

          osm$calcwidthnow[c] <- sum(w.cycleright, w.cycleleft, w.pave, w.psv, w.lanes)
          rm(w.cycleright, w.cycleleft, w.pave, w.psv, w.lanes, cycleright, cycleleft, footway)


        }else if(osm$highway[c] %in% c("pedestrian","steps","path","bridleway","cycleway","track", "footway")){
          # rules for off road
          rtype <- osm$roadtype[c]
          if(rtype == "Shared Path"){
            w.path  <- 3
          }else if(rtype == "Cycleway"){
            w.path  <- 3
          }else if(rtype == "Segregated Cycleway"){
            w.path  <- 4
          }else if(rtype == "Path - Cycling Forbidden"){
            w.path  <- 2
          }else if(rtype == "Segregated Shared Path"){
            w.path  <- 6
          }else{
            message(paste0("Path error on line",c))
            stop()
          }

          osm$calcwidthnow[c] <- w.path
          rm(w.path,rtype)

        }else if(osm$highway[c] %in% c("motorway")){
          # rules for motorway

          # Lane Widths
          w.lanes <- 3.65 * sum(osm$lanes.backward[c], osm$lanes.forward[c])
          w.psv <- 4.5 * sum(osm$lanes.psv.forward[c] , osm$lanes.psv.backward[c])

          #Add Hard Sholder
          if(osm$onewaysummary[c] == "Two Way"){
            w.hard <- 6.6
          }else{
            w.hard <- 3.3
          }

          osm$calcwidthnow[c] <- sum(w.hard, w.psv, w.lanes)
          rm(w.hard, w.psv, w.lanes)



        }else{
          # Rules for all other roads

          # Lane Widths
          w.lanes <- 3.65 * sum(osm$lanes.backward[c], osm$lanes.forward[c])
          w.psv <- 4.5 * sum(osm$lanes.psv.forward[c] , osm$lanes.psv.backward[c])

          # footway
          footway <- osm$sidewalk[c]
          if(footway %in% c("no","Not Applicable")){
            w.pave <- 0
          }else if(footway %in% c("left","right")){
            w.pave <- 1.8
          }else if(footway == "both"){
            w.pave <- 3.6
          }else{
            message(paste0("Footway error on line",c))
            stop()
          }

          #cycleways
          cycleleft <- osm$cycleway.left[c]
          if(cycleleft %in% c("no","share_busway")){
            w.cycleleft <- 0
          }else if(cycleleft == "lane"){
            w.cycleleft <- 1.5
          }else if(cycleleft == "track"){
            w.cycleleft <- 2
          }else{
            message(paste0("cycleleft error on line",c))
            stop()
          }

          cycleright <- osm$cycleway.right[c]
          if(cycleright %in% c("no","share_busway")){
            w.cycleright <- 0
          }else if(cycleright == "lane"){
            w.cycleright <- 1.5
          }else if(cycleright == "track"){
            w.cycleright <- 2
          }else{
            message(paste0("cycleright error on line",c))
            stop()
          }

          osm$calcwidthnow[c] <- sum(w.cycleright, w.cycleleft, w.pave, w.psv, w.lanes)
          rm(w.cycleright, w.cycleleft, w.pave, w.psv, w.lanes, cycleright, cycleleft, footway)

        }
      }
      rm(c)
      #Now calcualte the width with reccomended infrastrucutre

      #Add Main Roadwidths
      osm$calcwidthrec <- NA

      for(d in 1:nrow(osm)){
        if(osm$Change[d] %in% c("no change", "downgrade")){
          osm$calcwidthrec[d] <- osm$calcwidthnow[d]
        }else{
          if(osm$highway[d] %in% c("residential")){
            #Rules for Residential Roads

            # Lane Widths
            w.lanes <- 3 * sum(osm$lanes.backward[d], osm$lanes.forward[d])
            w.psv <- 4.5 * sum(osm$lanes.psv.forward[d] , osm$lanes.psv.backward[d])

            # footway
            footway <- osm$sidewalk[d]
            if(footway %in% c("no","Not Applicable")){
              w.pave <- 0
            }else if(footway %in% c("left","right")){
              w.pave <- 1.8
            }else if(footway == "both"){
              w.pave <- 3.6
            }else{
              message(paste0("Footway error on line",d))
              stop()
            }

            #cycleways
            cyclerec <- osm$Recommended[d]
            if(cyclerec %in% c("Stepped Cycle Tracks", "Segregated Cycle Track") ){
              if(osm$pct.census[d] < 150){
                w.cycle <- 5
              }else if(osm$pct.census[d] >= 150 & osm$pct.census[d] < 750){
                w.cycle <- 6
              }else if(osm$pct.census[d] >= 750){
                w.cycle <- 8
              }else{
                message(paste0("Cycleway error on line",d))
                stop()
              }

            }else if(cyclerec == "Cycle Lanes"){
              w.cycle <- 4
            }else if(cyclerec == "Cycle Lanes with light segregation"){
              w.cycle <- 5
            }else{
              w.cycle <- 0
            }

            osm$calcwidthrec[d] <- sum(w.cycle, w.pave, w.psv, w.lanes)
            rm(w.cycle, w.pave, w.psv, w.lanes, cyclerec, footway)


          }else if(osm$highway[d] %in% c("pedestrian","steps","path","bridleway","cycleway","track", "footway")){
            # rules for off road
            cyclerec <- osm$Recommended[d]

            if(cyclerec == "Segregated Cycle Track on Path"){
              w.path  <- 6
            }else if(cyclerec == "Cycle Lane on Path"){
              w.path  <- 3
            }else{
              w.path  <- osm$calcwidthnow[d]
            }

            osm$calcwidthrec[d] <- w.path
            rm(w.path, cyclerec)

          }else if(osm$highway[d] %in% c("motorway")){
            # rules for motorway

            # Lane Widths
            w.lanes <- 3.65 * sum(osm$lanes.backward[d], osm$lanes.forward[d])
            w.psv <- 4.5 * sum(osm$lanes.psv.forward[d] , osm$lanes.psv.backward[d])

            #Add Hard Sholder
            if(osm$onewaysummary[d] == "Two Way"){
              w.hard <- 6.6
            }else{
              w.hard <- 3.3
            }

            osm$calcwidthrec[d] <- sum(w.hard, w.psv, w.lanes)
            rm(w.hard, w.psv, w.lanes)



          }else{
            # Rules for all other roads

            # Lane Widths
            w.lanes <- 3.65 * sum(osm$lanes.backward[d], osm$lanes.forward[d])
            w.psv <- 4.5 * sum(osm$lanes.psv.forward[d] , osm$lanes.psv.backward[d])

            # footway
            footway <- osm$sidewalk[d]
            if(footway %in% c("no","Not Applicable")){
              w.pave <- 0
            }else if(footway %in% c("left","right")){
              w.pave <- 1.8
            }else if(footway == "both"){
              w.pave <- 3.6
            }else{
              message(paste0("Footway error on line",c))
              stop()
            }

            #cycleways
            cyclerec <- osm$Recommended[d]
            if(cyclerec %in% c("Stepped Cycle Tracks", "Segregated Cycle Track") ){
              if(osm$pct.census[d] < 150){
                w.cycle <- 5
              }else if(osm$pct.census[d] >= 150 & osm$pct.census[d] < 750){
                w.cycle <- 6
              }else if(osm$pct.census[d] >= 750){
                w.cycle <- 8
              }else{
                message(paste0("Cycleway error on line",d))
                stop()
              }

            }else if(cyclerec == "Cycle Lanes"){
              w.cycle <- 4
            }else if(cyclerec == "Cycle Lanes with light segregation"){
              w.cycle <- 5
            }else{
                w.cycle <- 0
              }

              osm$calcwidthrec[d] <- sum(w.cycle, w.pave, w.psv, w.lanes)
              rm(w.cycle, w.pave, w.psv, w.lanes, cyclerec, footway)

          }
        }

      }
      rm(d)

      #Compare Measured and Calcualted Width
      osm$widthdiffnow <- osm$widthpath - osm$calcwidthnow
      hist(osm$widthdiffnow, breaks = -40:40)

      osm$widthdiffrec <- osm$widthpath - osm$calcwidthrec
      hist(osm$widthdiffrec, breaks = -40:40)

      #Categorise with Width Difference
      osm$widthstatus <- NA

      for(e in 1:nrow(osm)){
        wd <- osm$widthdiffrec[e]
        if(is.na(wd)){
          osm$widthstatus[e] <- "Missing Width Data"
        }else if(wd > 1){
          osm$widthstatus[e] <- "More than sufficient width"
        }else if(wd <= 1 & wd >= -1 ){
          osm$widthstatus[e] <- "Approximatly sufficient width"
        }else if(wd < -1 & wd >= -2 ){
          osm$widthstatus[e] <- "Width Constrained"
        }else if(wd < -2){
          osm$widthstatus[e] <- "Insufficient width"
        }
      }
      rm(wd)

      #qtm(osm[osm$Change %in% c("upgrade","upgrade (one side)"),], lines.col = "widthstatus", lines.lwd = 3)


      #Save results
      if(overwrite){
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
      }else{
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines-widthseval.Rds"))

      }
      rm(osm)

    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)
