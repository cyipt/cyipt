# Compare the Width of thr road required and availiable

# function
calc.width.osm <- function(i){
  if(osm$highway[i] %in% c("residential")){
    #Rules for Residential Roads

    # Lane Widths
    w.lanes <- 3 * sum(osm$lanes.backward[i], osm$lanes.forward[i])
    w.psv <- 4.5 * sum(osm$lanes.psv.forward[i] , osm$lanes.psv.backward[i])

    # footway
    footway <- osm$sidewalk[i]
    if(footway %in% c("no","Not Applicable")){
      w.pave <- 0
    }else if(footway %in% c("left","right")){
      w.pave <- 1.8
    }else if(footway == "both"){
      w.pave <- 3.6
    }else{
      message(paste0("Footway error on line",i))
      stop()
    }

    #cycleways
    cycleleft <- osm$cycleway.left[i]
    if(cycleleft %in% c("no","share_busway")){
      w.cycleleft <- 0
    }else if(cycleleft == "lane"){
      w.cycleleft <- 1.5
    }else if(cycleleft == "track"){
      w.cycleleft <- 2
    }else{
      message(paste0("cycleleft error on line",i))
      stop()
    }

    cycleright <- osm$cycleway.right[i]
    if(cycleright %in% c("no","share_busway")){
      w.cycleright <- 0
    }else if(cycleright == "lane"){
      w.cycleright <- 1.5
    }else if(cycleright == "track"){
      w.cycleright <- 2
    }else{
      message(paste0("cycleright error on line",i))
      stop()
    }

    calcwidthnow <- sum(w.cycleright, w.cycleleft, w.pave, w.psv, w.lanes)
    rm(w.cycleright, w.cycleleft, w.pave, w.psv, w.lanes, cycleright, cycleleft, footway)


  }else if(osm$highway[i] %in% c("pedestrian","steps","path","bridleway","cycleway","track", "footway")){
    # rules for off road
    rtype <- osm$roadtype[i]
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
      message(paste0("Path error on line",i))
      stop()
    }

    calcwidthnow <- w.path
    rm(w.path,rtype)

  }else if(osm$highway[i] %in% c("motorway")){
    # rules for motorway

    # Lane Widths
    w.lanes <- 3.65 * sum(osm$lanes.backward[i], osm$lanes.forward[i])
    w.psv <- 4.5 * sum(osm$lanes.psv.forward[i] , osm$lanes.psv.backward[i])

    #Add Hard Sholder
    if(osm$onewaysummary[i] == "Two Way"){
      w.hard <- 6.6
    }else{
      w.hard <- 3.3
    }

    calcwidthnow <- sum(w.hard, w.psv, w.lanes)
    rm(w.hard, w.psv, w.lanes)

  }else{
    # Rules for all other roads

    # Lane Widths
    w.lanes <- 3.65 * sum(osm$lanes.backward[i], osm$lanes.forward[i])
    w.psv <- 4.5 * sum(osm$lanes.psv.forward[i] , osm$lanes.psv.backward[i])

    # footway
    footway <- osm$sidewalk[i]
    if(footway %in% c("no","Not Applicable")){
      w.pave <- 0
    }else if(footway %in% c("left","right")){
      w.pave <- 1.8
    }else if(footway == "both"){
      w.pave <- 3.6
    }else{
      message(paste0("Footway error on line",i))
      stop()
    }

    #cycleways
    cycleleft <- osm$cycleway.left[i]
    if(cycleleft %in% c("no","share_busway")){
      w.cycleleft <- 0
    }else if(cycleleft == "lane"){
      w.cycleleft <- 1.5
    }else if(cycleleft == "track"){
      w.cycleleft <- 2
    }else{
      message(paste0("cycleleft error on line",i))
      stop()
    }

    cycleright <- osm$cycleway.right[i]
    if(cycleright %in% c("no","share_busway")){
      w.cycleright <- 0
    }else if(cycleright == "lane"){
      w.cycleright <- 1.5
    }else if(cycleright == "track"){
      w.cycleright <- 2
    }else{
      message(paste0("cycleright error on line",i))
      stop()
    }

    calcwidthnow <- sum(w.cycleright, w.cycleleft, w.pave, w.psv, w.lanes)
    rm(w.cycleright, w.cycleleft, w.pave, w.psv, w.lanes, cycleright, cycleleft, footway)

  }

  return(calcwidthnow)
}

# reccomended width

calc.width.recc <- function(d){
  if(osm$Change[d] %in% c("no change", "downgrade")){
    calcwidthrec <- osm$calcwidthnow[d]
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

      calcwidthrec <- sum(w.cycle, w.pave, w.psv, w.lanes)
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

      calcwidthrec <- w.path
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

      calcwidthrec <- sum(w.hard, w.psv, w.lanes)
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
        message(paste0("Footway error on line",i))
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

      calcwidthrec <- sum(w.cycle, w.pave, w.psv, w.lanes)
      rm(w.cycle, w.pave, w.psv, w.lanes, cyclerec, footway)

    }
  }
  return(calcwidthrec)
}

# main code

regions <- regions.todo

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds"))
    #Check if PCT values exist in the file
    if(all(c("calcwidthnow","calcwidthrec") %in% names(osm)) & skip){
      message(paste0("Road width comparision calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting road width comparision for ",regions[b]," at ",Sys.time()))

      #If overwriting remove old data
      col.to.keep <- names(osm)[!names(osm) %in% c("calcwidthnow","calcwidthrec")]
      osm <- osm[,col.to.keep]
      rm(col.to.keep)

      #Add Main Roadwidths
      osm$calcwidthnow <- sapply(1:nrow(osm), calc.width.osm)

      #Now calcualte the width with reccomended infrastrucutre
      osm$calcwidthrec <- sapply(1:nrow(osm), calc.width.recc)

      #Compare Measured and Calcualted Width
      osm$widthdiffnow <- osm$widthpath - osm$calcwidthnow
      osm$widthdiffrec <- osm$widthpath - osm$calcwidthrec

      #Categorise with Width Difference
      osm$widthstatus <- NA

      for(e in 1:nrow(osm)){
        wd <- osm$widthdiffrec[e]
        if(is.na(wd)){
          osm$widthstatus[e] <- "Missing Width Data"
        }else if(wd > 1){
          osm$widthstatus[e] <- "More than sufficient width"
        }else if(wd <= 1 & wd >= -1 ){
          osm$widthstatus[e] <- "About sufficient width"
        }else if(wd < -1 & wd >= -2 ){
          osm$widthstatus[e] <- "Width Constrained"
        }else if(wd < -2){
          osm$widthstatus[e] <- "Insufficient width"
        }
        rm(wd)
      }
      rm(e)


      #Save results
      if(overwrite){
        saveRDS(osm,paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds"))
      }else{
        saveRDS(osm,paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines-widthseval.Rds"))

      }
      rm(osm)

    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)
