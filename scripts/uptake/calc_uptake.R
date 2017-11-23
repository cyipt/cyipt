##########################
# rewrite for performance


############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################

library(sf)
library(dplyr)
library(tmap)
library(pbapply)
library(parallel)
source("R/functions.R")
tmap_mode("view")

#osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",region,"/osm-lines.Rds"))


#Settings now come from master file
#skip <- FALSE #Skip Files that already have PCT values
#ncores <- 4 #number of cores to use in parallel processing
#overwrite <- FALSE #Overwrite or create new file

#Functions

getbusyscores <- function(c){
  osm.ids <- pct2osm[[c]]
  osm.sub <- osm[osm.ids,]
  #print(paste0("Show ", c))
  #plot(pct$geometry[c], lwd = 5, col = "black" )
  #plot(osm.sub$geometry, lwd = 3, col = "green" , add = T)
  #Sys.sleep(5)

  busyBefore <- sum(osm.sub$busyBefore, na.rm = T)
  busyAfter <- sum(osm.sub$busyAfter, na.rm = T)
  lengthOSM <- sum(osm.sub$length, na.rm = T)

  result <- data.frame(busyBefore = busyBefore,busyAfter = busyAfter, lengthOSM = lengthOSM)
  return(result)
}


getbusyscores.scheme <- function(c,osm_change){
  osm.ids <- pct2osm[[c]] #All the osms on the route
  osm.sub <- osm[osm.ids,]
  busyBefore <- sum(osm.sub$busyBefore, na.rm = T)
  lengthOSM <- sum(osm.sub$length, na.rm = T)

  osm.sub.unchange <- osm.sub[!(osm.sub$id %in% osm_change),]
  osm.sub.change <- osm.sub[(osm.sub$id %in% osm_change),]

  busyAfter <- sum(osm.sub.change$busyAfter, na.rm = T) + sum(osm.sub.unchange$busyBefore, na.rm = T)

  result <- data.frame(busyBefore = busyBefore,busyAfter = busyAfter, lengthOSM = lengthOSM)
  return(result)
}

##########################################################################################
calcChangeBusy <- function(k){
  #pct_.inter.id <- inter[k]
  message(paste0("Doing Line ",k))

  line <- pct.scheme[k,]

  if(line$todo[1]){
    #line2 <- pct.all[inter[[j]][k],]
    #qtm(line)

    # Select the object of intrest in df1 and then find nearby objects in df2
    grid_ids <- unlist(grid_pct[ inter[[j]][k] ]) #compex because first of pct.scheme is not first of pct.all on which grid is made
    ids_grid <- unique(unlist(grid_osm[grid_ids]))
    osm.sub <- osm[ids_grid,]
    #qtm(grid[grid_ids]) + qtm(osm.schemes[j,], lines.lwd = 5, lines.col = "black") + qtm(line, lines.lwd = 3,lines.col = "green") + qtm(line2, lines.lwd = 3,lines.col = "red")

    #Now check for actual intersection between the two objects
    # use buffer because pct lines don always perfectyl match osm roads
    line.buff <- st_buffer(line,4,nQuadSegs = 2)
    line.buff <- line.buff[,"ID"]
    osm.sub <- osm.sub[line.buff,]

    #always end up loosing the start and end roads so clip the raods to the buffer
    osm.sub <- st_intersection(line.buff, osm.sub)
    osm.sub$ID <- 1:nrow(osm.sub)

    #Check for 3 point intersection
    #match = sapply(1:nrow(osm.sub), roadsOnLine)
    #bm <- microbenchmark::microbenchmark(mapply(roadsOnLine2,osm.sub$geometry,line$geometry))
    match = mapply(roadsOnLine2,osm.sub$geometry,line$geometry) #internatised the function
    #match <- unlist(match)
    osm.sub <- osm.sub[match,]

    # CHang in busyness
    busyance.new <- vector(mode= "numeric", length = nrow(osm.sub))
    for(l in 1:nrow(osm.sub)){
      #Check for NA first
      if(is.na(osm.sub$group_id[l])){
        busyance.new[l] <- osm.sub$busyBefore[l]
      }else if(osm.sub$group_id[l] == j){
        busyance.new[l] <- osm.sub$busyAfter[l]
      }else{
        busyance.new[l] <- osm.sub$busyBefore[l]
      }
    }

    s1 <- sum(osm.sub$busyBefore, na.rm = T)
    s2 <- sum(busyance.new, na.rm = T)

    #message(paste0("Line ",k," has had its busyance changed by ",round((s1-s2)/s1 * 100,2)," %" ))
    result <- data.frame(id = line$ID, before = s1, after = s2)
    return(result)
  }else{
    return(NULL)
  }


}



#List folders
#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo
#regions <- "Bristol"

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
    #osm <- readRDS("../cyipt-bigdata/osm-prep/Bristol/")
    model <- readRDS("../cyipt/input-data/LSOAmodel-newbusy.Rds")
    #Check if PCT values exist in the file
    if(all(c("FILL ME IN") %in% names(osm)) & skip){
      message(paste0("Uptake numbers already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting uptake values for ",regions[b]," at ",Sys.time()))

      #If overwriting remove old data
      #col.to.keep <- names(osm)[!names(osm) %in% c("FILL ME IN")]
      #osm <- osm[,col.to.keep]
      #rm(col.to.keep)

      # Get PCT Data
      pct <- readRDS(paste0("../cyipt-securedata/pct-regions/",regions[b],".Rds"))
      pct2osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct2osm.Rds"))
      osm2pct <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm2pct.Rds"))

      #calc busyness score
      osm$quietness <- as.integer(osm$quietness)
      osm$busyBefore <- osm$length / (osm$quietness/100)

      #Update Busyness scaore based on Reccomended infra
      # simple test make quietness always equal to 100
      osm$busyAfter <- osm$busy
      for(i in 1:nrow(osm)){
        if(osm$Recommended[i] != "None"){
          osm$busyAfter[i] <- osm$length[i]
        }
      }

      #get the list of scheme_nos
      scheme_nos <- unique(osm$group_id)
      scheme_nos <- scheme_nos[!is.na(scheme_nos)]
      scheme_nos <- scheme_nos[order(scheme_nos)]

      if(!all(scheme_nos == 1:length(scheme_nos))){
        message("Problem with scheme numbering")
        stop()
      }

      #osm.schemes <- osm[!is.na(osm$group_id),]

      osm$group_id[is.na(osm$group_id)] <- 0 # repalce NAs with 0 scheme number

      #For each PCT Line get the busyness before and after off the OSM network



      #t1 <- Sys.time()
      #foo <- lapply(1:nrow(pct),getbusyscores)
      #foo <- bind_rows(foo)
      #t2 <- Sys.time()
      #difftime(t2,t1)

      #pct2 <- bind_cols(pct,foo)
      #pct2$difflength = round(((pct2$lengthOSM - pct2$length) / pct2$length) * 100,2)
      #hist(pct2$difflength)



      # Loop over schemes
      uptake.list <- list()
      #for(j in 1:2){
      for(j in scheme_nos){
        #message(paste0("Doing Scheme ",j))
        #Get the roads in the schemes
        #osm.scheme <- osm.schemes[osm.schemes$group_id == j,]

        scheme.osm_ids <- osm$id[osm$group_id == j] # get the osm ids for this scheme
        scheme.pct_ids <- unique(unlist(osm2pct[scheme.osm_ids])) # get the pct ids for this scheme
        pct.scheme <- pct[scheme.pct_ids,]


        scheme.changes <- lapply(scheme.pct_ids, getbusyscores.scheme, osm_change = scheme.osm_ids)
        scheme.changes <- bind_rows(scheme.changes)

        pct.scheme <- bind_cols(pct.scheme,scheme.changes)

        #pct.scheme <- left_join(pct.scheme, respar, by = c("ID" = "id"))
        #pct.scheme$total <- pct.scheme$pct.census + pct.scheme$onfoot + pct.scheme$motorvehicle + pct.scheme$publictransport + pct.scheme$other
        #foo <- pct.scheme$pct.census + pct.scheme$onfoot + pct.scheme$motorvehicle + pct.scheme$publictransport + pct.scheme$other

        pct.scheme.mat <- as.data.frame(pct.scheme[,c("length","busyBefore","cum_hill","change_elev","dif_max_min","up_tot","down_tot","av_incline","calories",
                                       "male_16_24","male_25_35","male_35_49","male_50_64","male_65_74","male_75p",
                                       "female_16_24","female_25_34", "female_35_49", "female_50_64", "female_65_74", "female_75p")])
        pct.scheme.mat$geometry <- NULL
        pct.scheme.mat <- as.matrix(pct.scheme.mat)
        cor <- round(cor(predict(object = model, pct.scheme.mat), pct.scheme$pct.census)^2,4)

        message(paste0(Sys.time()," Scheme ",j," has a correlation of ",cor))


        pct.scheme$model.before <- predict(object = model, pct.scheme.mat)

        pct.scheme.mat <- as.data.frame(pct.scheme[,c("length","busyAfter","cum_hill","change_elev","dif_max_min","up_tot","down_tot","av_incline","calories",
                                                      "male_16_24","male_25_35","male_35_49","male_50_64","male_65_74","male_75p",
                                                      "female_16_24","female_25_34", "female_35_49", "female_50_64", "female_65_74", "female_75p")])
        pct.scheme.mat$geometry <- NULL
        names(pct.scheme.mat)  <-  c("length","busyBefore","cum_hill","change_elev","dif_max_min","up_tot","down_tot","av_incline","calories",
                                     "male_16_24","male_25_35","male_35_49","male_50_64","male_65_74","male_75p",
                                     "female_16_24","female_25_34", "female_35_49", "female_50_64", "female_65_74", "female_75p")
        pct.scheme.mat <- as.matrix(pct.scheme.mat)
        pct.scheme$model.after <- predict(object = model, pct.scheme.mat)

        #Uptake for scheme is
        uptake <- data.frame(scheme = j, census = sum(pct.scheme$pct.census), model.now = sum(pct.scheme$model.before), model.future = sum(pct.scheme$model.after), correlation = cor)
        uptake.list[[j]] <- uptake

        #message(paste0("Done scheme ",j," at ",Sys.time()))

      }

      uptake.fin <- do.call("rbind",uptake.list)
      uptake.fin$pup <- uptake.fin$model.future / uptake.fin$model.now
      uptake.fin$expect <- round(uptake.fin$pup * uptake.fin$census,0)
      uptake.fin$change <- uptake.fin$expect - uptake.fin$census

      #saveRDS(pct.scheme,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct-up.Rds"))
      saveRDS(uptake.fin,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct-up.Rds"))

      #Save results
      #if(overwrite){
        #saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
        #saveRDS(schemes,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes.Rds"))
      #}else{
        #saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines-schemes.Rds"))
        #saveRDS(schemes,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes.Rds"))
      #}
      rm(osm,schemes)

    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)




















# Test of calcualting uptake for each scenario in bristol

# set up


















