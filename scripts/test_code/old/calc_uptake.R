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


#List folders
#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo
#regions <- "Bristol"

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))){
    #Check if Uptake values exist
    if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct-up.Rds")) & skip){
      message(paste0("Uptake numbers already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting uptake values for ",regions[b]," at ",Sys.time()))

      #Get file
      osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
      model <- readRDS("../cyipt/input-data/LSOAmodel-newbusy.Rds")

      # Get PCT Data
      pct <- readRDS(paste0("../cyipt-securedata/pct-regions/",regions[b],".Rds"))
      pct2osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct2osm.Rds"))
      osm2pct <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm2pct.Rds"))

      #calc busyness score
      osm$quietness <- as.integer(osm$quietness)
      osm$busyBefore <- osm$length / (osm$quietness/100)

      #Update Busyness score based on Reccomended infra
      # simple test make quietness always equal to 100



      quiet.scores <- read.csv("../cyipt/input-data/quietness.csv", stringsAsFactors = F)
      quiet.scores <- quiet.scores[quiet.scores$highway != "",]
      quiet.scores <- quiet.scores[quiet.scores$cycleway.left == quiet.scores$cycleway.right,]
      quiet.scores <- quiet.scores[quiet.scores$cycleway.left != "share_busway",]
      quiet.scores$cycleway.right <- NULL
      names(quiet.scores) <- c("highway","recTemp","quietnessAfter" )

      osm$recTemp <- NA

      for(k in 1:nrow(osm)){
        rec <- osm$Recommended[k]
        if(rec %in% c("Stepped Cycle Tracks","Segregated Cycle Track","Segregated Cycle Track on Path")){
          osm$recTemp[k] <- "track"
        }else if(rec %in% c("Cycle Lanes","Cycle Lanes with light segregation","Cycle Lane on Path")){
          osm$recTemp[k] <- "lane"
        }else{
          osm$recTemp[k] <- "no"
        }
      }

      osm <- left_join(osm,quiet.scores, by = c("highway" = "highway","recTemp" = "recTemp"))
      osm$quietnessAfter <- as.integer(osm$quietnessAfter)
      osm$busyAfter <- osm$length / (osm$quietnessAfter/100)




      #get the list of scheme_nos
      scheme_nos <- unique(osm$group_id)
      scheme_nos <- scheme_nos[!is.na(scheme_nos)]
      scheme_nos <- scheme_nos[order(scheme_nos)]

      if(!all(scheme_nos == 1:length(scheme_nos))){
        message("Problem with scheme numbering")
        stop()
      }

      osm$group_id[is.na(osm$group_id)] <- 0 # repalce NAs with 0 scheme number

      # Loop over schemes
      uptake.list <- list()
      #for(j in 1:2){
      for(j in scheme_nos){
        #message(paste0("Doing Scheme ",j))
        #Get the roads in the schemes

        scheme.osm_ids <- osm$id[osm$group_id == j] # get the osm ids for this scheme
        scheme.pct_ids <- unique(unlist(osm2pct[scheme.osm_ids])) # get the pct ids for this scheme
        pct.scheme <- pct[scheme.pct_ids,]


        #scheme.changes <- lapply(scheme.pct_ids, getbusyscores.scheme, osm_change = scheme.osm_ids)

        ##########################################################
        #Parallel
        start <- Sys.time()
        fun <- function(cl){
          parLapply(cl, scheme.pct_ids,getbusyscores.scheme, osm_change = scheme.osm_ids)
        }
        cl <- makeCluster(ncores) #make clusert and set number of cores
        clusterExport(cl=cl, varlist=c("pct2osm", "osm","scheme.osm_ids"))
        clusterExport(cl=cl, c('getbusyscores.scheme') )
        clusterEvalQ(cl, {library(sf)})
        scheme.changes <- fun(cl)
        stopCluster(cl)

        end <- Sys.time()
        message(paste0("Did ",length(scheme.pct_ids)," pct routes in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
        rm(start,end, cl, fun)
        ##########################################################
        scheme.changes <- bind_rows(scheme.changes)

        pct.scheme <- bind_cols(pct.scheme,scheme.changes)

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

        rm(pct.scheme, pct.scheme.mat, uptake, cor, scheme.osm_ids, scheme.pct_ids)

        #message(paste0("Done scheme ",j," at ",Sys.time()))

      }

      uptake.fin <- do.call("rbind",uptake.list)
      uptake.fin$pup <- uptake.fin$model.future / uptake.fin$model.now
      uptake.fin$expect <- round(uptake.fin$pup * uptake.fin$census,0)
      uptake.fin$change <- uptake.fin$expect - uptake.fin$census

      #saveRDS(pct.scheme,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct-up.Rds"))
      saveRDS(uptake.fin,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct-up.Rds"))


      rm(osm,model, osm2pct, pct2osm, scheme_nos)

    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)




















# Test of calcualting uptake for each scenario in bristol

# set up


















