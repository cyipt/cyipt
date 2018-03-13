##########################
# Updated for new model from robin


############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################

library(sf)
library(dplyr)
library(tmap)
library(xgboost)
#library(pbapply)
library(parallel)
source("R/functions.R")
tmap_mode("view")

#osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",region,"/osm-lines.Rds"))


#Settings now come from master file
#skip <- FALSE #Skip Files that already have PCT values
#ncores <- 4 #number of cores to use in parallel processing
#overwrite <- FALSE #Overwrite or create new file

#Functions
get.exposure <- function(c){
  route.pct.id <- (1:nrow(pct))[pct$ID == pct.scheme$ID[c] ]
  route.osmids <- unique(pct2osm[[route.pct.id]])
  route.osmids <- route.osmids[route.osmids %in% scheme.osm_ids]
  route.osm <- osm[route.osmids,]
  result <- data.frame(ID = as.character(pct.scheme$ID[c]),
                       lengthOffRoad = sum(route.osm$length[route.osm$Recommended %in% c("Stepped Cycle Tracks","Segregated Cycle Track","Cycle Lane on Path","Segregated Cycle Track on Path")]),
                       lengthOnRoad = sum(route.osm$length[route.osm$Recommended %in% c("Cycle Street","Cycle Lanes","Cycle Lanes with light segregation")])
  )
  return(result)
}

# Variaibles in the model

modelvars <- c("cycleway","path",
                "other20_N","other20_I","other30_N","other30_I","other40_N",
                "primary20_N","primary20_I","primary30_N","primary30_I","primary40_N","primary40_I",
                "residential20_N", "residential20_I", "residential30_N", "residential30_I", "residential40_N","residential40_I",
                "secondary20_N","secondary20_I","secondary30_N","secondary30_I","secondary40_N","secondary40_I",
                "tertiary20_N","tertiary20_I","tertiary30_N","tertiary30_I","tertiary40_N","tertiary40_I",
                "trunk20_N","trunk30_N","trunk30_I","trunk40_N","trunk40_I")


# infra change
get.infrachange <- function(x){
  route.pct.id <- (1:nrow(pct))[pct$ID == pct.scheme$ID[x] ]
  route.length <- pct$length[pct$ID == pct.scheme$ID[x]]

  route.osmids <- unique(pct2osm[[route.pct.id]])
  route.osmids <- route.osmids[route.osmids %in% scheme.osm_ids]
  route.osm <- osm[route.osmids,]
  route.osm <- as.data.frame(route.osm)
  #qtm(route.osm) +
  #  qtm(pct.scheme[x,], lines.col = "blue")
  route.osm <- route.osm[,c("osm_id","highway","cycleway","maxspeed","Recommended","length")]

  # summarise infrastrucutre before
  route.before <- route.osm[,c("highway","maxspeed","cycleway","length")]
  route.before <- group_by(route.before, highway, maxspeed, cycleway)
  route.before <- summarise(route.before, length = sum(length))
  route.before$combined <- paste0(route.before$highway,route.before$maxspeed,"_",route.before$cycleway)
  route.before <- route.before[,c("combined","length")]
  names(route.before) <- c("combined","lengthBefore")
  route.before$combined[route.before$combined %in% c("cycleway20_N","cycleway30_N","cycleway40_N","cycleway20_I","cycleway30_I","cycleway40_I")] <- "cycleway"
  route.before$combined[route.before$combined %in% c("path20_N","path30_N","path40_N","path20_I","path30_I","path40_I")] <- "path"

  # summarise the infrastrucutre after
  route.osm$cycleway.after <- ifelse(route.osm$Recommended == "None","N","I")
  route.osm$cycleway.after <- ifelse(route.osm$highway == "cycleway","I",route.osm$cycleway.after)

  route.after <- route.osm[,c("highway","maxspeed","cycleway.after","length")]
  route.after <- group_by(route.after, highway, maxspeed, cycleway.after)
  route.after <- summarise(route.after, length = sum(length))
  route.after$combined <- paste0(route.after$highway,route.after$maxspeed,"_",route.after$cycleway.after)
  route.after <- route.after[,c("combined","length")]
  names(route.after) <- c("combined","lengthAfter")
  route.after$combined[route.after$combined %in% c("cycleway20_N","cycleway30_N","cycleway40_N","cycleway20_I","cycleway30_I","cycleway40_I")] <- "cycleway"
  route.after$combined[route.after$combined %in% c("path20_N","path30_N","path40_N","path20_I","path30_I","path40_I")] <- "path"


  #put results togther
  route.change <- data.frame(type = modelvars, stringsAsFactors = F)
  route.change <- left_join(route.change, route.before, by = c("type" = "combined"))
  route.change <- left_join(route.change, route.after, by = c("type" = "combined"))

  #remove NAs
  route.change$lengthAfter[is.na(route.change$lengthAfter)] <- 0
  route.change$lengthBefore[is.na(route.change$lengthBefore)] <- 0
  route.change$change <- (route.change$lengthAfter - route.change$lengthBefore) / route.length

  #Pivot and prep for export
  route.change.names <- route.change$type
  route.change <- as.data.frame(t(route.change$change))
  names(route.change) <- paste0("C", route.change.names)
  route.change$id <- as.character(pct$ID[route.pct.id])

  return(route.change)

}




#List folders
#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo
#regions <- "Bristol"

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes.Rds"))){
    #Check if Uptake values exist
    if(file.exists(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/pct-up.Rds")) & skip){
      message(paste0("Uptake numbers already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting uptake values for ",regions[b]," at ",Sys.time()))

      #Get file
      osm <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds"))
      model <- readRDS("../cyipt/input-data/m4.Rds")


      # Get PCT Data
      pct <- readRDS(paste0("../cyipt-securedata/pct-regions/",regions[b],".Rds"))
      pct2osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct2osm.Rds"))
      osm2pct <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm2pct.Rds"))

      #simplify the speeds
      osm$maxspeed[osm$maxspeed <= 20] <- 20
      osm$maxspeed[osm$maxspeed >= 40] <- 40
      osm$maxspeed[osm$maxspeed < 40 & osm$maxspeed > 20] <- 30

      #summarise highway
      osm$highway <- sub("_link","",osm$highway)
      osm$highway[osm$highway %in% c("track", "pedestrian","steps","bridleway","byway", "footway")] <- "path"
      osm$highway[osm$highway %in% c("unclassified", "service","living_street","road","byway", "Other","bus_guideway","BOAT")] <- "other"


      #simplify infrastrucutre before
      osm$cycleway <- NA
      for(i in 1:nrow(osm)){
        left <- osm$cycleway.left[i]
        right <- osm$cycleway.right[i]

        if(left == right ){
          result <- left
        }else if(left == "no"){
          result <- right
        }else if(right == "no"){
          result <- left
        }else if(right %in% c("share_busway","lane") & left %in% c("lane","track") ){
          result <- left
        }else if(left %in% c("share_busway","lane") & right %in% c("lane","track") ){
          result <- right
        }else{
          message(paste0("Unusual case for row ",i," left = ",left," right = ",right))
          result <- left
        }

        if(result %in% c("lane","track")){
          result <- "I"
        }else{
          result <- "N"
        }

        osm$cycleway[i] <- result
      }

      osm$cycleway[osm$highway == "path"] <- "N"
      osm$cycleway[osm$highway == "cycleway"] <- "I"


      #get the list of scheme_nos
      scheme_nos <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes.Rds"))

      if(all(class(scheme_nos) == c("sf","data.frame"))){
        scheme_nos <- unique(scheme_nos$group_id)
        scheme_nos <- scheme_nos[!is.na(scheme_nos)]
        scheme_nos <- scheme_nos[order(scheme_nos)]


        osm$group_id[is.na(osm$group_id)] <- 0 # repalce NAs with 0 scheme number

        # Loop over schemes
        uptake.list <- list()
        uptake.route.list <- list()

        for(j in scheme_nos){

          #Get the roads in the schemes
          scheme.osm_ids <- osm$id[osm$group_id == j] # get the osm ids for this scheme
          scheme.pct_ids <- unique(unlist(osm2pct[scheme.osm_ids])) # get the pct ids for this scheme

          pct.scheme <- pct[scheme.pct_ids,]

          #For each route get the length of on road and off road infa
          infrachange <- lapply(1:nrow(pct.scheme), get.infrachange)
          infrachange <- bind_rows(infrachange)

          pct.scheme <- left_join(pct.scheme, infrachange, by = c("ID" = "id"))

          # New Route CHange Method

          #prep matrix for xgboost
          pct.scheme.mat <- as.data.frame(pct.scheme[,c(paste0("C", modelvars),"length","av_incline")])
          pct.scheme.mat$geometry <- NULL
          pct.scheme.mat$rf_avslope_perc <- pct.scheme.mat$av_incline
          pct.scheme.mat$av_incline <- NULL
          pct.scheme.mat <- as.matrix(pct.scheme.mat)

          pct.scheme$perincrease <- round(predict(object = model, pct.scheme.mat),3)
          pct.scheme$uptake <- pct.scheme$perincrease / 100 * pct.scheme$all_16p



          foo <- as.data.frame(pct.scheme[,c("ID","pct.census","all_16p","perincrease","uptake")])
          foo$geometry <- NULL

          uptake <- data.frame(scheme = j, census = sum(pct.scheme$pct.census), model.future = round(sum(pct.scheme$pct.census) + sum(pct.scheme$uptake),0))

          pct.scheme$schemeID <- j


          uptake.list[[j]] <- uptake

          pct.scheme <- as.data.frame(pct.scheme)
          pct.scheme$geometry <- NULL
          pct.scheme <- pct.scheme[,c("ID","schemeID","perincrease","uptake","Ccycleway",
                                      "Cpath","Cother20_N","Cother20_I","Cother30_N","Cother30_I",
                                      "Cother40_N","Cprimary20_N","Cprimary20_I","Cprimary30_N","Cprimary30_I",
                                      "Cprimary40_N","Cprimary40_I","Cresidential20_N","Cresidential20_I","Cresidential30_N",
                                      "Cresidential30_I","Cresidential40_N","Cresidential40_I","Csecondary20_N","Csecondary20_I",
                                       "Csecondary30_N","Csecondary30_I","Csecondary40_N","Csecondary40_I","Ctertiary20_N",
                                       "Ctertiary20_I","Ctertiary30_N","Ctertiary30_I","Ctertiary40_N","Ctertiary40_I",
                                      "Ctrunk20_N","Ctrunk30_N","Ctrunk30_I","Ctrunk40_N","Ctrunk40_I")]

          uptake.route.list[[j]] <- pct.scheme


          rm(pct.scheme, pct.scheme.mat, uptake, scheme.osm_ids, scheme.pct_ids)
          #rm(pct.scheme, pct.scheme.mat, uptake, cor, scheme.osm_ids, scheme.pct_ids)

          message(paste0("Done scheme ",j," at ",Sys.time()))

        }

        uptake.fin <- bind_rows(uptake.list)

        uptake.fin$change <- uptake.fin$model.future - uptake.fin$census
        uptake.fin$per <- round(uptake.fin$change / uptake.fin$census * 100, 2)

        uptake.route <- bind_rows(uptake.route.list)


        saveRDS(uptake.fin,paste0("../cyipt-bigdata/osm-recc/",regions[b],"/scheme-uptake.Rds"))
        saveRDS(uptake.route,paste0("../cyipt-bigdata/osm-recc/",regions[b],"/route-uptake.Rds"))

        rm(osm,model, osm2pct, pct2osm, scheme_nos)
      }else{
        message(paste0("No schemes for ",regions[b]))
      }



    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)



#qtm(osm[osm$group_id == 92,])

















# Test of calcualting uptake for each scenario in bristol

# set up


















