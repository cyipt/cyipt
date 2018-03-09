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


# Variaibles in the model

modelvars <- c("cycleway","path",
               "main20_N","main20_I","main30_N","main30_I","main40_N","main40_I",
               "residential20_N", "residential20_I", "residential30_N", "residential30_I", "residential40_N", "residential40_I",
               "trunk20_N","trunk20_I","trunk30_N","trunk30_I","trunk40_N","trunk40_I",
               "length","r_avslope_perc","percycle01")



# infra change
get.infrachange <- function(x, pct.scheme, j, scheme.osm_ids){

  message(paste0(" Got inside get.infrachange for ",j," ",x))
  route.pct.id <- (1:nrow(pct))[pct$ID == pct.scheme$ID[x] ]
  route.length <- pct$length[pct$ID == pct.scheme$ID[x]]

  route.osmids <- unique(pct2osm[[route.pct.id]])
  route.osmids <- route.osmids[route.osmids %in% scheme.osm_ids]
  route.osm <- osm[route.osmids,]
  #qtm(route.osm) +
  #  qtm(pct.scheme[x,], lines.col = "blue")

  #route.osm <- as.data.frame(route.osm)
  #route.osm <- route.osm[,c("id","highway","cycleway","maxspeed","Recommended","length")]

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
  #check changes are part of the scheme
  route.osm$Recommended <- ifelse(route.osm$id %in% scheme.osm_ids,route.osm$Recommended,"None")

  #Summarise as infrastruture or no infrastrucutre for the uptake model
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
  names(route.change) <- paste0("F", route.change.names)
  route.change$id <- as.character(pct$ID[route.pct.id])

  #message(paste0("done ",x))
  return(route.change)

}


evaluate.schemes <- function(j){

  #Get the roads in the schemes
  scheme.osm_ids <- osm$id[osm$group_id == j] # get the osm ids for this scheme
  scheme.pct_ids <- unique(unlist(osm2pct[scheme.osm_ids])) # get the pct ids for this scheme

  pct.scheme <- pct[scheme.pct_ids,]

  #message(paste0(" Got to lapply for ",j," variaibles are ",paste(ls(), collapse = " ")," objects are ",paste(objects(), collapse = " ")))
  #message(paste0("there are ",nrow(pct.scheme)," rows in pct.scheme beffore the lappy in",j))

  #For each route get the length of on road and off road infa
  infrachange <- lapply(1:nrow(pct.scheme), get.infrachange, pct.scheme = pct.scheme, j = j, scheme.osm_ids = scheme.osm_ids)
  infrachange <- bind_rows(infrachange)

  pct.scheme <- left_join(pct.scheme, infrachange, by = c("ID" = "id"))

  # New Route CHange Method

  #prep matrix for xgboost
  pct.scheme.mat <- as.data.frame(pct.scheme[,c(paste0("F", modelvars),"length","av_incline","percycle01")])
  pct.scheme.mat$geometry <- NULL
  pct.scheme.mat$rf_avslope_perc <- pct.scheme.mat$av_incline
  pct.scheme.mat$av_incline <- NULL
  pct.scheme.mat <- as.matrix(pct.scheme.mat)

  pct.scheme$percycleAfter <- round(predict(object = model, pct.scheme.mat),3)
  pct.scheme$cycleAfter <- pct.scheme$percycleAfter * pct.scheme$all_16p

  uptake <- data.frame(scheme = j, census = sum(pct.scheme$pct.census), model.future = round(sum(pct.scheme$cycleAfter),0))

  pct.scheme$schemeID <- j



  pct.scheme <- as.data.frame(pct.scheme)
  pct.scheme$geometry <- NULL
  pct.scheme <- pct.scheme[,c("ID","schemeID","percycleAfter","cycleAfter","Fcycleway","Fpath",
                              "Fmain20_N","Fmain20_I","Fmain30_N","Fmain30_I","Fmain40_N","Fmain40_I",
                              "Fresidential20_N","Fresidential20_I","Fresidential30_N","Fresidential30_I","Fresidential40_N","Fresidential40_I",
                              "Ftrunk20_N","Ftrunk30_N","Ftrunk30_I","Ftrunk40_N","Ftrunk40_I")]


  result <- list()
  result[[1]] <- uptake
  result[[2]] <- pct.scheme

  return(result)

}





#List folders
regions <- regions.todo


for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes.Rds"))){
    #Check if Uptake values exist
    if(file.exists(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/pct-up.Rds")) & skip){
      message(paste0("Uptake numbers already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting uptake values for ",regions[b]," at ",Sys.time()))

      #Get file
      osm <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds"))
      model <- readRDS("../cyipt/input-data/m5.Rds")


      # Get PCT Data
      pct <- readRDS(paste0("../cyipt-securedata/pct-regions/",regions[b],".Rds"))
      pct$percycle01 <- pct$pct.census / pct$all_16p #call 01 for model but actually 2011

      pct2osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct2osm.Rds"))
      osm2pct <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm2pct.Rds"))

      #simplify the speeds
      osm$maxspeed[osm$maxspeed <= 20] <- 20
      osm$maxspeed[osm$maxspeed >= 40] <- 40
      osm$maxspeed[osm$maxspeed < 40 & osm$maxspeed > 20] <- 30

      #summarise highway
      osm$highway <- sub("_link","",osm$highway)
      osm$highway[osm$highway %in% c("track", "pedestrian","steps","bridleway","byway", "footway")] <- "path"
      osm$highway[osm$highway %in% c("primary","secondary","tertiary","unclassified", "service","living_street","road","byway", "Other","bus_guideway","BOAT")] <- "main"
      osm$highway[osm$highway %in% c("motorway", "trunk")] <- "trunk"

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

      #discard unneded data in preparation for paralleisation
      # reduced memeory use and time copying data to each cluster
      osm <- as.data.frame(osm)
      osm <- osm[,c("id","highway","cycleway","maxspeed","Recommended","length","group_id")]

      pct <- as.data.frame(pct)
      pct <- pct[,c("ID","length","av_incline","all_16p","pct.census","percycle01")]

      #get the list of scheme_nos
      scheme_nos <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes.Rds"))



      if(all(c("sf","data.frame") %in% class(scheme_nos))){
        scheme_nos <- unique(scheme_nos$group_id)
        scheme_nos <- scheme_nos[!is.na(scheme_nos)]
        scheme_nos <- scheme_nos[order(scheme_nos)]


        osm$group_id[is.na(osm$group_id)] <- 0 # repalce NAs with 0 scheme number

        # Loop over schemes


        ##########################################################
        #Parallel

        start <- Sys.time()
        fun <- function(cl){
          parLapply(cl, scheme_nos, evaluate.schemes)
        }
        cl <- makeCluster(ncores, outfile = paste0("parlog-",Sys.Date(),".txt")) #make clusert and set number of cores
        clusterEvalQ(cl, {library(dplyr) })
        #clusterExport(cl=cl, varlist=c("pct","osm","pct2osm","osm2pct","modelvars"), envir=environment())
        clusterExport(cl=cl, varlist=c("pct","osm","pct2osm","osm2pct","modelvars","model") )
        clusterExport(cl=cl, c('get.infrachange') )
        respar <- fun(cl)
        stopCluster(cl)
        end <- Sys.time()
        if(verbose){message(paste0("Did ",length(scheme_nos)," schemes in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))}
        rm(cl,start,end,fun)
        ##################################################

        uptake <- list()
        uptake.route <- list()

        for(i in 1:length(respar)){
          uptake[[i]] <- respar[[i]][[1]]
          uptake.route[[i]] <- respar[[i]][[2]]
        }

        uptake <- bind_rows(uptake)
        uptake.route <- bind_rows(uptake.route)

        uptake$change <- uptake$model.future - uptake$census
        uptake$per <- round(uptake$change / uptake$census * 100, 2)


        saveRDS(uptake,paste0("../cyipt-bigdata/osm-recc/",regions[b],"/scheme-uptake.Rds"))
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


















