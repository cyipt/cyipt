##########################
# rewrite for performance


############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################

library(sf)
library(dplyr)
library(tmap)
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
      model <- readRDS("../cyipt/input-data/m.Rds")


      # Get PCT Data
      pct <- readRDS(paste0("../cyipt-securedata/pct-regions/",regions[b],".Rds"))
      pct2osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct2osm.Rds"))
      osm2pct <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm2pct.Rds"))

      #Get Lines
      lines <- readRDS("../cyipt-securedata/pct-lines-all.Rds")
      lines <- lines[lines$ID %in% pct$ID,]
      lines <- st_transform(lines, 27700)

      #calc busyness score
      osm$quietness <- as.integer(osm$quietness)
      osm$busyBefore <- osm$length / (osm$quietness/100)

      #Update Busyness score based on Reccomended infra
      # simple test make quietness always equal to 100

      #get the list of scheme_nos
      scheme_nos <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes.Rds"))
      scheme_nos <- unique(scheme_nos$group_id)
      scheme_nos <- scheme_nos[!is.na(scheme_nos)]
      scheme_nos <- scheme_nos[order(scheme_nos)]


      osm$group_id[is.na(osm$group_id)] <- 0 # repalce NAs with 0 scheme number

      # Loop over schemes
      uptake.list <- list()
      uptake.route.list <- list()

      for(j in scheme_nos){

        #Find the exposure to the scheme of each pct straigh line
        scheme.buff <- st_buffer(osm[osm$group_id == j,], 500, nQuadSegs = 8)
        scheme.buff <- scheme.buff %>% group_by(group_id) %>% summarise()


        # Find Exposed Length
        # St_length does not like mix of lines and mulilines to split, calc, recombine
        lines.scheme <- lines[scheme.buff,]
        lines.scheme <- st_intersection(lines.scheme,scheme.buff)

        lines.scheme.l <-  lines.scheme[st_geometry_type(lines.scheme) == "LINESTRING",]
        lines.scheme.ml <-  lines.scheme[st_geometry_type(lines.scheme) == "MULTILINESTRING",]

        # Check for Lines
        if(nrow(lines.scheme.l)>0){
          lines.scheme.l$exposeLengthLine <- as.numeric(st_length(lines.scheme.l))
          lines.scheme.l <- as.data.frame(lines.scheme.l)
          lines.scheme.l$geometry <- NULL
        }

        #Check for mulilines
        if(nrow(lines.scheme.ml)>0){
          lines.scheme.ml$exposeLengthLine <- as.numeric(st_length(lines.scheme.ml))
          lines.scheme.ml <- as.data.frame(lines.scheme.ml)
          lines.scheme.ml$geometry <- NULL
        }

        #Recombine
        if(nrow(lines.scheme.l) > 0 & nrow(lines.scheme.ml) > 0){
          lines.scheme <- bind_rows(lines.scheme.l, lines.scheme.ml)
        }else if(nrow(lines.scheme.l) == 0 & nrow(lines.scheme.ml) > 0){
          lines.scheme <- lines.scheme.ml
        }else if(nrow(lines.scheme.l) > 0 & nrow(lines.scheme.ml) == 0){
          lines.scheme <- lines.scheme.l
        }else{
          message(paste0(" Something has gone wrong with ",j))
        }

        #Calcualte Exposure
        lines.scheme$exposureLine <- lines.scheme$exposeLength / lines.scheme$length
        lines.scheme <- lines.scheme[,c("ID","exposeLengthLine","exposureLine")]
        rm(scheme.buff, lines.scheme.l, lines.scheme.ml)


        #Get the roads in the schemes
        scheme.osm_ids <- osm$id[osm$group_id == j] # get the osm ids for this scheme
        scheme.pct_ids <- unique(unlist(osm2pct[scheme.osm_ids])) # get the pct ids for this scheme
        pct.scheme <- pct[scheme.pct_ids,]
        pct.scheme <- left_join(pct.scheme, lines.scheme, by = c("ID" = "ID"))



        # New Exposure Method
        pct.scheme$exposureLine[is.na(pct.scheme$exposureLine)] <- 0
        pct.scheme.mat <- as.data.frame(pct.scheme[,c("length","exposureLine")])
        pct.scheme.mat$geometry <- NULL

        names(pct.scheme.mat) <- c("dist","exposure")

        pct.scheme$perincrease <- round(predict(object = model, pct.scheme.mat),3)
        pct.scheme$uptake <- ((pct.scheme$pct.census / pct.scheme$all_16p) + pct.scheme$perincrease) * pct.scheme$all_16p

        #foo <- as.data.frame(pct.scheme[,c("ID","pct.census","all_16p","perincrease","uptake")])
        #foo$geometry <- NULL

        uptake <- data.frame(scheme = j, census = sum(pct.scheme$pct.census), model.future = round(sum(pct.scheme$uptake),0))

        pct.scheme$schemeID <- j


        uptake.list[[j]] <- uptake

        pct.scheme <- as.data.frame(pct.scheme)
        pct.scheme$geometry <- NULL
        pct.scheme <- pct.scheme[,"ID","schemeID","exposeLengthLine","exposureLine","perincrease","uptake"]

        uptake.route.list[[j]] <- pct.scheme


        rm(pct.scheme, pct.scheme.mat, uptake, scheme.osm_ids, scheme.pct_ids)
        #rm(pct.scheme, pct.scheme.mat, uptake, cor, scheme.osm_ids, scheme.pct_ids)

        message(paste0("Done scheme ",j," at ",Sys.time()))

      }

      uptake.fin <- bind_rows(uptake.list)
      uptake.fin$change <- uptake.fin$model.future - uptake.fin$census
      uptake.fin$per <- round(uptake.fin$change / uptake.fin$census * 100, 2)

      uptake.route <- bind_rows(uptake.route.list)


      saveRDS(uptake.fin,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/scheme-uptake.Rds"))
      saveRDS(uptake.route,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/route-uptake.Rds"))

      rm(osm,model, osm2pct, pct2osm, scheme_nos)

    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)



#qtm(osm[osm$group_id == 92,])

















# Test of calcualting uptake for each scenario in bristol

# set up


















