#Gets PCT Values for the road segments

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

#Functions
recc.infra <- function(c){
  not_road <- c("bridleway","construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track")
  osm.sub <- osm[c,]

  #On road or off road
  if(osm.sub$highway[1] %in% not_road){
    #Off Road
    rules.sub <- rules.offroad[rules.offroad$pctmin <= osm.sub$pct.census[1] &
                                  rules.offroad$pctmax > osm.sub$pct.census[1]
                                ,]
    }else{
    #On Road
    rules.sub <- rules.onroad[rules.onroad$speedmin < osm.sub$maxspeed[1] &
                                  rules.onroad$speedmax >= osm.sub$maxspeed[1] & #Nb equals different for speed as speedlimits are usually at maximum end
                                  rules.onroad$pctmin <= osm.sub$pct.census[1] &
                                  rules.onroad$pctmax > osm.sub$pct.census[1] &
                                  rules.onroad$AADTmin <= osm.sub$aadt.temp[1] &
                                  rules.onroad$AADTmax > osm.sub$aadt.temp[1]
                                ,]
    }

    if(nrow(rules.sub) != 1){
      message(paste0("Error: Not valid rules for line ",c))
      stop()
    }

    #Remove unneded columns, add on id value
    rules.sub <- rules.sub[,c("CycleRouteProvision","DesWidth","MinWidth","DesSeparation","MinSeparation")]
    names(rules.sub) <- c("Recommended","DesWidth","MinWidth","DesSeparation","MinSeparation")
    rules.sub$id <- osm.sub$id
    return(rules.sub)
}

get.costs <- function(d){
  osm.sub <- osm[d,]

  #Get Costs
  costs.sub <- costs[costs$Existing == osm.sub$Existing[1] & costs$Recommended == osm.sub$Recommended[1],]

  #Check for errors
  if(nrow(costs.sub) != 1){
    message(paste0("Error: Not valid costs for line ",d))
    stop()
  }

  costs.sub$id <- osm.sub$id
  costs.sub <- costs.sub[,c("id","Change","costperm")]
  return(costs.sub)
}

#Creat Polygons Around each scheme
schemepoly <- function(a){
  sub <- osm_sub[osm_sub$group_id == a,]
  sub <- sub$geometry
  buf <- st_buffer(sub,10)
  buf <- st_union(buf)
  buf <- st_simplify(buf, preserveTopology = FALSE, dTolerance = 0.5)
  return(buf)
}


#Group lines into schemes
groupinfra <- function(type, grp_start, buff_dists){
  sub <- osm[osm$Recommended == type & (osm$Change == "upgrade" | osm$Change == "upgrade (one side)"),]
  sub <- sub[,c("id")]
  if(nrow(sub) > 1){
    buff <- st_buffer(sub, buff_dists)
    #Find Instersections
    inter <- st_intersects(buff,buff)
    edges <- do.call(rbind, lapply(inter, function(x) {
      if (length(x) > 1) cbind(head(x, -1), tail(x, -1)) else NULL
    }))

    if(!is.null(edges)){
      #Find Groups
      g <- graph.data.frame(edges, directed=FALSE)
      g <- split(V(g)$name, clusters(g)$membership)
      grps <- list()
      for(a in 1:length(g)){
        grps[[a]] <- as.numeric(unlist(g[a]))
      }
      #Assing Groups
      for(b in 1:nrow(sub)){
        res <- which(sapply(grps,`%in%`, x = b))
        if(length(res) == 0){
          sub$group_id[b] <- NA
        }else{
          sub$group_id[b] <- res + grp_start
        }

      }
      sub <- as.data.frame(sub)
      sub <- sub[,c("id","group_id")]
    }else{
      #None of the infra interacts so add each to its own group
      sub <- as.data.frame(sub)
      sub$group_id <- (grp_start + 1):(grp_start + nrow(sub))
      sub <- sub[,c("id","group_id")]
    }

  }else if(nrow(sub) == 1){
    #Only one type so no grouping to be done
    sub <- as.data.frame(sub)
    sub$group_id <- grp_start + 1
  }else{
    #No road of that type
    sub <- NA
  }
  return(sub)
}


#List folders
#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo

rules.onroad <- read.csv("../cyipt/input-data/InfraSelectionRules_OnRoad.csv", stringsAsFactors = FALSE)
rules.offroad <- read.csv("../cyipt/input-data/InfraSelectionRules_OffRoad.csv", stringsAsFactors = FALSE)
costs <- read.csv("../cyipt/input-data/Costs.csv", stringsAsFactors = FALSE)

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
    #Check if PCT values exist in the file
    if(all(c("group_id","Existing","Recommended","DesWidth","MinWidth","DesSeparation","MinSeparation","Change","costperm","length","costTotal") %in% names(osm)) & skip){
      message(paste0("infrastructure types values already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting infrastructure types values for ",regions[b]," at ",Sys.time()))

      #If overwriting remove old data
      col.to.keep <- names(osm)[!names(osm) %in% c("group_id","Existing","Recommended","DesWidth","MinWidth","DesSeparation","MinSeparation","Change","costperm","length","costTotal")]
      osm <- osm[,col.to.keep]
      rm(col.to.keep)

      ##############################################
      #Create temp aadt values with no NAs
      osm$aadt.temp <- osm$aadt
      osm$aadt.temp[is.na(osm$aadt.temp)] <- 0

      ###########################################################################################################
      #Step 1: Compare Against Rules Table
      res <- lapply(1:nrow(osm),recc.infra)
      res <- do.call("rbind", res)

      #join in resutls
      osm <- left_join(osm, res, by = c("id" = "id"))

      #remove temp aadt
      osm$aadt.temp <- NULL
      rm(res)

      ######################################################################################
      #Summary Existing Infra

      osm$Existing <- paste0(osm$roadtype," ",osm$lanes.psv.forward," ",osm$cycleway.left," ",osm$cycleway.right," ",osm$lanes.psv.backward)

      #For testing only

      summary <- as.data.frame(osm)
      summary <- summary[,c("Existing","Recommended")]
      summary <- unique(summary)
      costs.summary <- costs[,c("Existing","Recommended")]
      #write.csv(summary, paste0("../cyipt-bigdata/osm-prep/",regions[b],"/RoadCombis.csv"), row.names = F)
      compar  <- function(a1,a2)
      {
        a1.vec <- apply(a1, 1, paste, collapse = "")
        a2.vec <- apply(a2, 1, paste, collapse = "")
        a1.without.a2.rows <- a1[!a1.vec %in% a2.vec,]
        return(a1.without.a2.rows)
      }
      comp <- compar(summary,costs.summary)




      ###########################################################################
      #Step 3: Costs
      res2 <- lapply(1:nrow(osm),get.costs)
      res2 <- do.call("rbind", res2)

      #join in resutls
      osm <- left_join(osm, res2, by = c("id" = "id"))
      rm(res2)

      #Step 7: Find lenghts and total costs
      osm$length <- as.numeric(st_length(osm))
      osm$costTotal <- as.integer(osm$costperm * osm$length)

      #############################################################################
      # Step 4: Group into schemes

      #Add Groups Column
      osm$group_id <- NA

      if(sum(osm$costTotal) != 0){
        #At least one piece of infrastructure to group existis
        #Do each type
        result <- groupinfra("Segregated Cycle Track", 0, 10)
        if(class(result) == "data.frame"){
          for(c in 1:nrow(result)){
            osm$group_id[osm$id == result$id[c]] <- result$group_id[c]
          }
        }
        rm(result)
        maxgrp <- if(max(osm$group_id, na.rm = T) == -Inf){0}else{max(osm$group_id, na.rm = T)}
        result <- groupinfra("Stepped Cycle Tracks", maxgrp, 10)
        if(class(result) == "data.frame"){
          for(c in 1:nrow(result)){
            osm$group_id[osm$id == result$id[c]] <- result$group_id[c]
          }
        }
        rm(result)
        maxgrp <- if(max(osm$group_id, na.rm = T) == -Inf){0}else{max(osm$group_id, na.rm = T)}
        result <- groupinfra("Cycle Lanes with light segregation", maxgrp, 10)
        if(class(result) == "data.frame"){
          for(c in 1:nrow(result)){
            osm$group_id[osm$id == result$id[c]] <- result$group_id[c]
          }
        }
        rm(result)
        maxgrp <- if(max(osm$group_id, na.rm = T) == -Inf){0}else{max(osm$group_id, na.rm = T)}
        result <- groupinfra("Cycle Lanes", maxgrp, 100)
        if(class(result) == "data.frame"){
          for(c in 1:nrow(result)){
            osm$group_id[osm$id == result$id[c]] <- result$group_id[c]
          }
        }
        rm(result)
        maxgrp <- if(max(osm$group_id, na.rm = T) == -Inf){0}else{max(osm$group_id, na.rm = T)}
        result <- groupinfra("Cycle Street", maxgrp, 500)
        if(class(result) == "data.frame"){
          for(c in 1:nrow(result)){
            osm$group_id[osm$id == result$id[c]] <- result$group_id[c]
          }
        }
        rm(result)
        maxgrp <- if(max(osm$group_id, na.rm = T) == -Inf){0}else{max(osm$group_id, na.rm = T)}
        result <- groupinfra("Cycle Lane on Path", maxgrp, 50)
        if(class(result) == "data.frame"){
          for(c in 1:nrow(result)){
            osm$group_id[osm$id == result$id[c]] <- result$group_id[c]
          }
        }
        rm(result)
        maxgrp <- if(max(osm$group_id, na.rm = T) == -Inf){0}else{max(osm$group_id, na.rm = T)}
        result <- groupinfra("Segregated Cycle Track on Path", maxgrp, 50)
        if(class(result) == "data.frame"){
          for(c in 1:nrow(result)){
            osm$group_id[osm$id == result$id[c]] <- result$group_id[c]
          }
        }
        rm(result)
        print(paste0("There are ",length(unique(osm$group_id))," groups"))
        print(paste0(nrow(osm[is.na(osm$group_id) & (osm$Change == "upgrade" | osm$Change == "upgrade (one side)"),])," of ",nrow(osm[osm$Change == "upgrade" | osm$Change == "upgrade (one side)",])," lines were not classified"))

        #Classify up the remaining lines in a rag bag category
        osm$group_id[is.na(osm$group_id) & (osm$Change == "upgrade" | osm$Change == "upgrade (one side)")] <- (max(osm$group_id, na.rm = T) + 1)

        #Make Polygons around schemes
        osm_sub <- osm[!(is.na(osm$group_id)),]
        l <- sapply(1:max(osm_sub$group_id, na.rm = T),schemepoly)
        schemes <- data.frame(group = 1:max(osm_sub$group_id, na.rm = T),
                              type = NA,
                              length = NA,
                              cost = NA,
                              geometry = NA)
        schemes$geometry <- st_sfc(l)
        schemes <- st_as_sf(schemes)
        st_crs(schemes) <- 27700
        schemes <- st_transform(schemes, 27700)

        for(f in 1:nrow(schemes)){
          id <- schemes$group[f]
          schemes$type[f] <- osm_sub$Recommended[osm_sub$group_id == id][1]
          schemes$length[f] <- sum(osm_sub$length[osm_sub$group_id == id])
          schemes$cost[f] <- sum(osm_sub$costTotal[osm_sub$group_id == id])
        }
        #qtm(schemes, fill = "type")
        rm(f,c,osm_sub)
      }else{
        message("No infrastructure upgrades were found")
        schemes <- 0
      }


      #Save results
      if(overwrite){
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
        saveRDS(schemes,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes.Rds"))
      }else{
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines-reccinfra.Rds"))
        saveRDS(schemes,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes.Rds"))
      }
      rm(osm,schemes)

    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions,rules.offroad,rules.onroad,costs)
