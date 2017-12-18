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
  costs.sub <- costs[costs$Existing == osm.sub$Existing[1] & costs$Recommended == osm.sub$Recommended[1] & costs$onewaysummary == osm.sub$onewaysummary[1],]

  #Check for errors
  if(nrow(costs.sub) != 1){
    message(paste0("Error: Not valid costs for line ",d," ",nrow(costs.sub)))
    stop()
  }

  costs.sub$id <- osm.sub$id
  costs.sub <- costs.sub[,c("id","Change","costperm")]
  return(costs.sub)
}

#List folders
#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo

rules.onroad <- read.csv("../cyipt/input-data/InfraSelectionRules_OnRoad.csv", stringsAsFactors = FALSE)
rules.offroad <- read.csv("../cyipt/input-data/InfraSelectionRules_OffRoad.csv", stringsAsFactors = FALSE)
costs <- read.csv("../cyipt/input-data/costs2.csv", stringsAsFactors = FALSE)

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
    #Check if PCT values exist in the file
    if(all(c("Existing","Recommended","DesWidth","MinWidth","DesSeparation","MinSeparation","Change","costperm","length","costTotal") %in% names(osm)) & skip){
      message(paste0("infrastructure types values already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting infrastructure types values for ",regions[b]," at ",Sys.time()))

      #If overwriting remove old data
      col.to.keep <- names(osm)[!names(osm) %in% c("Existing","Recommended","DesWidth","MinWidth","DesSeparation","MinSeparation","Change","costperm","length","costTotal")]
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
      summary <- summary[,c("Existing","Recommended","onewaysummary")]
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


      #Save results
      if(overwrite){
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
      }else{
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines-reccinfra.Rds"))
      }
      rm(osm)

    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions,rules.offroad,rules.onroad,costs)
