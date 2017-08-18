#Gets PCT Values for the road segments

############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################

library(sf)
library(dplyr)
library(parallel)


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



#List folders
#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo

rules.onroad <- read.csv("../cyipt/input-data/InfraSelectionRules_OnRoad.csv", stringsAsFactors = FALSE)
rules.offroad <- read.csv("../cyipt/input-data/InfraSelectionRules_OffRoad.csv", stringsAsFactors = FALSE)
costs <- read.csv("../cyipt/input-data/costs3.csv", stringsAsFactors = FALSE)

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
    #Check if PCT values exist in the file
    if(all(c("FILL ME IN") %in% names(osm)) & skip){
      message(paste0("infrastructure types values already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting infrastructure types values for ",regions[b]," at ",Sys.time()))

      #If overwriting remove old data
      col.to.keep <- names(osm)[!names(osm) %in% c("FILL ME IN")]
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

      ######################################################################################
      #Summary Existing Infra

      osm$Existing <- paste0(osm$roadtype," ",osm$lanes.psv.forward," ",osm$cycleway.left," ",osm$cycleway.right," ",osm$lanes.psv.backward)

      #For testing only
      #summary <- as.data.frame(osm)
      #summary <- summary[,c("Existing","Recommended")]
      #summary <- unique(summary)
      #write.csv(summary, paste0("../cyipt-bigdata/osm-prep/",regions[b],"/RoadCombis.csv"))


      ###########################################################################
      #Step 3: Costs




      #Save results
      if(overwrite){
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
      }else{
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines-reccinfra.Rds"))
      }
      rm(osm,pct.all,respar)



    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)




###########################################################################
#Step 3: Remove NA values
osm$aadt[is.na(osm$aadt)] <- 0


osm$roadtype2 <- paste0(osm$roadtype," ",osm$leftside," ",osm$rightside)





#Step 6: Compare existing and proposed
osm$action <- paste0(osm$roadtype2," -> ",osm$infra_score)
costs <- read.csv("../cyipt/input-data/costs3.csv", stringsAsFactors = FALSE)
#costs$id <- NULL

#stop()

osm <- left_join(osm,costs, by = c("action" = "type"))
#Step 7: Find lenghts and total costs
osm$length <- as.numeric(st_length(osm))
osm$cost.total <- as.integer(osm$cost.per.m * osm$length)

#test <- osm[osm$infra_score != "None",]




tm_shape(osm[osm$change != "no need",]) +
  tm_lines(col = "change", lwd = 5, alpha = 1,
           title.col = "Change",
           popup.vars = c("infra_score","roadtype2", "cost.total"))




saveRDS(osm,"../example-data/cambridge/results/osm-select-infra.Rds")
#plot(test[test$infra_score == "None",])
