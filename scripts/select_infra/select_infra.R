#Reccomends infrastructure for the road segments

#create directory
if(!dir.exists(paste0("../cyipt-bigdata/osm-recc"))){
  dir.create(paste0("../cyipt-bigdata/osm-recc"))
}

# Functions

# testing fucntion
compar  <- function(a1,a2)
{
  a1.vec <- apply(a1, 1, paste, collapse = "")
  a2.vec <- apply(a2, 1, paste, collapse = "")
  a1.without.a2.rows <- a1[!a1.vec %in% a2.vec,]
  return(a1.without.a2.rows)
}


# main functions

recc.infra <- function(i){
  not_road <- c("bridleway","construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track")
  osm.sub <- osm[i,]

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
      message(paste0("Error: Not valid rules for line ",i))
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

#Start of main code

regions <- regions.todo

rules.onroad <- read.csv("../cyipt/input-data/InfraSelectionRules_OnRoad.csv", stringsAsFactors = FALSE)
rules.offroad <- read.csv("../cyipt/input-data/InfraSelectionRules_OffRoad.csv", stringsAsFactors = FALSE)
costs <- read.csv("../cyipt/input-data/costs2.csv", stringsAsFactors = FALSE)

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))){

    #Check if Infra file has already been created
    if(file.exists(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds")) & skip){
      message(paste0("infrastructure types values already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting infrastructure types values for ",regions[b]," at ",Sys.time()))

      #Get file
      osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))

      #create dir
      dir.create(paste0("../cyipt-bigdata/osm-recc/",regions[b]))

      #If overwriting remove old data
      col.to.keep <- names(osm)[!names(osm) %in% c("Existing","Recommended","DesWidth","MinWidth","DesSeparation","MinSeparation","Change","costperm","length","costTotal")]
      osm <- osm[,col.to.keep]
      rm(col.to.keep)

      #Create temp aadt values with no NAs
      osm$aadt.temp <- osm$aadt
      busy.raodtypes <- c("motorway","motorway_link","primary","primary_link","secondary","secondary_link","tertiary","tertiary_link","trunk","trunk_link")
      # Main roads have voer 2500 AADT (i.e no cycle streets)
      # Otherwise 0 which has the same affect as an vlaue between 0 and 2500
      osm$aadt.temp[is.na(osm$aadt.temp) & !(osm$highway %in% busy.raodtypes)] <- 0
      osm$aadt.temp[is.na(osm$aadt.temp) & (osm$highway %in% busy.raodtypes)] <- 2501

      ###########################################################################################################
      #Step 1: Compare Against Rules Table

      ##########################################################
      #Parallel
      m = 1
      n = nrow(osm)
      start <- Sys.time()
      fun <- function(cl){
        parLapply(cl, m:n,recc.infra)
      }
      cl <- makeCluster(ncores) #make clusert and set number of cores
      clusterExport(cl=cl, varlist=c("rules.onroad", "rules.offroad","osm"))
      clusterExport(cl=cl, c('recc.infra') )
      clusterEvalQ(cl, {library(sf)})
      res <- fun(cl)
      stopCluster(cl)
      end <- Sys.time()
      if(verbose){message(paste0("Did ",n," lines in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))}
      ##########################################################

      res <- bind_rows(res)

      #join in resutls
      osm <- left_join(osm, res, by = c("id" = "id"))

      #remove temp aadt
      osm$aadt.temp <- NULL
      rm(res)

      ######################################################################################
      #Step 2: Summary Existing Infra

      osm$Existing <- paste0(osm$roadtype," ",osm$lanes.psv.forward," ",osm$cycleway.left," ",osm$cycleway.right," ",osm$lanes.psv.backward)

      ###########################################################################
      #Step 3: Costs
      res2 <- try(lapply(1:nrow(osm),get.costs))

      if(class(res2) == "try-error"){
        summary <- as.data.frame(osm)
        summary <- summary[,c("Existing","Recommended","onewaysummary")]
        summary <- unique(summary)
        costs.summary <- costs[,c("Existing","Recommended","onewaysummary")]
        comp <- compar(summary,costs.summary)
        rm(summary, costs.summary)
        print(comp)
        stop()
      }else{
        res2 <- bind_rows(res2)
      }

      # join in resutls
      osm <- left_join(osm, res2, by = c("id" = "id"))
      rm(res2)

      #Step 7: Find lenghts and total costs
      osm$length <- as.numeric(st_length(osm))
      osm$costTotal <- as.integer(osm$costperm * osm$length)

      #Save results
      if(overwrite){
        saveRDS(osm,paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds"))
      }else{
        saveRDS(osm,paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines-reccinfra.Rds"))
      }
      rm(osm)

    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions,rules.offroad,rules.onroad,costs)
