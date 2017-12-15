# Costs Maker
library(dplyr)


regions <- list.dirs(path = "../cyipt-bigdata/osm-prep", full.names = FALSE) # Now get regions from the master file
regions <- regions[2:length(regions)]
regions.list <- list()

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
    message(paste0(Sys.time()," Processing ",regions[b]," with ",nrow(osm)," lines"))

    osm <- as.data.frame(osm)
    osm <- osm[,c("roadtype","onewaysummary","cycleway.left","lanes.psv.forward","lanes.forward","lanes.backward","lanes.psv.backward","cycleway.right","Existing","Recommended" )]



    regions.list[[b]] <- osm
    rm(osm)

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)

message(paste0(Sys.time()," Combining Regions into master file "))
osm.all <- bind_rows(regions.list)
rm(regions.list)
osm.all <- unique(osm.all)
rownames(osm.all) <- 1:nrow(osm.all)

osm.all$Change <- NA
osm.all$costperm <- NA

#Work out the kind of upgrade
for(a in 1:nrow(osm.all)){
  if(osm.all$Recommended[a] == "None"){
    #No Upgrade
    if(osm.all$cycleway.left[a] == "no" & osm.all$cycleway.right[a] == "no"){
      osm.all$Change[a] <- "no change"
      osm.all$costperm[a] <- 0
    }else{
      osm.all$Change[a] <- "downgrade"
      osm.all$costperm[a] <- 0
    }
  }else if(osm.all$Recommended[a] == "Cycle Lanes"){
    if(osm.all$cycleway.left[a] %in% c("lane","share_busway") & osm.all$cycleway.right[a] %in% c("lane","share_busway") & osm.all$onewaysummary[a] %in% c("Two Way","One Way - Two Way Cycling")){
      osm.all$Change[a] <- "no change"
      osm.all$costperm[a] <- 0
    }else if(osm.all$cycleway.left[a] %in% c("lane","share_busway") & osm.all$onewaysummary[a] %in% c("One Way")){
      osm.all$Change[a] <- "no change"
      osm.all$costperm[a] <- 0
    }else if(osm.all$cycleway.left[a] %in% c("lane","track","share_busway") & osm.all$cycleway.right[a] %in% c("lane","track","share_busway")){
      osm.all$Change[a] <- "downgrade"
      osm.all$costperm[a] <- 0
    }else if(osm.all$cycleway.left[a] %in% c("lane","track") & osm.all$onewaysummary[a] %in% c("One Way")){
      osm.all$Change[a] <- "downgrade"
      osm.all$costperm[a] <- 0
    }else if(osm.all$cycleway.left[a] == "no" & osm.all$cycleway.right[a] == "no" & osm.all$onewaysummary[a] %in% c("Two Way","One Way - Two Way Cycling")){
      osm.all$Change[a] <- "upgrade (both sides)"
      osm.all$costperm[a] <- 460
    }else if(osm.all$cycleway.left[a] == "no" | osm.all$cycleway.right[a] == "no" & osm.all$onewaysummary[a] %in% c("Two Way","One Way - Two Way Cycling")){
      osm.all$Change[a] <- "upgrade (one side)"
      osm.all$costperm[a] <- 180
    }else if(osm.all$cycleway.left[a] == "no" & osm.all$onewaysummary[a] %in% c("One Way")){
      osm.all$Change[a] <- "upgrade (one side)"
      osm.all$costperm[a] <- 180
    }
  }else if(osm.all$Recommended[a] == "Cycle Lanes with light segregation"){
    if(osm.all$cycleway.left[a] %in% c("track") & osm.all$cycleway.right[a] %in% c("track")){
      osm.all$Change[a] <- "downgrade"
      osm.all$costperm[a] <- 0
    }else if(osm.all$cycleway.left[a] %in% c("no","lane","share_busway") & osm.all$cycleway.right[a] %in% c("no","lane","share_busway") & osm.all$onewaysummary[a] %in% c("Two Way","One Way - Two Way Cycling")){
      osm.all$Change[a] <- "upgrade (both sides)"
      osm.all$costperm[a] <- 740
    }else if(osm.all$cycleway.left[a] %in% c("no","lane","share_busway") | osm.all$cycleway.right[a] %in% c("no","lane","share_busway") & osm.all$onewaysummary[a] %in% c("Two Way","One Way - Two Way Cycling")){
      osm.all$Change[a] <- "upgrade (one side)"
      osm.all$costperm[a] <- 370
    }else if(osm.all$cycleway.left[a] %in% c("no","lane","share_busway") & osm.all$onewaysummary[a] %in% c("One Way")){
      osm.all$Change[a] <- "upgrade (one side)"
      osm.all$costperm[a] <- 370
    }
  }else if(osm.all$Recommended[a] == "Stepped Cycle Tracks"){
    if(osm.all$cycleway.left[a] %in% c("track") & osm.all$cycleway.right[a] %in% c("track") & osm.all$onewaysummary[a] %in% c("Two Way","One Way - Two Way Cycling")){
      osm.all$Change[a] <- "no change"
      osm.all$costperm[a] <- 0
    }else if(osm.all$cycleway.left[a] %in% c("track") & osm.all$onewaysummary[a] %in% c("One Way")){
      osm.all$Change[a] <- "no change"
      osm.all$costperm[a] <- 0
    }else if(osm.all$cycleway.left[a] %in% c("no","lane","share_busway") & osm.all$cycleway.right[a] %in% c("no","lane","share_busway") & osm.all$onewaysummary[a] %in% c("Two Way","One Way - Two Way Cycling")){
      osm.all$Change[a] <- "upgrade (both sides)"
      osm.all$costperm[a] <- 960
    }else if(osm.all$cycleway.left[a] %in% c("no","lane","share_busway") | osm.all$cycleway.right[a] %in% c("no","lane","share_busway") & osm.all$onewaysummary[a] %in% c("Two Way","One Way - Two Way Cycling")){
      osm.all$Change[a] <- "upgrade (one side)"
      osm.all$costperm[a] <- 480
    }else if(osm.all$cycleway.left[a] %in% c("no","lane","share_busway") & osm.all$onewaysummary[a] %in% c("One Way")){
      osm.all$Change[a] <- "upgrade (one side)"
      osm.all$costperm[a] <- 480
    }
  }else if(osm.all$Recommended[a] == "Segregated Cycle Track"){
    if(osm.all$cycleway.left[a] %in% c("track") & osm.all$cycleway.right[a] %in% c("track") & osm.all$onewaysummary[a] %in% c("Two Way","One Way - Two Way Cycling")){
      osm.all$Change[a] <- "no change"
      osm.all$costperm[a] <- 0
    }else if(osm.all$cycleway.left[a] %in% c("track") & osm.all$onewaysummary[a] %in% c("One Way")){
      osm.all$Change[a] <- "no change"
      osm.all$costperm[a] <- 0
    }else if(osm.all$cycleway.left[a] %in% c("no","lane","share_busway") & osm.all$cycleway.right[a] %in% c("no","lane","share_busway") & osm.all$onewaysummary[a] %in% c("Two Way","One Way - Two Way Cycling")){
      osm.all$Change[a] <- "upgrade (both sides)"
      osm.all$costperm[a] <- 1450
    }else if(osm.all$cycleway.left[a] %in% c("no","lane","share_busway") | osm.all$cycleway.right[a] %in% c("no","lane","share_busway") & osm.all$onewaysummary[a] %in% c("Two Way","One Way - Two Way Cycling")){
      osm.all$Change[a] <- "upgrade (one side)"
      osm.all$costperm[a] <- 725
    }else if(osm.all$cycleway.left[a] %in% c("no","lane","share_busway") & osm.all$onewaysummary[a] %in% c("One Way")){
      osm.all$Change[a] <- "upgrade (one side)"
      osm.all$costperm[a] <- 725
    }
  }else if(osm.all$Recommended[a] == "Cycle Street"){
    if(osm.all$roadtype[a] %in% c("Living Street")){
      osm.all$Change[a] <- "no change"
      osm.all$costperm[a] <- 0
    }else if(osm.all$cycleway.left[a] %in% c("lane","track","share_busway") & osm.all$cycleway.right[a]  %in% c("lane","track","share_busway") & osm.all$onewaysummary[a] %in% c("Two Way","One Way - Two Way Cycling")){
      osm.all$Change[a] <- "downgrade"
      osm.all$costperm[a] <- 0
    }else if(osm.all$cycleway.left[a] %in% c("lane","track","share_busway") & osm.all$onewaysummary[a] %in% c("One Way")){
      osm.all$Change[a] <- "no change"
      osm.all$costperm[a] <- 0
    }else if(osm.all$cycleway.left[a] == "no" & osm.all$cycleway.right[a] == "no" & osm.all$onewaysummary[a] %in% c("Two Way","One Way - Two Way Cycling")){
      osm.all$Change[a] <- "upgrade (both sides)"
      osm.all$costperm[a] <- 15
    }else if(osm.all$cycleway.left[a] == "no" & osm.all$cycleway.right[a] == "no" | osm.all$onewaysummary[a] %in% c("Two Way","One Way - Two Way Cycling")){
      osm.all$Change[a] <- "upgrade (one side)"
      osm.all$costperm[a] <- 15
    }else if(osm.all$cycleway.left[a] == "no" & osm.all$onewaysummary[a] %in% c("One Way")){
      osm.all$Change[a] <- "upgrade (one side)"
      osm.all$costperm[a] <- 15
    }
  }else if(osm.all$Recommended[a] == "Cycle Lane on Path"){
    if(osm.all$roadtype[a] %in% c("Cycleway")){
      osm.all$Change[a] <- "no change"
      osm.all$costperm[a] <- 0
    }else if(osm.all$roadtype[a] %in% c("Path - Cycling Forbidden","Shared Path")){
      osm.all$Change[a] <- "upgrade"
      osm.all$costperm[a] <- 140
    }else{
      osm.all$Change[a] <- "downgrade"
      osm.all$costperm[a] <- 0
    }
  }else if(osm.all$Recommended[a] == "Segregated Cycle Track on Path"){
    if(osm.all$roadtype[a] %in% c("Segregated Cycleway")){
      osm.all$Change[a] <- "no change"
      osm.all$costperm[a] <- 0
    }else if(osm.all$roadtype[a] %in% c("Path - Cycling Forbidden","Shared Path","Cycleway")){
      osm.all$Change[a] <- "upgrade"
      osm.all$costperm[a] <- 190
    }else{
      osm.all$Change[a] <- "downgrade"
      osm.all$costperm[a] <- 0
    }
  }



  #End of Loop
}

osm.all <- osm.all[c("Existing","Recommended","onewaysummary","Change","costperm")]
osm.all <- unique(osm.all)
write.csv(osm.all,"../cyipt/input-data/costs2.csv", row.names = FALSE)
