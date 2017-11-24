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



#List folders
#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes.Rds"))){
    #Get file
    scheme <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes.Rds"))
    #Check if PCT values exist in the file
    if(all(c("FILL ME IN") %in% names(osm)) & skip){
      message(paste0("Scheme Simplification already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Summarising Schemes for ",regions[b]," at ",Sys.time()))

      #Dump Unneded Data
      scheme <- scheme[,c("group_id","costTotal","Recommended")]

      #Summarise
      scheme <- scheme %>%
        group_by(group_id) %>%
        summarise(costTotal = sum(costTotal), type = Recommended[which.max(table(Recommended))] )

      #Get uptake data
      uptake <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/scheme-uptake.Rds"))
      scheme <- left_join(scheme,uptake, by = c("group_id" = "scheme"))

      #Add Exta Calcs
      scheme$costperperson <- round(scheme$costTotal / scheme$change,2)



      saveRDS(scheme,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes-simplified.Rds"))

      rm(scheme)

    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)
