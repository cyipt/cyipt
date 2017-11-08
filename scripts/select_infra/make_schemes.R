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
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
    #Check if PCT values exist in the file
    if(all(c("group_id") %in% names(osm)) & skip){
      message(paste0("Scheme numbers already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting infrastructure types values for ",regions[b]," at ",Sys.time()))

      #If overwriting remove old data
      col.to.keep <- names(osm)[!names(osm) %in% c("group_id")]
      osm <- osm[,col.to.keep]
      rm(col.to.keep)

      # Group into schemes
      if(sum(osm$costTotal) != 0){

        osm.schemes <- osm[osm$costTotal > 0,]
        osm.schemes <- osm.schemes[,c("id","name","ref","Recommended","length")]

        osm.schemes <- st_buffer(osm.schemes,5)
        inter <- st_intersects(osm.schemes)
        ls <- lengths(inter)
        rep <- rep(1:length(inter),ls)
        inter.df <- data.frame(from = rep, to = unlist(inter))
        rm(ls,rep,inter)

        # make a lookup
        lookup <- data.frame(numb = 1:nrow(osm.schemes), id = osm.schemes$id)
        inter.df <- left_join(inter.df,lookup, by = c("from" = "numb"))
        names(inter.df) <- c("from", "to", "id.from")
        inter.df <- left_join(inter.df,lookup, by = c("to" = "numb"))
        names(inter.df) <- c("from", "to", "id.from", "id.to")
        inter.df <- inter.df[,c("id.from", "id.to")]
        rm(lookup)

        st_geometry(osm.schemes) <- NULL
        names(osm.schemes) <- c("id","roadname","ref","Recommended","length") #igraph does not like "name"

        # Make Graph
        g <- graph_from_data_frame(inter.df, directed = FALSE, vertices = osm.schemes)
        g <- simplify(g, remove.loops = T, remove.multiple = T)
        rm(osm.schemes, inter.df)
        clus = cluster_fast_greedy(g, merges = TRUE, modularity = TRUE, membership = TRUE)
        V(g)$member <- membership(clus)

        # export graph and attach to osm
        verts <- igraph::as_data_frame(g, what="vertices")
        verts <- verts[,c("name","member")]
        names(verts) <- c("id","group_id")
        verts$id <- as.integer(verts$id)
        #osm.schemes.path$group_id <- NULL
        osm <- left_join(osm,verts, by = c("id" = "id"))
        schemes <- osm[!is.na(osm$group_id),]

        rm(verts,g,clus)

      }else{
        message("No infrastructure upgrades were found")
        osm$group_id <- NA
        schemes <- 0
      }


      #Save results
      if(overwrite){
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
        saveRDS(schemes,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes.Rds"))
      }else{
        saveRDS(osm,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines-schemes.Rds"))
        saveRDS(schemes,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes.Rds"))
      }
      rm(osm,schemes)

    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)
