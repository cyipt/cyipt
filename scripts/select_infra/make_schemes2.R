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
  if(file.exists(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds"))
    #Check if PCT values exist in the file
    if(all(c("group_id") %in% names(osm)) & skip){
      message(paste0("Scheme numbers already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Getting schemes for ",regions[b]," at ",Sys.time()))

      #If overwriting remove old data
      col.to.keep <- names(osm)[!names(osm) %in% c("group_id")]
      osm <- osm[,col.to.keep]
      rm(col.to.keep)

      # Group into schemes
      if(sum(osm$costTotal) != 0){

        osm.schemes <- osm[osm$costTotal > 0,]
        osm.schemes <- osm.schemes[,c("id","name","ref","highway","Recommended","length")]

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
        names(osm.schemes) <- c("id","roadname","ref","highway","Recommended","length") #igraph does not like "name"

        #Separate Residential from all other road types
        rtype <- osm.schemes[,c("id","highway")]

        inter.df <- left_join(inter.df, rtype, by = c("id.from" = "id"))
        names(inter.df) <- c("id.from","id.to","highway.from")
        inter.df <- left_join(inter.df, rtype, by = c("id.to" = "id"))
        names(inter.df) <- c("id.from","id.to","highway.from","highway.to")
        #remove interaction between residential and non residential roads
        inter.df <- inter.df[!xor(inter.df$highway.from == "residential", inter.df$highway.to == "residential"),]
        inter.df <- inter.df[,c("id.from","id.to")]


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


        #Remove Tiny Schemes
        # Get scheme numbers
        scheme_nos <- unique(schemes$group_id)
        scheme_nos <- scheme_nos[order(scheme_nos)]

        # Sum lengths of each scheme
        length.schemes <- function(x){
          result <- sum(schemes$length[schemes$group_id == x])
          return(result)
        }

        scheme.lengths <- data.frame(scheme = scheme_nos, length = NA)
        scheme.lengths$length <- sapply(scheme_nos, length.schemes)

        #Find Long Schemes
        scheme.lengths <- scheme.lengths[scheme.lengths$length > 200,]

        schemes <- schemes[schemes$group_id %in% scheme.lengths$scheme,]
        rm(scheme.lengths, scheme_nos)

        rm(verts,g,clus)

        #Dump Unneded Data
        schemes <- schemes[,c("group_id","costTotal","Recommended")]

        #Summarise
        schemes <- schemes %>%
          group_by(group_id) %>%
          summarise(costTotal = sum(costTotal), type = Recommended[which.max(table(Recommended))] )



      }else{
        message("No infrastructure upgrades were found")
        osm$group_id <- NA
        schemes <- 0
      }


      #Save results
      if(overwrite){
        saveRDS(osm,paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds"))
        saveRDS(schemes,paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes.Rds"))
      }else{
        saveRDS(osm,paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines-schemes.Rds"))
        saveRDS(schemes,paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes.Rds"))
      }
      rm(osm,schemes)

    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)
