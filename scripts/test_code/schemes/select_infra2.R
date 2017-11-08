#Gets PCT Values for the road segments

############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################

library(sf)
library(dplyr)
library(parallel)
library(igraph)
library(tmap)
tmap_mode("view")

#Settings now come from master file
#skip <- FALSE #Skip Files that already have PCT values
#ncores <- 4 #number of cores to use in parallel processing
#overwrite <- FALSE #Overwrite or create new file

#Functions


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


      #############################################################################
      # Step 4: Group into schemes

      #Add Groups Column
      osm$group_id <- NA

      osm.schemes <- osm[osm$costTotal > 0,]
      osm.schemes <- osm.schemes[,c("id","name","ref","Recommended","length","group_id")]

      ############
      # Step 1: Group Br road name and add clusters of over 1km in length
      summary(is.na(osm.schemes$name))

      osm.withref <- osm.schemes[!is.na(osm.schemes$ref),]
      #some road have a ref but no name lets tidy that up
      for(l in 1:nrow(osm.schemes)){
        reftocheck <- osm.schemes$ref[l]
        if(is.na(osm.schemes$name[l]) & !is.na(reftocheck)){
          #Fist check for other roads with the same ref
          osm.names <- osm.withref$name[osm.withref$ref == reftocheck]
          top.name <- names(which.max(table(osm.names)))
          if(is.null(top.name)){
            osm.schemes$name[l] <- reftocheck
          }else{
            osm.schemes$name[l] <- top.name
          }

        }
      }
      summary(is.na(osm.schemes$name))



      #first group by road name
      road_names <- data.frame(roadNameID = NA, name = unique(osm.schemes$name))
      road_names <- road_names[!is.na(road_names$name),]
      road_names$roadNameID <- 1:nrow(road_names)
      osm.schemes <- left_join(osm.schemes,road_names, by = c("name" = "name"))

      #If the scheme is over 1 km include it in the master list
      osm.schemes.rname <- osm.schemes[,c("roadNameID","length")]
      st_geometry(osm.schemes.rname) <- NULL
      osm.schemes.rname <- aggregate(osm.schemes.rname$length, by=list(roadNameID=osm.schemes.rname$roadNameID), FUN=sum)
      osm.schemes.rname <- osm.schemes.rname[osm.schemes.rname$x > 1000,]

      for(i in 1:nrow(osm.schemes)){
        if(osm.schemes$roadNameID[i] %in% osm.schemes.rname$roadNameID){
          osm.schemes$group_id[i] <- osm.schemes$roadNameID[i]
        }
      }

      ################# simple paths method ######################
      osm.schemes.path <- osm.schemes[is.na(osm.schemes$group_id),] #so not already grouped

      osm.schemes.path.buf <- st_buffer(osm.schemes,0.5)
      path.inter <- st_intersects(osm.schemes.path.buf)
      ls <- lengths(path.inter)
      rep <- rep(1:length(path.inter),ls)
      path.inter.df <- data.frame(from = rep, to = unlist(path.inter))

      # make a lookup
      lookup <- data.frame(numb = 1:nrow(osm.schemes.path.buf), id = osm.schemes.path.buf$id)

      path.inter.df <- left_join(path.inter.df,lookup, by = c("from" = "numb"))
      names(path.inter.df) <- c("from", "to", "id.from")
      path.inter.df <- left_join(path.inter.df,lookup, by = c("to" = "numb"))
      names(path.inter.df) <- c("from", "to", "id.from", "id.to")
      path.inter.df <- path.inter.df[,c("id.from", "id.to")]

      verts.import <- osm.schemes
      st_geometry(verts.import) <- NULL
      verts.import <- verts.import[,c("id","name","ref","Recommended","length","group_id")]
      names(verts.import) <- c("id","roadname","ref","Recommended","length","group_id")
      #verts.import$roadname <- NULL
      row.names(verts.import) <- 1:nrow(verts.import)









      g.path <- graph_from_data_frame(path.inter.df, directed = FALSE, vertices = verts.import)
      g.path <- simplify(g.path, remove.loops = T, remove.multiple = T)
      g.path <- delete.vertices(g.path, which(degree(g.path)==0))
      rm(ls,rep,path.inter.df)
      gorder(g.path)
      ecount(g.path)

      V(g.path)$degree <- degree(g.path)
      #clus = cluster_walktrap(g.path, steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)
      clus = cluster_fast_greedy(g.path, merges = TRUE, modularity = TRUE, membership = TRUE)
      #clus = communities(g.path)
      V(g.path)$member <- membership(clus)

      colours = sample ( rainbow ( max ( V(g.path)$member )  + 1) )
      V(g.path)$color = colours[V(g.path)$member +1]

      svg(filename="clusters-bristol.svg",
          width=25,
          height=20,
          pointsize=2)
      par(mar = c(1,1,1,1))
      plot(g.path,
           edge.width = 1,
           vertex.size = 1 ,
           edge.arrow.size = 0.2,
           edge.curved= 0,
           vertex.color = V(g.path)$color,
           vertex.label.family= "Helvetica",
           vertex.label.color = "black",
           vertex.frame.color = V(g.path)$color,
           layout = layout_nicely,
           rescale = T,
           axes = F)
      dev.off()


      verts <- igraph::as_data_frame(g.path, what="vertices")
      verts <- verts[,c("name","member")]
      names(verts) <- c("id","group_id")
      verts$id <- as.integer(verts$id)
      osm.schemes.path$group_id <- NULL
      osm.schemes.path <- left_join(osm.schemes.path,verts, by = c("id" = "id"))


      g.chain <- delete.vertices(g.path, which(degree(g.path) > 2)) #All the vertexs that are 1 in 1 out
      g.chain <- delete.vertices(g.chain, which(degree(g.chain) == 0)) #All the vertexs that are 1 in 1 out
      gorder(g.chain)
      ecount(g.chain)

      clus = components(g.chain, mode = "strong")
      V(g.chain)$chain <- membership(clus)
      length(unique(V(g.chain)$chain))

      verts <- igraph::as_data_frame(g.chain, what="vertices")
      verts$id <- as.integer(verts$name)
      verts <- verts[,c("id","chain")]
      verts$chain <- as.integer(verts$chain)
      verts$chain <- verts$chain + max(osm.schemes$group_id, na.rm = T)

      #join on the resutls
      osm.schemes <- left_join(osm.schemes,verts, by = c("id" = "id"))
      osm.schemes.path <- left_join(osm.schemes.path,verts, by = c("id" = "id"))


      #If the scheme is over 500m include it in the master list
      osm.schemes.path.summ <- osm.schemes.path[,c("chain","length")]
      st_geometry(osm.schemes.path.summ) <- NULL
      osm.schemes.path.summ <- aggregate(osm.schemes.path.summ$length, by=list(chain=osm.schemes.path.summ$chain), FUN=sum)
      osm.schemes.path.summ <- osm.schemes.path.summ[osm.schemes.path.summ$x > 500,]


      for(i in 1:nrow(osm.schemes)){
        if(osm.schemes$chain[i] %in% osm.schemes.path.summ$chain){
          osm.schemes$group_id[i] <- osm.schemes$chain[i]
        }
      }

      qtm(osm.schemes, lines.col = "group_id", lines.lwd = 5)
      length(unique(osm.schemes$group_id))




      #####################
      # Stept 2 Find Cluster of major infrastrucutre

      #Idetify the major netowrk clusters
      osm.schemes.major <- osm.schemes[is.na(osm.schemes$group_id),] #so not already grouped
      osm.schemes.major <- osm.schemes.major[osm.schemes.major$Recommended %in% c("Segregated Cycle Track","Segregated Cycle Track on Path","Stepped Cycle Tracks","Cycle Lanes with light segregation"),]

      osm.schemes.major.buf <- st_buffer(osm.schemes.major,5)
      major.inter <- st_intersects(osm.schemes.major.buf)
      ls <- lengths(major.inter)
      rep <- rep(1:length(major.inter),ls)
      major.inter.df <- data.frame(from = rep, to = unlist(major.inter))

      # make a lookup
      lookup <- data.frame(numb = 1:nrow(osm.schemes.major.buf), id = osm.schemes.major.buf$id)

      major.inter.df <- left_join(major.inter.df,lookup, by = c("from" = "numb"))
      names(major.inter.df) <- c("from", "to", "id.from")
      major.inter.df <- left_join(major.inter.df,lookup, by = c("to" = "numb"))
      names(major.inter.df) <- c("from", "to", "id.from", "id.to")
      major.inter.df <- major.inter.df[,c("id.from", "id.to")]


      g.major <- graph_from_data_frame(major.inter.df, directed = FALSE)
      g.major <- simplify(g.major, remove.loops = T, remove.multiple = T)
      g.major <- delete.vertices(g.major, which(degree(g.major)==0))
      rm(ls,rep,major.inter.df)
      gorder(g.major)
      ecount(g.major)

      clus = components(g.major, mode = "strong")
      V(g.major)$member <- membership(clus)
      length(unique(V(g.major)$member))

      verts <- igraph::as_data_frame(g.major, what="vertices")
      names(verts) <- c("id","ClusterIDMajor")
      verts$id <- as.integer(verts$id)
      verts$ClusterIDMajor <- as.integer(verts$ClusterIDMajor)
      verts$ClusterIDMajor <- verts$ClusterIDMajor + max(osm.schemes$group_id, na.rm = T)

      #join on the resutls
      osm.schemes <- left_join(osm.schemes,verts, by = c("id" = "id"))
      osm.schemes.major <- left_join(osm.schemes.major,verts, by = c("id" = "id"))


      #If the scheme is over 500m include it in the master list
      osm.schemes.major.summ <- osm.schemes.major[,c("ClusterIDMajor","length")]
      st_geometry(osm.schemes.major.summ) <- NULL
      osm.schemes.major.summ <- aggregate(osm.schemes.major.summ$length, by=list(ClusterIDMajor=osm.schemes.major.summ$ClusterIDMajor), FUN=sum)
      osm.schemes.major.summ <- osm.schemes.major.summ[osm.schemes.major.summ$x > 500,]


      for(i in 1:nrow(osm.schemes)){
        if(osm.schemes$ClusterIDMajor[i] %in% osm.schemes.major.summ$ClusterIDMajor){
          osm.schemes$group_id[i] <- osm.schemes$ClusterIDMajor[i]
        }
      }

      qtm(osm.schemes, lines.col = "group_id", lines.lwd = 5)
      length(unique(osm.schemes$group_id))


      #####################
      # Step 3 Find Cluster of Minor infrastrucutre

      #Idetify the minor netowrk clusters
      osm.schemes.minor <- osm.schemes[is.na(osm.schemes$group_id),] #so not already grouped
      osm.schemes.minor <- osm.schemes.minor[osm.schemes.minor$Recommended %in% c("Cycle Street" ,"Cycle Lanes"),]

      osm.schemes.minor.buf <- st_buffer(osm.schemes.minor,5)
      minor.inter <- st_intersects(osm.schemes.minor.buf)
      ls <- lengths(minor.inter)
      rep <- rep(1:length(minor.inter),ls)
      minor.inter.df <- data.frame(from = rep, to = unlist(minor.inter))

      # make a lookup
      lookup <- data.frame(numb = 1:nrow(osm.schemes.minor.buf), id = osm.schemes.minor.buf$id)

      minor.inter.df <- left_join(minor.inter.df,lookup, by = c("from" = "numb"))
      names(minor.inter.df) <- c("from", "to", "id.from")
      minor.inter.df <- left_join(minor.inter.df,lookup, by = c("to" = "numb"))
      names(minor.inter.df) <- c("from", "to", "id.from", "id.to")
      minor.inter.df <- minor.inter.df[,c("id.from", "id.to")]


      g.minor <- graph_from_data_frame(minor.inter.df, directed = FALSE)
      g.minor <- simplify(g.minor, remove.loops = T, remove.multiple = T)
      g.minor <- delete.vertices(g.minor, which(degree(g.minor)==0))
      rm(ls,rep,minor.inter.df)
      gorder(g.minor)
      ecount(g.minor)

      clus = components(g.minor, mode = "strong")
      V(g.minor)$member <- membership(clus)
      length(unique(V(g.minor)$member))

      verts <- igraph::as_data_frame(g.minor, what="vertices")
      names(verts) <- c("id","ClusterIDminor")
      verts$id <- as.integer(verts$id)
      verts$ClusterIDminor <- as.integer(verts$ClusterIDminor)
      verts$ClusterIDminor <- verts$ClusterIDminor + max(osm.schemes$group_id, na.rm = T)

      #join on the resutls
      osm.schemes <- left_join(osm.schemes,verts, by = c("id" = "id"))
      osm.schemes.minor <- left_join(osm.schemes.minor,verts, by = c("id" = "id"))


      #If the scheme is over 500 include it in the master list
      osm.schemes.minor.summ <- osm.schemes.minor[,c("ClusterIDminor","length")]
      st_geometry(osm.schemes.minor.summ) <- NULL
      osm.schemes.minor.summ <- aggregate(osm.schemes.minor.summ$length, by=list(ClusterIDminor=osm.schemes.minor.summ$ClusterIDminor), FUN=sum)
      osm.schemes.minor.summ <- osm.schemes.minor.summ[osm.schemes.minor.summ$x > 500,]


      for(i in 1:nrow(osm.schemes)){
        if(osm.schemes$ClusterIDminor[i] %in% osm.schemes.minor.summ$ClusterIDminor){
          osm.schemes$group_id[i] <- osm.schemes$ClusterIDminor[i]
        }
      }

      qtm(osm.schemes, lines.col = "group_id", lines.lwd = 5)
      length(unique(osm.schemes$group_id))



















      #Make shared road names into a single geometry
      osm.schemes.union <- data.frame(roadNameID = unique(osm.schemes$roadNameID), geometry = NA)
      osm.schemes.nona <- osm.schemes[!is.na(osm.schemes$roadNameID),]
      osm.schemes.comp <- list()
      road_ids <- osm.schemes.union$roadNameID
      road_ids <- road_ids[!is.na(road_ids)]
      for(j in road_ids){
        lines <- osm.schemes.nona[osm.schemes.nona$roadNameID == j,]
        lines.geom <- st_combine(lines)
        lines <- data.frame(roadNameID = j, geometry = NA)
        st_geometry(lines) <- lines.geom
        osm.schemes.comp[[j]] <- lines
      }
      osm.schemes.comp <- do.call("rbind",osm.schemes.comp)

      #Look at the interactions of the different road sections
      roads.inter <- st_intersects(osm.schemes.comp)
      #roads.inter <- st_intersects(osm.schemes)

      ls <- lengths(roads.inter)
      rep <- rep(1:length(roads.inter),ls)
      roads.inter.df <- data.frame(from = rep, to = unlist(roads.inter))
      g.roadname <- graph_from_data_frame(roads.inter.df, directed = FALSE)
      g.roadname <- simplify(g.roadname, remove.loops = T, remove.multiple = T)
      gorder(g.roadname)
      ecount(g.roadname)
      g.roadname.trim <- delete.vertices(g.roadname, which(degree(g.roadname)==0))
      gorder(g.roadname.trim)
      ecount(g.roadname.trim)

      plot(g.roadname.trim, layout = layout_nicely,  edge.width = 2, vertex.size = 2)

      clus = cluster_walktrap(g.roadname.trim, steps = 10, merges = TRUE, modularity = TRUE, membership = TRUE)
      V(g.roadname.trim)$member <- membership(clus)
      length(unique(V(g.roadname.trim)$member))

      verts <- igraph::as_data_frame(g.roadname.trim, what="vertices")
      names(verts) <- c("roadNameID","ClusterID")
      verts$roadNameID <- as.integer(verts$roadNameID)
      verts$ClusterID <- as.integer(verts$ClusterID)
      osm.schemes <- left_join(osm.schemes,verts, by = c("roadNameID" = "roadNameID"))


      foo <- group_by(osm.schemes, roadNameID)











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
