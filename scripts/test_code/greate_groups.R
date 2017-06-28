library(sf)
library(igraph)

#Input Data
osm <- readRDS("../example-data/bristol/results/osm-select-infra.Rds")
buff_dists = 20


#Agg Groups Column
osm$group_id <- NA

grp_start = 0
#############################################################
#Step 1: Segregated Cycle Track
sub <- osm[osm$infra_score == "Segregated Cycle Track" & (osm$change == "upgrade" | osm$change == "upgrade (one side)"),]
sub <- sub[,c("id")]
buff <- st_buffer(sub, buff_dists)
#Find Instersections
inter <- st_intersects(buff,buff)
edges <- do.call(rbind, lapply(inter, function(x) {
  if (length(x) > 1) cbind(head(x, -1), tail(x, -1)) else NULL
}))
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
#Update Main Table
for(c in 1:nrow(sub)){
  osm$group_id[osm$id == sub$id[c]] <- sub$group_id[c]
}
#Cleanup and get next group number
grp_start <- max(sub$group_id, na.rm = T)
rm(sub,buff,edges,a,b,c,g,grps,res,inter)

##################################################################################################
#Step 2: Stepped Cycle Tracks
sub <- osm[osm$infra_score == "Stepped Cycle Tracks" & (osm$change == "upgrade" | osm$change == "upgrade (one side)"),]
sub <- sub[,c("id")]
buff <- st_buffer(sub, buff_dists)
#Find Instersections
inter <- st_intersects(buff,buff)
edges <- do.call(rbind, lapply(inter, function(x) {
  if (length(x) > 1) cbind(head(x, -1), tail(x, -1)) else NULL
}))
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
#Update Main Table
for(c in 1:nrow(sub)){
  osm$group_id[osm$id == sub$id[c]] <- sub$group_id[c]
}
#Cleanup and get next group number
grp_start <- max(sub$group_id, na.rm = T)
rm(sub,buff,edges,a,b,c,g,grps,res,inter)



##################################################################################################
#Step 3: Cycle Lanes with light segregation
sub <- osm[osm$infra_score == "Cycle Lanes with light segregation" & (osm$change == "upgrade" | osm$change == "upgrade (one side)"),]
sub <- sub[,c("id")]
buff <- st_buffer(sub, buff_dists)
#Find Instersections
inter <- st_intersects(buff,buff)
edges <- do.call(rbind, lapply(inter, function(x) {
  if (length(x) > 1) cbind(head(x, -1), tail(x, -1)) else NULL
}))
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
#Update Main Table
for(c in 1:nrow(sub)){
  osm$group_id[osm$id == sub$id[c]] <- sub$group_id[c]
}
#Cleanup and get next group number
grp_start <- max(sub$group_id, na.rm = T)
rm(sub,buff,edges,a,b,c,g,grps,res,inter)


##################################################################################################
#Step 4: Track/Path
sub <- osm[osm$infra_score == "Track/Path" & (osm$change == "upgrade" | osm$change == "upgrade (one side)"),]
sub <- sub[,c("id")]
buff <- st_buffer(sub, buff_dists)
#Find Instersections
inter <- st_intersects(buff,buff)
edges <- do.call(rbind, lapply(inter, function(x) {
  if (length(x) > 1) cbind(head(x, -1), tail(x, -1)) else NULL
}))
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
#Update Main Table
for(c in 1:nrow(sub)){
  osm$group_id[osm$id == sub$id[c]] <- sub$group_id[c]
}
#Cleanup and get next group number
grp_start <- max(sub$group_id, na.rm = T)
rm(sub,buff,edges,a,b,c,g,grps,res,inter)


##################################################################################################
#Step 5: Cycle Lanes
sub <- osm[osm$infra_score == "Cycle Lanes" & (osm$change == "upgrade" | osm$change == "upgrade (one side)"),]
sub <- sub[,c("id")]
buff <- st_buffer(sub, buff_dists)
#Find Instersections
inter <- st_intersects(buff,buff)
edges <- do.call(rbind, lapply(inter, function(x) {
  if (length(x) > 1) cbind(head(x, -1), tail(x, -1)) else NULL
}))
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
#Update Main Table
for(c in 1:nrow(sub)){
  osm$group_id[osm$id == sub$id[c]] <- sub$group_id[c]
}
#Cleanup and get next group number
grp_start <- max(sub$group_id, na.rm = T)
rm(sub,buff,edges,a,b,c,g,grps,res,inter)



##################################################################################################
#Step 6: Cycle Street
sub <- osm[osm$infra_score == "Cycle Street" & (osm$change == "upgrade" | osm$change == "upgrade (one side)"),]
sub <- sub[,c("id")]
buff <- st_buffer(sub, buff_dists)
#Find Instersections
inter <- st_intersects(buff,buff)
edges <- do.call(rbind, lapply(inter, function(x) {
  if (length(x) > 1) cbind(head(x, -1), tail(x, -1)) else NULL
}))
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
#Update Main Table
for(c in 1:nrow(sub)){
  osm$group_id[osm$id == sub$id[c]] <- sub$group_id[c]
}
#Cleanup and get next group number
grp_start <- max(sub$group_id, na.rm = T)
rm(sub,buff,edges,a,b,c,g,grps,res,inter)





print(paste0("There are ",length(unique(osm$group_id))," groups using a buffer lenght of ",buff_dists))
print(paste0(nrow(osm[is.na(osm$group_id) & osm$change == "upgrade",])," of ",nrow(osm[osm$change == "upgrade",])," lines were not classified"))

library(tmap)
tmap_mode("view")
tmap_style("col_blind")
qtm(osm[osm$change == "upgrade",], lines.col = "group_id", lines.lwd = 3)


#Creat Polygons Around each scheme
schemepoly <- function(a){
  sub <- osm_sub[osm_sub$group_id == a,]
  if(sub$infra_score[1] == "Segregated Cycle Track" |
     sub$infra_score[1] == "Stepped Cycle Tracks" |
     sub$infra_score[1] == "Cycle Lanes with light segregation" |
     sub$infra_score[1] == "Cycle Lanes" ){
    sub <- sub$geometry
    buf <- st_buffer(sub,100)
    buf <- st_union(buf)
    buf <- st_simplify(buf, preserveTopology = FALSE, dTolerance = 0)
    return(buf)
  }else{
    sub <- sub$geometry
    buf <- st_buffer(sub,10)
    buf <- st_union(buf)
    poly <- st_convex_hull(buf)
    return(poly)
  }
}


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


for(d in 1:nrow(schemes)){
  id <- schemes$group[d]
  schemes$type[d] <- osm_sub$infra_score[osm_sub$group_id == id][1]
  schemes$length[d] <- sum(osm_sub$length[osm_sub$group_id == id])
  schemes$cost[d] <- sum(osm_sub$cost.total[osm_sub$group_id == id])
}

schemes$type <- as.factor(schemes$type)
schemes <- st_transform(schemes, 4326)
saveRDS(schemes,"../example-data/bristol/results/schemes.Rds")
saveRDS(osm, "../example-data/bristol/results/osm-schemes.Rds")


style <- c("Red","Orange","Green","Blue","Purple","Yellow")
#qtm(schemes, fill = "type", style = style)



pal <- colorFactor(c("red","green","blue","orange","yellow","purple"), schemes$type)
popup <- paste("Scheme Number:", schemes$group, "<br>",
               "Infrastructure Type: ", schemes$type, "<br>",
               "Total Length: ", schemes$length, "<br>",
               "Total Cost: ", schemes$cost,"<br>",
               paste0("<a href=\"http://www.cyipt.bike/demo/bristol/scheme?id=",schemes$group,"\">Details</a>"))



leaflet(schemes) %>%
  #Base map options
  addProviderTiles("OpenMapSurfer.Grayscale", group = "Greyscale") %>%

  #Existing Infrastructure
  addPolygons(data = schemes,
              color = ~pal(schemes$type),
              weight = 4,
              popup = popup,
              highlightOptions = highlightOptions(color = "black", weight = 4,
                                                  bringToFront = TRUE)) %>%
  addLegend(pal = pal,
            values = ~type,
            opacity = 1,
            title = "Existing Cycling Infrastructure")





