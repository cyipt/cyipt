# Find Cycling Regions
library(sf)
library(igraph)
library(dplyr)
library(tmap)
tmap_mode("view")

pct.all <- readRDS("../cyipt-securedata/pct-routes-all.Rds") #Read in All the route with any cycing at all in any scenario
pct.all <- pct.all[,c("ID","pct.census")] # use ebike as the most optimistic case
pct.all <- pct.all[pct.all$pct.census > 2,] # Remove the lowes cycling routes #dutch 8 is good, 7 is better # 6 is a bit too far
pct.all.buf <- st_buffer(pct.all, 2, nQuadSegs = 2)


inter <- st_intersects(pct.all.buf, sparse = TRUE)

sum(lengths(inter))

ls <- lengths(inter)
rep <- rep(1:length(inter),ls)
df <- data.frame(from = rep, to = unlist(inter))

#Make look up of row number to route ID
lookup <- data.frame(numb = 1:nrow(pct.all), id = pct.all$ID)

df <- left_join(df,lookup, by = c("from" = "numb"))
names(df) <- c("from", "to", "id.from")
df <- left_join(df,lookup, by = c("to" = "numb"))
names(df) <- c("from", "to", "id.from", "id.to")
df <- df[,c("id.from", "id.to")]


g <- graph_from_data_frame(df, directed = FALSE)
g <- simplify(g, remove.loops = T, remove.multiple = T)
gorder(g)
ecount(g)
g.trim <- delete.vertices(g, which(degree(g)<=1))
gorder(g.trim)
ecount(g.trim)

#clus = cluster_walktrap(g.trim, steps = 6, merges = TRUE, modularity = TRUE, membership = TRUE)
#clus = cluster_edge_betweenness(g.trim, merges = FALSE, modularity = FALSE, membership = TRUE)
#clus = cluster_fast_greedy(g.trim, merges = FALSE, modularity = FALSE, membership = TRUE)
#clus = cluster_leading_eigen(g.trim)
clus <- components(g.trim, mode = "weak")
V(g.trim)$member <- membership(clus)
#V(g.trim)$member <- membership(clus)

#cliques <- cliques(g.trim, min = 5, max = NULL)

#colours = sample ( rainbow ( max ( V(g.trim)$member )  + 1) )
#V(g.trim)$colour = colours[V(g.trim)$member +1]

verts <- igraph::as_data_frame(g.trim, what="vertices")
row.names(verts) <- 1:nrow(verts)

# Make centroid of the lines
pct.main <- pct.all[pct.all$ID %in% verts$name,]
#cents <- st_centroid(pct.main)

#cents <- left_join(cents, verts, by = c("ID" = "name"))
pct.main <- left_join(pct.main, verts, by = c("ID" = "name"))


poly.list <- list()

for(i in unique(pct.main$member)){
  lines.sub <- pct.main[pct.main$member == i,]
  lines.sub <- st_combine(lines.sub)
  poly.geom <- st_convex_hull(lines.sub)
  poly <- data.frame(member = i, geometry = NA)
  st_geometry(poly) <- poly.geom
  poly.list[[i]] <- poly
}


poly.all <- do.call("rbind", poly.list)
#qtm(poly.all, fill = "member")

# Merg togther small polygons that are contain within large polygons

cont <- st_contains(poly.all)
cont.unlist <- as.data.frame(table(unlist(cont)))
poly.cont.list <- list()
for(j in 1:nrow(poly.all)){
  if(length(cont[[j]]) > 1){
    polys <- poly.all[cont[[j]],]
    poly.geom <- st_union(polys, by_feature = FALSE)
    poly <- data.frame(member = j, geometry = NA)
    st_geometry(poly) <- poly.geom
    st_crs(poly) <- 27700
    poly.cont.list[[j]] <- poly
  }else if(cont.unlist$Freq[cont.unlist$Var1 == j] == 1 ){
    poly <- poly.all[j,]
    st_crs(poly) <- 27700
    poly.cont.list[[j]] <- poly

  }
}

poly.cont.list <- poly.cont.list[lapply(poly.cont.list,length)>0]
poly.cont <- do.call("rbind", poly.cont.list)
#qtm(poly.cont, fill = "member")

poly.area <- as.numeric(st_area(poly.cont))

lsoa <- st_read("../cyipt-bigdata/boundaries/LSOA/Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales.shp")
st_crs(lsoa) <- 27700

#remove wales
lsoa$lsoa11cd <- as.character(lsoa$lsoa11cd)
lsoa <- lsoa[substr(lsoa$lsoa11cd,1,1) == "E",]

inter.lsoa <- st_intersects(lsoa, poly.cont)

lsoa$region <- NA

for(k in 1:nrow(lsoa)){
  if(length(inter.lsoa[[k]]) == 1){
    lsoa$region[k] <- inter.lsoa[[k]]
  }else if(length(inter.lsoa[[k]]) > 1){
    areas <- poly.area[inter.lsoa[[k]]]
    lsoa$region[k] <- inter.lsoa[[k]][which.max(areas)]
  }
}

lsoa.region <- lsoa %>% group_by(region) %>% summarize()
qtm(lsoa.region, fill = "region")

