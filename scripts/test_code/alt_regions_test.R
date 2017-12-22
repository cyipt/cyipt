# Regions Checking
library(sf)
library(igraph)
library(dplyr)
library(tmap)
tmap_mode("view")

pct <- readRDS("../cyipt-securedata/pct-routes-all.Rds")

pct <- as.data.frame(pct)
pct$geometry <- NULL

pct <- pct[,c("lsoa1","lsoa2","pct.census","all_16p")]
names(pct) <- c("lsoa1","lsoa2","bike","all")

g <- graph_from_data_frame(pct, directed = FALSE)
gorder(g)
clus <- cluster_walktrap(g, weights = E(g)$bike)
V(g)$members <- membership(clus)
verts <- igraph::as_data_frame(g, what = "vertices")


bounds <- st_read("../cyipt-bigdata/boundaries/LSOA/Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales.shp")
bounds <- bounds[,c("lsoa11cd")]

bounds <- left_join(bounds, verts, by = c("lsoa11cd" = "name"))

bounds.simple <- bounds %>% group_by(members) %>% summarise()
st_crs(bounds.simple) <- 27700
bounds.simple <- st_simplify(bounds.simple, 100)
qtm(bounds.simple, fill = "members")
