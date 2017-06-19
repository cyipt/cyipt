sub <- osm[osm$change != "no",]
sub2 <- sub[sub$infra_score == "Segregated Cycle Track",]
buff <- st_buffer(sub2, 1)

dist <- st_distance(sub2[1:5,])
inter <- st_intersects(buff,buff)
summary(lengths(inter))


edges <- do.call(rbind, lapply(inter, function(x) {
  if (length(x) > 1) cbind(head(x, -1), tail(x, -1)) else NULL
}))
edges

library(igraph)
g <- graph.data.frame(edges, directed=FALSE)
g <- split(V(g)$name, clusters(g)$membership)

grps <- list()
for(a in 1:length(g)){
  grps[[a]] <- as.numeric(unlist(g[a]))
}

sub2$group_id <- NA

for(b in 1:nrow(sub2)){
  res <- which(sapply(grps,`%in%`, x = b))
  if(length(res) == 0){
    sub2$group_id[b] <- 0
  }else{
    sub2$group_id[b] <- res
  }

}

plot(sub2["group_id"])

tm_shape(sub2) +
  tm_lines(col = "group_id", lwd = 5, alpha = 1,
           title.col = "ID",
           popup.vars = c("infra_score","existing_infra", "highway", "cycleway"))


conclusions <- data.frame(project = unique(sub2$group_id), cost = 0)

for(c in 1:nrow(conclusions)){
  conclusions$cost[c] <- sum(sub2$cost.total[sub2$group_id == conclusions$project[c]])
}



test <- sub2[sub2$group_id == 1,]
test2 <- st_cast(test, "LINESTRING")
test3 <- st_union(test)
st_geometry_type(test3)
test4 <- st_cast(test3, "LINESTRING")


sub3 <- sub2[,"group_id"]


buff <- st_buffer(sub3, 50)


any(grps %in% 1)

test <- list(rep(c(1:2), 21))

grps <- vector(mode = "list", length = 21)



test[[1]] <- as.numeric(unlist(g[1]))







list2vec <- function(x){
  return(as.numeric(unlist(g[x])))
}

grps <- lapply(1:length(g),list2vec)
class(grps)
class(grps[1])

test <- any(grps %in% 1)

test <- list(1:length(g))

g3 <- unlist(g2, recursive=FALSE)
m1 <- do.call(rbind, g2)

sub2$group_id

a = 1
test <- any(g2 %in% a)
test <- sapply(grid_osm,function(x)any(x %in% grd))




list2vec(3)
