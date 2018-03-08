library(tmap)
tmap_mode("view")
library(sf)

inter <- readRDS("N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyipt-securedata/uptakemodel/osm_rf_inter.Rds")
routes <- readRDS("N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyipt-securedata/uptakemodel/routes01_11.Rds")
osm <- readRDS("N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyipt-securedata/uptakemodel/osm_clean.Rds")
buffer <- readRDS("N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyipt-securedata/uptakemodel/rf_buff.Rds")

# range distplacement (find the lines on either side)

line.number <- 61368
range.neg <- 2
range.pos <- 2
line.numbers <- c((line.number-range.neg):(line.number+range.pos))

routes.sub <- routes[line.numbers,]
buffer.sub <- buffer[line.numbers,]
ids <- unique(unlist(inter[line.numbers]))
osm.sub <- osm[ids,]

plot(routes.sub$geometry[c(1:range.neg,(range.neg+2):nrow(routes.sub))], col = "red", lwd = 2)
plot(routes.sub$geometry[(range.neg+1)], col = "green", lwd = 3, add = T)
plot(osm.sub$geometry, col = "blue", add = T)

# fixed displacement (find just one line dispalced by X rows)

line.number <- 84542
displace <- 0

routes.sub <- routes[line.number,]
buffer.sub <- buffer[line.number,]
ids <- unique(unlist(inter[line.number + displace]))
osm.sub <- osm[ids,]

plot(routes.sub$geometry, col = "red", lwd = 2)
plot(osm.sub$geometry, col = "blue", add = T)
plot(buffer.sub$geometry, add = T, col = "green")
