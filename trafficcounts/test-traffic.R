# Aim: test traffic counts data
library(tidyverse)
library(tmap)
library(sf)
tc = read_csv("trafficcounts/trafficcounts.csv")
names(tc)
sapply(tc, class)
col_types = rep("c", times = ncol(tc))
col_types[grepl(pattern = "pcu|[0-9]|latitude|longi", names(tc))] = "d"
col_types = paste0(col_types, collapse = "")
tc = read_csv("trafficcounts/trafficcounts.csv", col_types = col_types)

tc # 174 vars, 33k rows
ts = st_sfc(st_multipoint(cbind(tc$longitude, tc$latitude)))
ts = ts %>% st_cast(to = "POINT")
tc = st_sf(tc, ts, crs = 4326)

# Explore linking variables (to assign to osm)
summary(as.factor(tc$road)) # 10k, 1/3rd
plot(tc[1]) # UK coverage

# Pick a single road
roadname = "A4018"
t1 = filter(tc, road == roadname)
tmap_mode("view")
qtm(t1)

# Link with OSM ids
ways = readRDS("../example-data/bristol/osm-lines-quietness-full.Rds")
plot(ways[1:1000, 1])
plot(t1, add = T)
head(ways$name)
t1$road
sapply(ways, function(x) summary(x %in% t1$road[1])) # it's the ref col
wt = filter(ways, ref == t1$road[1])
qtm(wt) # a single road composed of 119 osm elements
tbuff = geo_buffer(shp = t1, dist = 100)
sel_buff = st_intersects(wt, tbuff, sparse = FALSE)

# using a for loop (slow)
for(i in 1:nrow(t1)) {
  wt$pcu_100[sel_buff[,i]] = tbuff$all_motors_pcu_16[i]
}

# using a matrix (fast)
sel_buff_any = apply(sel_buff, 1, any)
sel_buff_which = unlist(apply(sel_buff, 1, which))
wt$pcu_100[sel_buff_any] == tbuff$all_motors_pcu_16[sel_buff_which]

# with voronoi polygons
v = dismo::voronoi(xy = st_coordinates(t1), ext = raster::extent(st_coordinates(wt)[,1:2]))
v_sf = as(v, "sf")
st_crs(v_sf) = st_crs(t1)
plot(v_sf)
v_sf = st_join(x = v_sf, y = t1["all_motors_pcu_16"])
wt$pcu_v = st_join(wt, v_sf["all_motors_pcu_16"], FUN = mean)$all_motors_pcu_16
plot(wt, add = T)

# plot results
qtm(wt, lines.col = "pcu_100", lines.lwd = 30) +
  qtm(wt, lines.col = "pcu_v", lines.lwd = 10)

wt_out = select(wt, osm_id, pcu_100, pcu_v)
st_geometry(wt_out) = NULL
write_csv(wt_out, "pcu_data_osm.csv")

# for all counters in Bristol
