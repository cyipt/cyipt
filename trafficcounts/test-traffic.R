# Aim: test traffic counts data

# Dependencies
library(tidyverse)
library(tmap)
library(sf)
devtools::install()
library(cyipt)


# checking and preprocessing tc data
tc = read_csv("trafficcounts/trafficcounts.csv")
names(tc)
# sapply(tc, class) # check col types
col_types = rep("c", times = ncol(tc))
col_types[grepl(pattern = "pcu|[0-9]|latitude|longi", names(tc))] = "d"
col_types = paste0(col_types, collapse = "")
# read-in again with correct column types
tc = read_csv("trafficcounts/trafficcounts.csv", col_types = col_types)
tc # 174 vars, 33k rows
# Explore linking variables (to assign to osm)
summary(as.factor(tc$road))[1:9] / nrow(tc) * 100 # ~1/3rd rd names are NA:
tc$road[tc$road %in% c("C", "U")] = NA

# Make spatial
ts = st_sfc(st_multipoint(cbind(tc$longitude, tc$latitude)))
ts = ts %>% st_cast(to = "POINT")
tc = st_sf(tc, ts, crs = 4326)
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

# # using a for loop (slow)
# for(i in 1:nrow(t1)) {
#   wt$pcu_100[sel_buff[,i]] = tbuff$all_motors_pcu_16[i]
# }

# using a matrix (fast, but faster with st_join)
sel_buff_any = apply(sel_buff, 1, any)
sel_buff_which = unlist(apply(sel_buff, 1, which))
tbuff = tbuff %>% rename(pcu = all_motors_pcu_16)
wt = st_join(wt, tbuff["pcu"])

# with voronoi polygons
all_coords = rbind(st_coordinates(wt)[,1:2], st_coordinates(t1)[,1:2])
ext = raster::extent(all_coords)
v = dismo::voronoi(xy = st_coordinates(t1), ext = ext)
v_sf = as(v, "sf")
st_crs(v_sf) = st_crs(t1)
v_sf = st_join(x = v_sf, y = t1["all_motors_pcu_16"])
v_sf = v_sf %>% rename(pcu_v = all_motors_pcu_16)
wt$pcu_v = st_join(wt, v_sf["pcu_v"], FUN = mean)$pcu_v

# plot results
qtm(wt, lines.col = "pcu", lines.lwd = 30) +
  qtm(wt, lines.col = "pcu_v", lines.lwd = 10) +
  qtm(t1)

wt_out = select(wt, osm_id, pcu, pcu_v)
st_geometry(wt_out) = NULL
# write_csv(wt_out, "pcu_data_osm.csv")

# for all counters in Bristol
rm(wt)
if(exists("wt_out")) rm(wt_out)
region = st_read("areas/bristol-poly.geojson")
tc_region = tc %>% filter(st_intersects(., region, sparse = F))
qtm(tc_region) # it works!
t_roads = unique(tc_region$road)
i = 5 # for testing
for(i in seq_along(t_roads)) {
  count_num = round(i / length(t_roads) * 100)
  message(paste0(count_num), " % done")
  roadname = t_roads[i]
  if(is.na(roadname))
    next()
  t1 = filter(tc, road == roadname)
  wt = filter(ways, ref == roadname)

  tbuff = geo_buffer(shp = t1, dist = 100)
  sel_buff = st_intersects(wt, tbuff, sparse = FALSE)

  # geo link (if road name link fail)
  if(nrow(wt) == 0) {
    wt = ways[tbuff,]
    if(nrow(wt) == 0) {
      message(paste0("No road & spatial matches found for the road "), roadname)
      next()
    }
  }

  qtm(wt) + # plot roads for checking
    qtm(t1)

  # # find roads associated with un-matched counters - not implemented
  # sel_buff_any = apply(sel_buff, 1, any)
  # sel_traf_any = apply(sel_buff, 2, any)
  # t_unmatched = tbuff[!sel_traf_any,]

  # spatal join with tbuff data
  tbuff = tbuff %>% rename(pcu = all_motors_pcu_16)
  wt = st_join(wt, tbuff["pcu"])

  # voronoi join
  all_coords = rbind(st_coordinates(wt)[,1:2], st_coordinates(t1)[,1:2])
  ext = raster::extent(all_coords)
  if(nrow(t1) <= 1) {
    wt$pcu_v = NA
  } else {
    v = dismo::voronoi(xy = st_coordinates(t1), ext = ext)
    v_sf = as(v, "sf")
    st_crs(v_sf) = st_crs(t1)
    v_sf = st_join(x = v_sf, y = t1["all_motors_pcu_16"])
    v_sf = v_sf %>% rename(pcu_v = all_motors_pcu_16)
    wt$pcu_v = st_join(wt, v_sf["pcu_v"], FUN = mean)$pcu_v

    qtm(wt, lines.col = "pcu", lines.lwd = 30) +
      qtm(wt, lines.col = "pcu_v", lines.lwd = 10) +
      qtm(t1)
  }

  st_geometry(wt) = NULL

  if(i == 1) {
    wt_out = select(wt, osm_id, pcu, pcu_v)
    } else {
    wt_out = bind_rows(wt_out, select(wt, osm_id, pcu, pcu_v))
  }
}


