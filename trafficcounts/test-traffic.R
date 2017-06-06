# Aim: test traffic counts data

# Dependencies
library(tidyverse)
library(tmap)
library(sf)
library(osmdata)
# devtools::install()
source("R/geobuffer.R")

region = st_read("areas/bristol-poly.geojson")

q = opq(st_bbox(region)) %>%
  add_feature(key = "highway")
# res = osmdata_sf(q = q)
# ways = res$osm_lines
# checking and preprocessing tc data
tc = read_csv("trafficcounts/trafficcounts.csv")
names(tc)
# sapply(tc, class) # check col types
col_types = rep("c", times = ncol(tc))
col_types[grepl(pattern = "pcu|[0-9]|ati|ongi", names(tc))] = "d"
col_types = paste0(col_types, collapse = "")
# read-in again with correct column types
tc = read_csv("trafficcounts/trafficcounts.csv", col_types = col_types)
tc # 174 vars, 33k rows
# Explore linking variables (to assign to osm)
summary(as.factor(tc$road))[1:9] / nrow(tc) * 100 # ~1/3rd rd names are NA:
tc$road[tc$road %in% c("C", "U")] = NA
summary(is.na(tc$startLatitude)) # around 50% have no start lat
tc$is_line = !is.na(tc$startLatitude)

# Make spatial
ts = st_sfc(st_multipoint(cbind(tc$longitude, tc$latitude)))
ts = ts %>% st_cast(to = "POINT")
tc = st_sf(tc, ts, crs = 4326)
plot(tc[1]) # UK coverage


# add lines
linelist = vector(mode = "list", length = nrow(tc))

for(i in 1:length(linelist)) {
    linemat = matrix(c(tc$startLongitude[i], tc$startLatitude[i],
                       tc$finishLongitude[i], tc$finishLatitude[i]), ncol = 2, byrow = TRUE)
    linelist[[i]] = st_linestring(x = linemat)
}

tc$geom_line = st_sfc(linelist)
plot(tc$geom_line[tc$is_line])

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

# using a matrix (fast, but faster with st_join)
sel_buff_any = apply(sel_buff, 1, any)
sel_buff_which = unlist(apply(sel_buff, 1, which))
tbuff = tbuff %>% rename(aadt =all_motors_16)
wt = st_join(wt, tbuff["aadt"])

# with voronoi polygons
all_coords = rbind(st_coordinates(wt)[,1:2], st_coordinates(t1)[,1:2])
ext = raster::extent(all_coords)
v = dismo::voronoi(xy = st_coordinates(t1), ext = ext)
v_sf = as(v, "sf")
st_crs(v_sf) = st_crs(t1)
v_sf = st_join(x = v_sf, y = t1["all_motors_16"])
v_sf = v_sf %>% rename(aadt_v = all_motors_16)
wt$aadt_v = st_join(wt, v_sf["aadt_v"], FUN = mean)$aadt_v

# plot results
qtm(wt, lines.col = "aadt", lines.lwd = 30) +
  qtm(wt, lines.col = "aadt_v", lines.lwd = 10) +
  qtm(t1)

wt_out = select(wt, osm_id, aadt, aadt_v)
st_geometry(wt_out) = NULL
# write_csv(wt_out, "aadt_data_osm.csv")

# for all counters in Bristol
rm(wt)
if(exists("wt_out")) rm(wt_out)
tc_region = tc %>% filter(st_intersects(., region, sparse = F))
qtm(tc_region) # it works!
t_roads = unique(tc_region$road)
i = 23 # NA
i = 5 # for testing
for(i in seq_along(t_roads)) {
  count_num = round(i / length(t_roads) * 100)
  message(paste0(count_num), " % done")
  roadname = t_roads[i]

  # For points with no road name (2/3rds...)
  if(is.na(roadname)) {
    # spatal join with tbuff data
      t1 = filter(tc_region, is.na(road))
      tbuff = geo_buffer(shp = t1, dist = 20)
      plot(tbuff[1])
      tbuff = tbuff %>% rename(aadt = all_motors_16)
      wt = ways[tbuff,]
      wt = st_join(wt, tbuff["aadt"], FUN = mean)

  } else {

    t1 = filter(tc_region, road == roadname)
  wt = filter(ways, ref == roadname)

  tbuff = geo_buffer(shp = t1, dist = 20)
  tbuff = tbuff %>% rename(aadt = all_motors_16)
  sel_buff = st_intersects(wt, tbuff, sparse = FALSE)
  wt_all = ways[tbuff,]

  # geo link (if road name link fail)
  if(nrow(wt) == 0) {
    wt = ways[tbuff,]
    if(nrow(wt) == 0) {
      message(paste0("No road & spatial matches found for the road "), roadname)
    }
  }

  qtm(tbuff) + # plot roads for checking
    qtm(wt) +
    qtm(wt_all, lines.col = "green") # all matching

  # link by road lengths (ideal case)
  t_lines = t1$geom_line[t1$is_line]
  if(length(t_lines) == nrow(t1)) {
    line_lengths = geo_projected(t_lines, st_length)
    tbuff = geo_buffer(t_lines, dist = line_lengths / 5)
    tbuff = st_sf(t1, tbuff)
    tbuff = tbuff %>% rename(aadt = all_motors_16)
    plot(tbuff["aadt"])
    plot(wt, add = T)
    # st_within(wt, tbuff)
    wt = wt[tbuff,]
    if(nrow(wt) == 0) {
      next()
    }
      wt = st_join(wt, tbuff["aadt"], join = st_within, FUN = mean)

  } else { # otherwise use point buffers
    # spatal join with tbuff data

    wt = st_join(wt, tbuff["aadt"])
    # voronoi join
    all_coords = rbind(st_coordinates(wt)[,1:2], st_coordinates(t1)[,1:2])
    ext = raster::extent(all_coords)

    if(nrow(t1) > 1) {
      v = dismo::voronoi(xy = st_coordinates(t1), ext = ext)
      v_sf = as(v, "sf")
      st_crs(v_sf) = st_crs(t1)
      v_sf = st_join(x = v_sf, y = t1["all_motors_16"])
      v_sf = v_sf %>% rename(aadt_v = all_motors_16)
      wt$aadt_v = st_join(wt, v_sf["aadt_v"], FUN = mean)$aadt_v

      qtm(wt, lines.col = "aadt", lines.lwd = 30) +
        qtm(wt, lines.col = "aadt_v", lines.lwd = 10) +
        qtm(t1)
    }
  }
  }

  # # find roads associated with un-matched counters - not implemented
  # sel_buff_any = apply(sel_buff, 1, any)
  # sel_traf_any = apply(sel_buff, 2, any)
  # t_unmatched = tbuff[!sel_traf_any,]

  st_geometry(wt) = NULL

  if(is.null(wt$aadt_v))
    wt$aadt_v = NA

  if(i == 1) {
    wt_out = select(wt, osm_id, aadt, aadt_v)
    } else {
    wt_out = bind_rows(wt_out, select(wt, osm_id, aadt, aadt_v))
  }
}

# plot and export
ways_t = ways[ways$osm_id %in% wt_out$osm_id,]
ways_t = left_join(ways_t, wt_out)
ways_t = filter(ways_t, !is.na(aadt))
ways_t = select(ways_t, -aadt_v)

qtm(ways_t, lines.col = "aadt", lines.lwd = 30) +
  qtm(tc_region)

# save outputs
ways_t = select(ways_t, osm_id, aadt)
st_geometry(ways_t) = NULL
readr::write_csv(ways_t, "trafficcounts/trafficcounts-osm.csv")

# benchmarks

# geo_line = function(i) {
#   linemat = matrix(c(tc$startLongitude[i], tc$startLatitude[i],
#                      tc$finishLongitude[i], tc$finishLatitude[i]), ncol = 2, byrow = TRUE)
#   st_linestring(x = linemat)
# }

# system.time({
#   ll = lapply(X = 1:nrow(tc), FUN = geo_line)
# })
#
#
# identical(ll, linelist)
#
# system.time(for(i in 1:length(linelist)) {
#   linemat = matrix(c(tc$startLongitude[i], tc$startLatitude[i],
#                      tc$finishLongitude[i], tc$finishLatitude[i]), ncol = 2, byrow = TRUE)
#   linelist[[i]] = st_linestring(x = linemat)
# })
