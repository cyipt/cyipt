# Aim: load data
# required packages ----
rm(list = ls()) # start with a blank slate
devtools::install_github("robinlovelace/ukboundaries")
library(tmap)
tmap_mode("view")
library(ukboundaries)
library(tidyverse)
library(stplanr)
library(osmdata)
library(sf)

# Parameters ----
# region_name = "Bristol" # for doing locally (not used)
min_od_sample = 200 # lower bound to subset routes (for testing)


# read-in data ----
z = msoa2011_vsimple %>%
  select(geo_code = msoa11cd)
# if using aggregating zones
region_shape = readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
# region_shape = filter(region_shape, ttwa11nm == region_name)
region_shape = st_buffer(region_shape, dist = 0) # for all of UK
qtm(region_shape)

# subset areal data to region and aggregate msoa-cas flows ----
z = z[region_shape, ]
# u_flow_11 = "https://github.com/npct/pct-outputs-national/raw/master/commute/msoa/l_all.Rds"
# download.file(u_flow_11, "../cyipt-bigdata/l_all.Rds")
# u_rf = "https://github.com/npct/pct-outputs-national/raw/master/commute/msoa/rf_all.Rds"
# download.file(u_rf, "../cyipt-bigdata/rf.Rds")
rf_all_orig = readRDS("../cyipt-bigdata/rf.Rds")
rf_all_orig = rf_new_order %>%
  st_as_sf()
# join osm lookup list to results ----
# rf = readRDS("../cyipt-securedata/uptakemodel/routes01_11.Rds") # not used - same as l$geometry_rf
# names(rf)
# rf_all_orig = readRDS("../cyipt-bigdata/rf.Rds")
# cor(rf$all11, rf_all_orig$all)
lines_lookup = readRDS("../cyipt-securedata/uptakemodel/osm_rf_inter.Rds")
rf_all_orig$osm_lookup = lines_lookup
# flow_11_orig = readRDS("~/npct/pct-outputs-regional-R/commute/msoa/avon/l.Rds") %>%
#   as(Class = "sf")
flow_11_orig = readRDS("../cyipt-bigdata/l_all.Rds") # from github/npct
o = order(rf_all_orig$id)
rf_new_order = rf_all_orig[o,]
saveRDS(rf_new_order, "../cyipt-bigdata/uptake-files/rf_new_order.Rds")

ukbound = getbb("Great Britain")
# ukways = dodgr::dodgr_streetnet("Great Britain") # fail
# ukways = osmdata_sf(q = opq(bbox = "Great Britain"), doc = "../cyipt-bigdata/gb_ways.osm")

# Process lines data ----
rf11 = rf_new_order %>%
  mutate(pcycle11 = bicycle / all) %>%
  select(o = geo_code1, d = geo_code2, all11 = all, pcycle11, dist = rf_dist_km,
         hilliness = rf_avslope_perc, qdf = dist_rq_rf, car_driver, osm_lookup) %>%
  filter(o %in% z$geo_code, d %in% z$geo_code) %>%
  filter(all11 > 20) %>% # 130k when >20
  mutate(pcar = car_driver / all11) %>%
  select(-car_driver)

l11 = flow_11_orig %>%
  st_as_sf() %>%
  mutate(pcycle11 = bicycle / all) %>%
  select(o = geo_code1, d = geo_code2, all11 = all, pcycle11) %>%
  filter(o %in% z$geo_code, d %in% z$geo_code) %>%
  filter(all11 > 20) # 130k when >20

# add straight line geometry to rf11 for faster default plotting
rf11$geometry_rf = rf11$geometry
rf11$geometry = l11$geometry

od_01_new = readRDS("../cyoddata/od_01_new.Rds")
l_joined = left_join(rf11, od_01_new) %>%
  na.omit()
saveRDS(l_joined, "../cyipt-bigdata/uptake-files/l_joined.Rds")
l = l_joined[l_joined$all11 >= min_od_sample & l_joined$all01 >= min_od_sample, ]
sum(l$all11) # 7 million ppl (500k when 500+, 2m when 200)
# check osm data
qtm(l$geometry[1]) +
  qtm(ways_uk$geometry[l$osm_lookup[[1]]])
saveRDS(l, "../cyipt-bigdata/uptake-files/l.Rds")

# road net and old-infra data ----
# td = st_read("../td/dft-england-cycling-data-2011.geojson")
# td_type = st_geometry_type(td)
# summary(td_type)
# td_p = td[td_type == "POINT", ]
# summary(td_p)
# # plot(td_p$geometry)
# td = td[td_type == "LINESTRING", ]
# old_infra = td # rely on td data for now...
# todo: add a bit with all ways, not just busy (40mph+ ones)
# ways_uk = ...
ways_busy_no_infra = readRDS("../cyipt-bigdata/ways_busy_no_infra.Rds") # load all intersections with fastest
# read-in data generate by uptake_2001_2011.R

ways_uk <- readRDS("../cyipt-securedata/uptakemodel/osm_clean.Rds")
ways_busy = filter(ways_uk, maxspeed > 30)

old_infra_all = readRDS("../cyipt-securedata/uptakemodel/infra_historic.Rds") # supercedes previous version
old_infra = filter(old_infra_all, date > "2001-01-01", date < "2011-01-01")
# old_infra1 = old_infra[1:999, ]
old_infra_buffer = st_buffer(old_infra, 10, nQuadSegs = 4) # use old_infra1 for testing
ways_within = ways_busy[old_infra_buffer, , op = st_within] # excludes crossing busy roads
old_infra$busy_lookup = st_contains(old_infra_buffer, ways_busy) # excludes crossing busy roads
qtm(ways_within, basemaps = c("Thunderforest.OpenCycleMap", "OpenStreetMap.BlackAndWhite"))
saveRDS(ways_within, "../cyipt-securedata/uptakemodel/ways_within.Rds")

# add new busy routes data to old_infra
busy_colnames = paste0("busy", c(40, 60))
old_infra[busy_colnames] = NA
for(i in 1:nrow(old_infra)) {
  # test: works
  # ways_local = ways_busy[old_infra$busy_lookup[[220]], ]
  # plot(ways_local$geometry)
  # plot(old_infra$geometry[220], add = T)
  ways_local = ways_busy[old_infra$busy_lookup[[i]], ]
  dists_local = as.numeric(st_distance(ways_local$geometry))
  old_infra$busy40[i] = sum(dists_local[ways_local$maxspeed == 40])
  old_infra$busy60[i] = sum(dists_local[ways_local$maxspeed == 60])
}
summary(old_infra$busy40)
summary(old_infra$busy60)
old_infra_wgs = st_transform(old_infra, 4326)
saveRDS(old_infra_wgs, "../cyipt-securedata/uptakemodel/old_infra_wgs.Rds")

# b1000 = old_infra %>%
#   st_transform(27700) %>%
#   st_buffer(dist = 1000, nQuadSegs = 4) %>%
#   st_transform(4326) # time consuming
# saveRDS(b1000, "../cyipt-bigdata/b1000.Rds")
# b500 = old_infra %>%
#   st_transform(27700) %>%
#   st_buffer(dist = 500, nQuadSegs = 2) %>%
#   st_transform(4326) # time consuming
# saveRDS(b500, "../cyipt-bigdata/b500.Rds")
# b200 = old_infra %>%
#   st_transform(27700) %>%
#   st_buffer(dist = 200, nQuadSegs = 2) %>%
#   st_transform(4326) # time consuming
# saveRDS(b200, "../cyipt-bigdata/b200.Rds")
# b100 = old_infra %>%
#   st_transform(27700) %>%
#   st_buffer(dist = 100, nQuadSegs = 2) %>%
#   st_transform(4326) # time consuming
# saveRDS(b100, "../cyipt-bigdata/b100.Rds")
# b50 = old_infra %>%
#   st_transform(27700) %>%
#   st_buffer(dist = 100, nQuadSegs = 2) %>%
#   st_transform(4326) # time consuming
# saveRDS(b50, "../cyipt-bigdata/b50.Rds")
b500 = readRDS("../cyipt-bigdata/b500.Rds")
b200 = readRDS("../cyipt-bigdata/b200.Rds")
b100 = readRDS("../cyipt-bigdata/b100.Rds")
b50 = readRDS("../cyipt-bigdata/b50.Rds")

# l_joined = st_join(l_sam, b) # generates huge output - better on per-route level
## Estimate exposure per route ----
rf_b = b50
# td_near_routes = st_intersection(td, rf_b) # to clip td (todo and test model if time allows)

sel_infra = st_contains(rf_b, old_infra_wgs) # tried initially with intersects...
saveRDS(sel_infra, "../cyipt-bigdata/uptake-files/sel_infra.Rds")
# sel_busy = st_intersects(rf_b, ways_busy_no_infra)
# todo: add sel_ways with all ways accross UK

# save results and work in progress ----
dir.create("../cyipt-bigdata/uptake-files/")
# save(b100, b200, l, rf_b, sel_infra, sel_busy, old_infra, ways_busy_no_infra, file = "../cyipt-bigdata/uptake-files/uptake-files-all.Rds")

# # Function to Aggregate and clean cycleway tags
# # take the highest form
# aggregate.cycleway <- function(left,right,oneside,otherside){
#   if("track" %in% c(left , right, oneside, otherside)){
#     res <- "track"
#   }else if("lane" %in% c(left , right, oneside, otherside)){
#     res <- "lane"
#   }else if("share_busway" %in% c(left , right, oneside, otherside)){
#     res <- "share_busway"
#   }else{
#     res <- "none"
#   }
#   return(res)
# }
#
# ways_busy_no_infra$cycleway.alltype
# foo <- mapply(aggregate.cycleway,
#               left = old_infra$cycleway.left,
#               right = old_infra$cycleway.right,
#               oneside = old_infra$cycleway.oneside,
#               otherside = old_infra$cycleway.otherside,
#               SIMPLIFY = T)
# summary(as.factor(foo))



# communicate:
# file.copy("scripts/benefits/historic-uptake.R", "./", overwrite = T)
# knitr::spin("historic-uptake.R")
# file.remove("historic-uptake.R")
# m = lm(pcycle11 ~ pcycle01, data = od, weights = all11)
# plot(od$all11 * predict(m, od), od$all11 * od$pcycle11)
# cor(od$all11 * predict(m, od), od$all11 * od$pcycle11, use = "complete.obs")^2 # 2001 level explains 81% of cycling in 2011!
#
# # test data
# summary(as.factor(ways$cycleway.left))
# ways = remove_cycle_infra(ways)
# summary # 200 cycle paths removed
#
# # imagine all infrastructure is new...
# # lines most exposed to new infrastructure (within a 1km buffer around them)
# l_buf = geo_buffer(l, dist = 1000) # looks good
# st_crs(l_buf)
# cpaths = st_transform(cpaths, 4326)
# cpaths$length = as.numeric(st_length(cpaths))
# l$cpath_length_buff = aggregate(cpaths["length"], l_buf, sum)$length
# l$cpath_length_buff[is.na(l$cpath_length_buff)] = 0
# summary(l)
# plot(l$cpath_length_buff, l$uptake)
# cor(l$cpath_length_buff, l$uptake, use = "complete.obs")
# m1 = lm(formula = uptake ~ cpath_length_buff, data = l, weights = all11)
#
# cor(predict(m1, l) * l$all, l$cpath_length_buff)^2

# # subset those with high flows and high exposure
# l_sub = l %>% filter(all11 > median(all11)) %>%
#   filter(cpath_length_buff > median(.$cpath_length_buff))
# r = line2route(l_sub)
#
# # incongruence example
# st_crs(lads)
# st_intersects(z, lads[lads$lad16nm == "Bristol, ",])
# see look-up:
# u_lookup = "https://opendata.arcgis.com/datasets/65544c20a5804677a2594fe750bf4482_0.csv"
# download.file(u_lookup, "../cyipt-inputs-official/ward-lookup.csv")
# lookup = read_csv("../cyipt-inputs-official/ward-lookup.csv")
# process OD data ----
# od_01 = read_csv("../cyoddata/od_01.csv", skip = 4, col_names = F)
# names(od_01) = c("o", "d", "all", "mfh", "car", "bicycle", "foot")
# sum(od_01$all, na.rm = T) # 48.5 million
# od_01$all = od_01$all - od_01$mfh
# sum(od_01$all, na.rm = T) # 44 million
# od_01$mfh = NULL
# saveRDS(od_01, "../cyoddata/od_01.Rds")
# od_01 = readRDS("../cyoddata/od_01.Rds")
# cas_codes = select(cas2003_simple, ons_label, name) %>%
#   st_set_geometry(NULL) %>%
#   filter(!duplicated(name), name %in% od_01$o | name %in% od_01$d)
# # join-on the ons labels
# od_01 = read_csv("../cyoddata/msoa_01_11_work.csv", skip = 4, col_names = F) # 12 million OD pairs
# names(od_01) = c("o", "d", "all", "mfh", "underground", "train", "bus", "taxi",
#                  "car_driver", "car_passenger", "motorcycle", "bicycle", "foot", "other")
# barplot(colSums(od_01[3:ncol(od_01)], na.rm = T))
# od_01$all = od_01$all - od_01$mfh
# od_01 = select(od_01, o, d, all, bicycle)
# saveRDS(od_01, "../cyoddata/od_01.Rds")
# od_01 = readRDS("../cyoddata/od_01.Rds")
# lookup_msoa = read_csv("../cyipt-inputs-official/MSOA01_MSOA11_LAD11_EW_LU.csv")
# summary(od_01$o %in% lookup_msoa$MSOA01NM)
# summary(od_01$o %in% lookup_msoa$MSOA11NM) # ~ 9% not 1-to-1 fit
# lookup_msoa$o = lookup_msoa$MSOA01NM
# lookup_msoa$d = lookup_msoa$MSOA01NM
# lookup_msoa$o11 = lookup_msoa$MSOA11CD
# lookup_msoa$d11 = lookup_msoa$MSOA11CD
# od_01_j1 = left_join(od_01, select(lookup_msoa, o, o11))
# od_01_j2 = inner_join(od_01_j1, select(lookup_msoa, d, d11))
# od_01_new = od_01_j2 %>%
#   select(o = o11, d = d11, all, bicycle) %>%
#   filter(all > 0) %>%
#   group_by(o, d) %>%
#   summarise(all = sum(all), bicycle = sum(bicycle)) %>%
#   mutate(pcycle01 = bicycle / all) %>%
#   select(o, d, all01 = all, pcycle01)
# saveRDS(od_01_new, "../cyoddata/od_01_new.Rds")

# nrow(rf_all_orig)
# nrow(flow_11_orig)
# qtm(flow_11_orig[399999,]) +
#   qtm(rf_all_orig[399999,]) # they're not the same
# summary(flow_11_orig$id == rf_all_orig$id)
# m = rf_all_orig$id %in% flow_11_orig$id
# m = flow_11_orig$id %in% rf_all_orig$id
# summary(m)
# summary(duplicated(rf_all_orig$id))
# summary(duplicated(flow_11_orig$id))
# identical(order(flow_11_orig$id), order(rf_all_orig$id))
# plot(order(flow_11_orig$id))
# plot(order(rf_all_orig$id))
#
# summary(flow_11_orig$id == rf_new_order$id)
# qtm(flow_11_orig[399999,]) +
#   qtm(rf_new_order[399999,]) # they're not the same
# # process old historic infra data ----
# read-in and process infra data ----
# date_switch = function(d){
#   d = sapply(d, switch,
#              "2004/5" = "2005-01-01",
#              "2005/6" = "2006-01-01",
#              "2006/7" = "2007-01-01",
#              "2007/8" = "2008-01-01",
#              "2008/9" = "2009-01-01",
#              "2009/2010" = "2010-01-01",
#              "2010/2011" = "2011-01-01",
#              NA
#   )
#   as.Date(unlist(d))
# }
# sc2sd = readRDS("../cyinfdat/sc2sd") %>%
#   mutate(OpenDate = as.character(OpenDate)) %>%
#   filter(OpenDate < "2010-12-01") %>%
#   select(date = OpenDate, on_road = OnRoad) %>%
#   mutate(funding = "Connect 2") %>%
#   mutate(date = as.Date(date))
# # all before 2011
# sl2sc = readRDS("../cyinfdat/ri_04_11_dft") %>%
#   select(date = BuildYear, on_road = OnRoad) %>%
#   mutate(funding = "Links to schools") %>%
#   mutate(date = date_switch(date))
# sndft = readRDS("../cyinfdat/ri_01_11_non_dft") %>%
#   select(date = OpenDate, on_road = OnRoad) %>%
#   mutate(funding = "Non-DfT")
# # old_infra = rbind(sc2sd, sl2sc, sndft) %>%
# old_infra = rbind(sc2sd, sl2sc, sndft) %>%
#   st_transform(4326)  %>%
#   mutate(date = as.Date(date))
# summary(as.factor(old_infra$on_road))
# summary(old_infra$date)
# # qtm(old_infra, lines.col = "green")
#
# old_infra$years_complete = as.numeric(lubridate::ymd("2011-03-27") - old_infra$date) / 365
# summary(old_infra$years_complete)
# old_infra = old_infra %>% filter(years_complete < 10) # removed a few thousand schemes
# b = old_infra %>%
#   st_transform(27700) %>%
#   st_buffer(dist = 1000, nQuadSegs = 4) %>%
#   st_union() %>%
#   st_transform(4326)
# qtm(b)
# subset lines of interest and aggregate them to cas level
# selb = st_intersects(l_joined, b) # joining *all* lines
# saveRDS(selb, "selb.Rds")
# selb_logical = lengths(selb) > 0
# l = l_joined[selb_logical, ]
# lnb = l_joined[!selb_logical, ]
# set.seed(20012011)
# lnb_sample = sample_n(lnb, nrow(l))
# l = rbind(l, lnb_sample)
# crude measure of exposure: % of route near 1 cycle path: old measure of exposure ----
# for(i in 1:nrow(l)) {
#   intersection = st_intersection(l$geometry[i], b)
#   if(length(intersection) > 0) {
#     l$exposure[i] = st_length(intersection) /
#       st_length(l$geometry[i])
#   }
# }
# sel_na = is.na(l$exposure)
# l$exposure[sel_na] = 0
# l$dist = as.numeric(st_length(l))
# plot(l$dist, l$exposure)
# summary(l$exposure)
# m = lm(p_uptake ~ dist + exposure, l, weights = all11)
# p = (predict(m, l) + l$pcycle01) * l$all11
# add new variables to old infra data ----
# way1 = readRDS("../cyipt-bigdata/osm-prep/Andover/osm-lines.Rds")
# plot(way1$geometry)
# dirs = file.path(list.dirs("../cyipt-bigdata/osm-prep/"), "osm-lines.Rds")
# ways <- list()
# i = dirs[3]
# for(i in dirs[-c(1, 2)]) {
#   way1 = readRDS(i)
#   ways[[i]] <- way1
#   print(i)
# }
# ways_all = bind_rows(ways)
# ways_all = as.data.frame(ways_all)
# ways_all$geometry <- st_sfc(ways_all$geometry)
# ways_all = st_sf(ways_all)
# st_crs(ways_all) = 27700
# saveRDS(ways_all, "../cyipt-bigdata/ways_all.Rds")
# ways_all = readRDS("../cyipt-bigdata/ways_all.Rds")
# summary(as.factor(ways_all$highway))
# summary(as.factor(ways_all$cycleway.left))
# summary(as.factor(ways_all$maxspeed ))
# summary(as.factor(ways_all$cycleway.right ))
# summary(as.factor(ways_all$region))
# ways_cycling = ways_all %>%
#   filter(highway %in% c("cycleway", "primary", "secondary") |
#            cycleway.left != "no" |
#            cycleway.right != "no" |
#            maxspeed == 20)
# london = osmdata::getbb(place_name = "london", format_out = "sf_polygon")
# mapview::mapview(london)
# st_crs(london)
# st_crs(ways_all)
# ways_london = st_transform(ways_cycling, 4326)
# ways_london = ways_london[london, ]
# pryr::object_size(ways_london)
# saveRDS(ways_london, "/tmp/ways_london.Rds")
# zip(zipfile = "ways_london.zip", files = "/tmp/ways_london.Rds")
# summary(ways_all$maxspeed)
# summary(as.factor(ways_all$roadtype))
# ways_busy = filter(ways_all, maxspeed > 30 & highway != "pedestrian")
# ways_busy$length = as.numeric(st_length(ways_busy))
# summary(ways_busy)
# # mapview::mapview(ways_busy[1:2000, ]) # inspect, find which ones to clean
# ways_busy = filter(ways_busy, length > 50)
# ways_busy = rmapshaper::ms_simplify(ways_busy)
# ways_busy_wgs = st_transform(ways_busy, 4326)
#
# st_write(ways_busy_wgs, "../cyipt-bigdata/ways_busy_wgs.geojson")
#
# b200 = readRDS("../cyipt-bigdata/b200.Rds")
# b200u = st_union(b200) # 10 times smaller object.size(x = b200u) / object.size(b200$geometry)
# # ways_busy_no_infra = st_difference(ways_busy_wgs, b200) # takes forever
# ways_busy_no_infra <- st_difference(ways_busy_simple, b200u)
# saveRDS(ways_busy_no_infra, "../cyipt-bigdata/ways_busy_no_infra.Rds")
# # ways_busy_no_infra = ways_busy_wgs[b200u, , op = st_disjoint]
#
#
# ways_busy_leeds = ways_busy_simple[leeds, ]
# ways_busy_no_infra_leeds = ways_busy_no_infra[leeds, ]
# old_infra_leeds = old_infra[leeds, ]
#
# qtm(ways_busy_no_infra_leeds, lines.lwd = 4) +
#   qtm(ways_busy_leeds, lines.col = "green") +
#   qtm(old_infra_leeds, lines.col = "black")
#
#
# qtm(ways_busy_leeds)
# cambridge = region_shape %>% filter(grepl("Bris", ttwa11nm))
# ways_busy_cambridge = ways_busy_simple[cambridge, ]
# ways_busy_no_infra_cambridge = ways_busy_no_infra[cambridge, ]
# old_infra_cam = old_infra[cambridge, ]
# qtm(ways_busy_leeds, lines.col = "green", lines.lwd = 3) +
#   qtm(ways_busy_no_infra_cambridge) +
#   qtm(old_infra_cam, lines.col = "black")
# infrastructure on the ground between mid 2008 to 2011
# u_td = "https://github.com/cyclestreets/dft-england-cycling-data-2011/archive/master.zip"
# download.file(u_td, "td.zip")
# unzip("td.zip", exdir = "..")
# system("mv ../dft-england-cycling-data-2011-master ../td") # move to td repo
# untar("../td/dft-england-cycling-data-2011_formatted.geojson.gz")
#
# l = purrr::map(dirs[-1] ~
#                  readRDS(.)
# )
# # new measure
# i = 79
# l$exposure = NA
# line_exposure = function(rf_b, old_infra) {
#   sel_infra = st_intersects(rf_b, old_infra)
#   n_lines = ifelse(is(rf_b, "sf"), nrow(rf_b), length(rf_b))
#   res = data.frame(
#     id = rep(0, n_lines),
#     length_on_road = rep(0, n_lines),
#     length_off_road = rep(0, n_lines),
#     years_complete = rep(0, n_lines)
#   )
#   for(i in seq_len(n_lines)) {
#     # res$id[i] = rf_b$id[i]
#     if(length(sel_infra[[i]]) == 0) {
#       next()
#     } else {
#       # res$id[i] = rf_b$id[i]
#       intersection = st_intersection(old_infra[sel_infra[[i]], ], rf_b[i])
#       res$length_on_road[i] = as.numeric(sum(st_length(intersection[intersection$on_road == "t", ])))
#       res$length_off_road[i] = as.numeric(sum(st_length(intersection[intersection$on_road == "f", ])))
#       res$years_complete[i] = mean(intersection$years_complete)
#     }
#   }
#   res
# }
# res_exp = line_exposure(rf_b, old_infra)
# res_prop = res_exp %>%
#   mutate(length_on_road = length_on_road / l$dist, length_off_road = length_off_road / l$dist) / 1000
# summary(res_prop)
# l$id = paste(l$o, l$d)
# l_new = l
# l_new$length_on_road = res_prop$length_on_road
# l_new$length_off_road = res_prop$length_off_road
# l_new$length_on_road_sq = res_prop$length_on_road^2
# l_new$length_off_road_sq = res_prop$length_off_road^2
# l_new$years_complete = res_prop$years_complete
# l_new$dist_sq = l_new$dist^2
# summary(l_new)
# psimple = l$pcycle01 * l$all11 # simple model
# m = lm(p_uptake ~ dist + length_off_road +
#          length_on_road_sq + length_off_road_sq + years_complete
#        + pcar, data = l_new, weights = all11)
# saveRDS(m, "m.Rds")
# p = (predict(m, l_new) + l$pcycle01) * l$all11
# p[is.na(p)] = 0
# summary(m)
# summary(l_new)
# sum(psimple)
# sum(p)
# # install.packages("xgboost")
# library(xgboost)
# train = select(l_new, p_uptake, all11, dist, length_on_road, length_off_road, years_complete) %>%
#   sample_frac(0.5) %>%
#   st_set_geometry(NULL) %>%
#   as.matrix()
# m = xgboost(train[, -(1:2)], train[, 1], weight = train[, 2], nrounds = 10)
# lx = select(l_new, dist, length_on_road, length_off_road, years_complete) %>%
#   st_set_geometry(NULL) %>%
#   as.matrix()
# p = (predict(m, lx) + l$pcycle01) * l$all11
# xgb.plot.importance(xgb.importance(feature_names = colnames(lx), model = m))
# sum(p)
# sum(psimple) # 4000+ more cyclists estimated
# cor(l$pcycle11 * l$all11, p)^2
# cor(l$all11 * l$pcycle11, psimple)^2
