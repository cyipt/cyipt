# Aim: create predictive model of uptake of cycling following exposure to infrastructure

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
# flow_11_orig = readRDS("~/npct/pct-outputs-regional-R/commute/msoa/avon/l.Rds") %>%
#   as(Class = "sf")
flow_11_orig = readRDS("../cyipt-bigdata/l_all.Rds") # from github/npct
o = order(rf_all_orig$id)
rf_new_order = rf_all_orig[o,]

rf11_sf = rf_new_order %>%
  st_as_sf()

ukbound = getbb("Great Britain")
# ukways = dodgr::dodgr_streetnet("Great Britain") # fail
# ukways = osmdata_sf(q = opq(bbox = "Great Britain"), doc = "../cyipt-bigdata/gb_ways.osm")

# Process lines data ----
rf11 = rf11_sf %>%
  mutate(pcycle11 = bicycle / all) %>%
  select(o = geo_code1, d = geo_code2, all11 = all, pcycle11, dist = rf_dist_km,
         hilliness = rf_avslope_perc, qdf = dist_rq_rf, car_driver) %>%
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

# road net data ----
# todo: add a bit with all ways, not just busy (40mph+ ones)
# ways_uk = ...
ways_busy_no_infra = readRDS("../cyipt-bigdata/ways_busy_no_infra.Rds") # load all intersections with fastest

# new historic data ----
td = st_read("../td/dft-england-cycling-data-2011.geojson")
td_type = st_geometry_type(td)
# summary(td_type)
# td_p = td[td_type == "POINT", ]
# summary(td_p)
# # plot(td_p$geometry)
td = td[td_type == "LINESTRING", ]
old_infra = td # rely on td data for now...
l = l_joined[l_joined$all11 >= min_od_sample & l_joined$all01 >= min_od_sample, ]
sum(l$all11) # 7 million ppl (500k when 500+, 2m when 200)

b500 = readRDS("../cyipt-bigdata/b500.Rds")
b200 = readRDS("../cyipt-bigdata/b200.Rds")
b100 = readRDS("../cyipt-bigdata/b100.Rds")

# l_joined = st_join(l_sam, b) # generates huge output - better on per-route level
## Estimate exposure per route ----
rf_b = l$geometry_rf %>%
  st_transform(27700) %>%
  st_buffer(dist = 50, nQuadSegs = 4) %>%
  st_transform(4326)
sel_infra = st_intersects(rf_b, old_infra)
sel_busy = st_intersects(rf_b, ways_busy_no_infra)
# todo: add sel_ways with all ways accross UK

# save results and work in progress ----
dir.create("../cyipt-bigdata/uptake-files/")
save(b100, b200, l, rf_b, sel_infra, sel_busy, old_infra, ways_busy_no_infra, file = "../cyipt-bigdata/uptake-files/uptake-files-all.Rds")

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
