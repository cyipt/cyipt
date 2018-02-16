# load packages and data ----
rm(list = ls()) # start with a blank slate
library(tmap)
tmap_mode("view")
# install.packages("caret")
library(caret)
library(tidyverse)
library(sf)

# load files
sel_infra = readRDS("../cyipt-bigdata/uptake-files/sel_infra.Rds")
old_infra = readRDS("../cyipt-bigdata/uptake-files/old_infra_wgs.Rds")
l = readRDS("../cyipt-bigdata/uptake-files/l.Rds")
names(l)

# descriptive stats of inputs ----
summary(l)
summary(old_infra)
sum(st_length(old_infra))

# generate exposure measures per route ----
# create table to hold results
res_names = c("infra_length", "infra_length_prop", "infra_avwidth", "infra_date",
              "infra_cycleway", "infra_primary", "infra_secondary", "infra_tertiary",
              "infra_segregated", "infra_unsegregated", "infra_residential",
              "infra_footway", "infra_shared_use_footpath", "infra_lit", "infra_asphalt",
              "infra_40", "infra_60"
              # "road_busy",
              # "road_20", "road_30", "road_40", "road_60",
              # "road_primary", "road_secondary", "road_tertiary", "road_cycleway", "road_path"
              )

res = data.frame(matrix(nrow = nrow(l), ncol = length(res_names)))
names(res) = res_names

i = 2
for(i in 1:nrow(l)) {

  local_infra = old_infra[sel_infra[[i]],]
  # local_roads_busy = ways_busy_no_infra[sel_busy[[i]],]
  # local_roads = local_roads_busy # change this line to when running on full UK dataset: local_roads = ways_uk[sel_busy[[i]],]
  # busy_dists = as.numeric(st_length(local_roads_busy))
  # road_dists = as.numeric(st_length(local_roads))
  # roads_dist = sum(road_dists)

  infra_dists = as.numeric(st_length(local_infra))
  dist_infra = sum(infra_dists)

  # qtm(rf_b[i]) +
  #   qtm(l[i, ], add = T) +
  #   qtm(local_infra, add = T, lines.col = "green") +
  #   qtm(local_roads, add = T, lines.col = "red", lines.lwd = 3)

  res$infra_length[i] = dist_infra
  res$infra_length_prop[i] = dist_infra / (l$dist[i] * 1000)
  res$infra_avwidth[i] = weighted.mean(as.numeric(as.character(local_infra$est_width)), w = infra_dists, na.rm = T)
  res$infra_date[i] = mean(local_infra$date)

  # todo: add cycle left and right
  res$infra_cycleway[i] = sum(infra_dists[local_infra$highway == "cycleway"], na.rm = T) / dist_infra
  res$infra_residential[i] = sum(infra_dists[local_infra$highway == "residential"], na.rm = T) / dist_infra
  res$infra_footway[i] = sum(infra_dists[local_infra$highway == "footway"], na.rm = T) / dist_infra
  res$infra_primary[i] = sum(infra_dists[local_infra$highway == "primary"], na.rm = T) / dist_infra
  res$infra_secondary[i] = sum(infra_dists[local_infra$highway == "secondary"], na.rm = T) / dist_infra
  res$infra_tertiary[i] = sum(infra_dists[local_infra$highway == "tertiary"], na.rm = T) / dist_infra
  res$infra_segregated[i] = sum(infra_dists[local_infra$segregated == "yes"], na.rm = T) / dist_infra
  res$infra_unsegregated[i] = sum(infra_dists[local_infra$segregated == "no"], na.rm = T) / dist_infra
  res$infra_shared_use_footpath[i] = sum(infra_dists[local_infra$segregated == "Shared Use Footpath"], na.rm = T) / dist_infra
  res$infra_lit[i] = sum(infra_dists[local_infra$lit == "yes"], na.rm = T) / dist_infra
  res$infra_asphalt[i] = sum(infra_dists[local_infra$surface == "asphalt"], na.rm = T) / dist_infra

  # res$road_busy[i] = sum(busy_dists) / (l$dist[i] * 1000)

  res$infra_40 = sum(local_infra$busy40)
  res$infra_60 = sum(local_infra$busy60)
#   # Along a busy (> 30 mph) road without infrastructure
#   res$road_20[i] = sum(road_dists[local_roads$maxspeed < 30]) / roads_dist
#   res$road_30[i] = sum(road_dists[local_roads$maxspeed == 30]) / roads_dist
#   res$road_40[i] = sum(road_dists[local_roads$maxspeed > 30 & local_roads$maxspeed <= 40]) / roads_dist
#   res$road_60[i] = sum(road_dists[local_roads$maxspeed > 40]) / roads_dist
#
#   res$road_primary[i] = sum(infra_dists[grep(pattern = "primary", x = local_infra$highway)]) / roads_dist
#   res$road_secondary[i] = sum(infra_dists[grep(pattern = "secondary", x = local_infra$highway)]) / roads_dist
#   res$road_tertiary[i] = sum(infra_dists[grep(pattern = "tertiary", x = local_infra$highway)]) / roads_dist
#   res$road_cycleway[i] = sum(infra_dists[grep(pattern = "cycleway", x = local_infra$highway)]) / roads_dist
#   res$road_path[i] = sum(infra_dists[grep(pattern = "path|pedestrian|track|footway", x = local_infra$highway)]) / roads_dist
#
#   # todo: add breakdown by speed for primary
#   # todo:
#
#   res$road_primary[i] = sum(infra_dists[grep(pattern = "primary", x = local_infra$highway)]) / roads_dist
#   res$road_primary[i] = sum(infra_dists[local_roads$highway == "secondary"]) / roads_dist
#
#   # Along B road

}
res$infra_length_prop[res$infra_length_prop > 1] = 1
summary(res)
res[is.na(res)] <- 0
# sanity checks on lines ----
sum(l$all01)
sum(l$all11)
sum(l$all01 * l$pcycle01) / sum(l$all01)
sum(l$all11 * l$pcycle11) / sum(l$all11)
l = l %>% mutate(p_uptake = pcycle11 - pcycle01) %>%
l = bind_cols(l, res)
# summary(l$o %in% z$geo_code)
# summary(l$d %in% z$geo_code)
qtm(l[l$pcycle01 > 0.3,]) +
  qtm(l$geometry_rf[l$pcycle01 > 0.3])
qtm(l[l$pcycle11 > 0.3,])
qtm(l %>% top_n(n = 200, wt = infra_length), basemaps = c("OpenStreetMap.BlackAndWhite", "Thunderforest.OpenCycleMap"), lines.col = "black")

# remove list cols for model
l = select(l, -contains("geom"), -osm_lookup)

psimple = l$pcycle01 * l$all11 # simple model
m_all_lm = lm(p_uptake ~ dist + infra_length + infra_cycleway + infra_avwidth +
                infra_primary + infra_secondary + infra_tertiary + infra_residential +
                infra_segregated + infra_unsegregated + infra_shared_use_footpath +
                infra_asphalt + infra_lit, data = l, weights = all11)
m = lm(p_uptake ~ dist * infra_length +
         infra_primary + infra_secondary + infra_residential,
         data = l, weights = all11)
m2 = lm(p_uptake ~ infra_primary + infra_secondary + infra_residential,
        data = l, weights = all11)
saveRDS(m, "m.Rds")
saveRDS(m2, "m2.Rds")
p_lm = predict(m, l)
p = (p_lm + l$pcycle01) * l$all11
p[is.na(p)] = 0
summary(m_all_lm)
summary(m)
summary(m2)
sum(psimple)
sum(p)
# predict uptake under scenario of change
l_new = mutate(l, infra_length = dist, infra_primary = 1, infra_prop = 1)
uptake_new = (predict(m, l_new) + l$pcycle01) * l$all11
sum(uptake_new) / sum(l$all11)
sum(psimple) / sum(l$all11) # a 1% increase...
sum(uptake_new) / sum(l$all11)

# install.packages("xgboost")
library(xgboost)
train = select(l, p_uptake, all11, infra_length, infra_length_prop, infra_cycleway,
               infra_primary, infra_secondary, infra_tertiary, infra_residential
               # ,infra_footway, infra_shared_use_footpath, infra_segregated, infra_unsegregated
               ) %>%
  st_set_geometry(NULL) %>%
  as.matrix()
mxg = xgboost(train[, -(1:2)], train[, 1], weight = train[, 2], nrounds = 10)
saveRDS(mxg, "m-xgboost.Rds")
p_perc = predict(mxg, train[, -c(1:2)])
p = (p_perc + l$pcycle01) * l$all11
summary(train)
# to test very simple scenarios of change
train_new = select(l_new, p_uptake, all11, infra_length, infra_length_prop, infra_cycleway,
                   infra_primary, infra_secondary, infra_tertiary, infra_residential
                   # ,infra_footway, infra_shared_use_footpath, infra_segregated, infra_unsegregated
                   ) %>%
  st_set_geometry(NULL) %>%
  as.matrix()
p_new = (predict(mxg, train_new[, -c(1:2)]) + l$pcycle01) * l$all11
sum(psimple)
sum(p) / sum(l$all11)
sum(p_new) / sum(l$all11) # 2 % point increase

xgb.plot.importance(xgb.importance(feature_names = colnames(train)[-(1:2)], model = mxg))
cor(l$pcycle11 * l$all11, p)^2
cor(l$all11 * l$pcycle11, psimple)^2

# Estimate uptake ----
schemes = readRDS("../cyipt-bigdata/osm-prep/Bristol/schemes.Rds")
plot(schemes$geometry[schemes$group_id == 1])
sum(st_length(schemes$geometry[schemes$group_id == 1]))
b_scheme = schemes %>%
  st_buffer(nQuadSegs = 8, dist = 500) %>%
  group_by(group_id) %>%
  summarise(length = sum(length), costTotal = sum(costTotal)) %>%
  st_union(by_feature = T) %>%
  st_transform(4326)
summary(b_scheme)
# run model on modified interventions
l$scheme_number = NA
b_scheme$uptake = NA
j = 1
for(j in seq_along(b_scheme$group_id)) {
  intersection = st_intersection(l, b_scheme[j, ])
  intersection$exposure = as.numeric(st_length(intersection)) / intersection$dist
  b_scheme$uptake[j] = sum(predict(m, intersection) * intersection$all11)
}
plot(b_scheme$length, b_scheme$uptake)
sum(b_scheme$uptake) / sum(l$all11)
b_scheme$bcr = b_scheme$uptake / (b_scheme$costTotal / 1000)
summary(b_scheme$bcr)
qtm(filter(b_scheme, bcr > 1)) # schemes with > 1 person cycling per £1k spend...

# next steps
# 1. Length of exposure by Type of infra: Cycle track (off road), Cycle Lane, Cycle street (on road)
# 2. Hilliness 3. Distance (before/after: for phase)
# 4. Add change in n. commutes
# 5. Background
# Gender and age of origin
# Backgroud level of cycling



