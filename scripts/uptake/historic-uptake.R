# load packages and data ----
rm(list = ls()) # start with a blank slate
library(tmap)
tmap_mode("view")
# install.packages("caret")
library(caret)
library(tidyverse)
library(sf)

# load files
ways_intersect_wgs = readRDS("../cyipt-bigdata/uptake-files/ways_intersect_wgs.Rds")
ways_intersect_wgs$dist = as.numeric(st_length(ways_intersect_wgs))
sel_within = readRDS("../cyipt-bigdata/uptake-files/sel_within.Rds")
old_infra = readRDS("../cyipt-bigdata/uptake-files/old_infra_clean.Rds")
l = readRDS("../cyipt-bigdata/uptake-files/l.Rds")
names(l)

# descriptive stats of inputs ----
summary(l)
summary(as.factor(old_infra$cycleway))
summary(as.factor(old_infra$highway))
sum(old_infra$dist) / 1000 # 1400 km

# sanity test ----
# l$infra_lookup[1:99]
i = 993
local_infra = old_infra[l$infra_lookup[[i]],]
infra_dists = local_infra$dist
local_roads = ways_intersect_wgs[sel_within[[i]], ]

qtm(l$geometry_rfb[i]) +
  qtm(l[i, ]) +
  qtm(local_infra, lines.col = "green") +
  qtm(local_roads, lines.col = "maxspeed")

# generate exposure measures per route ----
# create table to hold results
path_pattern = "path|pedestrian|track|footway"
res_names = c("infra_length", "infra_length_prop", "infra_avwidth", "infra_date",
              "infra_cycleway.lane",
              "infra_highway.cycleway", "infra_highway.primary", "infra_highway.secondary",
              "infra_highway.tertiary", "infra_highway.residential", "infra_highway.path",
              "infra_segregated", "infra_unsegregated",
              "infra_lit", "infra_asphalt",
              "infra_maxspeed.20_or_less", "infra_maxspeed.30", "infra_maxspeed.40", "infra_maxspeed.60",
              "road_dist", "road_20", "road_30", "road_40", "road_60",
              "road_primary", "road_secondary", "road_tertiary", "road_residential",
              "road_cycleway", "road_path"
              )
res = data.frame(matrix(nrow = nrow(l), ncol = length(res_names)))
names(res) = res_names
class(res$infra_date) = "Date"
for(i in 1:nrow(l)) {
  local_infra = old_infra[l$infra_lookup[[i]],]
  infra_dists = local_infra$dist
  dist_infra = sum(infra_dists)

  res$infra_length[i] = dist_infra
  res$infra_length_prop[i] = dist_infra / (l$dist[i] * 1000)
  res$infra_avwidth[i] = weighted.mean(as.numeric(as.character(local_infra$est_width)), w = infra_dists, na.rm = T)
  res$infra_date[i] = mean(as.Date(local_infra$date))

  res$infra_cycleway.lane[i] = sum(infra_dists[local_infra$cycleway == "lane"], na.rm = T)
  res$infra_highway.cycleway[i] = sum(infra_dists[local_infra$highway == "cycleway"], na.rm = T)
  res$infra_highway.primary[i] = sum(infra_dists[local_infra$highway == "primary"], na.rm = T)
  res$infra_highway.secondary[i] = sum(infra_dists[local_infra$highway == "secondary"], na.rm = T)
  res$infra_highway.tertiary[i] = sum(infra_dists[local_infra$highway == "tertiary"], na.rm = T)
  res$infra_highway.residential[i] = sum(infra_dists[local_infra$highway == "residential"], na.rm = T)
  res$infra_highway.path[i] = sum(infra_dists[grepl("footway|path", local_infra$highway)], na.rm = T)
  res$infra_segregated[i] = sum(infra_dists[local_infra$segregated == "yes"], na.rm = T)
  res$infra_unsegregated[i] = sum(infra_dists[local_infra$segregated == "no"], na.rm = T)
  res$infra_lit[i] = sum(infra_dists[local_infra$lit == "yes"], na.rm = T)
  res$infra_asphalt[i] = sum(infra_dists[local_infra$surface == "asphalt"], na.rm = T)

  # res$road_busy[i] = sum(busy_dists)

  res$infra_maxspeed.20_or_less[i] = sum(infra_dists[local_infra$maxspeed < 21], na.rm = T)
  res$infra_maxspeed.30[i] = sum(infra_dists[local_infra$maxspeed > 21 & local_infra$maxspeed < 31], na.rm = T)
  res$infra_maxspeed.40[i] = sum(infra_dists[local_infra$maxspeed > 31 & local_infra$maxspeed < 41], na.rm = T)
  res$infra_maxspeed.60[i] = sum(infra_dists[local_infra$maxspeed > 41], na.rm = T)

#   # Along a busy (> 30 mph) road without infrastructure
  local_roads = ways_intersect_wgs[sel_within[[i]], ]
  road_dists = local_roads$dist
  dist_road = sum(road_dists)
  res$road_dist[i] = dist_road
  res$road_10[i] = sum(road_dists[local_roads$maxspeed <= 15])
  res$road_20[i] = sum(road_dists[local_roads$maxspeed > 15 & local_roads$maxspeed < 30])
  res$road_30[i] = sum(road_dists[local_roads$maxspeed == 30])
  res$road_40[i] = sum(road_dists[local_roads$maxspeed > 30 & local_roads$maxspeed <= 40])
  res$road_60[i] = sum(road_dists[local_roads$maxspeed > 40])
#
  res$road_primary[i] = sum(road_dists[grep(pattern = "primary", x = local_roads$highway)])
  res$road_secondary[i] = sum(road_dists[grep(pattern = "secondary", x = local_roads$highway)])
  res$road_tertiary[i] = sum(road_dists[grep(pattern = "tertiary", x = local_roads$highway)])
  res$road_residential[i] = sum(road_dists[grep(pattern = "resi", x = local_roads$highway)])
  res$road_cycleway[i] = sum(road_dists[grep(pattern = "cycleway", x = local_infra$highway)])

  res$road_path[i] = sum(road_dists[grep(pattern = path_pattern, x = local_infra$highway)])
#
#   # todo: add breakdown by speed for primary
}
summary(res$infra_length_prop) # on average 4% covered
res$infra_length_prop[res$infra_length_prop > 1] = 1
summary(res)
res$infra_date[is.na(res$infra_date)] = as.Date(mean(as.Date(res$infra_date), na.rm = T))
res[is.na(res)] <- 0
# sanity checks on lines ----
sum(l$all01)
sum(l$all11)
sum(l$all01 * l$pcycle01) / sum(l$all01)
sum(l$all11 * l$pcycle11) / sum(l$all11)
l = l %>% mutate(p_uptake = pcycle11 - pcycle01)
l = bind_cols(l, res)
l = select(l, -contains("geom"), -osm_lookup)
summary(l)
# summary(l$o %in% z$geo_code)
# summary(l$d %in% z$geo_code)
qtm(l[l$pcycle01 > 0.3,]) +
  qtm(l$geometry_rf[l$pcycle01 > 0.3])
qtm(l[l$pcycle11 > 0.3,])
qtm(l %>% top_n(n = 200, wt = infra_length), basemaps = c("OpenStreetMap.BlackAndWhite", "Thunderforest.OpenCycleMap"), lines.col = "black")

# remove list cols for model

psimple = l$pcycle01 * l$all11 # simple model
m_all_lm = lm(p_uptake ~ dist + infra_length + infra_length_prop + infra_avwidth + infra_date +
                infra_cycleway.lane + infra_highway.cycleway +
                infra_highway.primary + infra_highway.secondary + infra_highway.tertiary +
                infra_highway.residential + infra_highway.path +
                infra_segregated + infra_unsegregated + infra_lit + infra_asphalt +
                infra_maxspeed.20_or_less + infra_maxspeed.30 + infra_maxspeed.40 + infra_maxspeed.60
                , data = l, weights = all11)
summary(m_all_lm) # all in direction expected, e.g. avwidth etc
m_all_lm_roads = lm(p_uptake ~ dist + infra_length + infra_length_prop + infra_avwidth + infra_date +
                infra_cycleway.lane + infra_highway.cycleway +
                infra_highway.primary + infra_highway.secondary + infra_highway.tertiary +
                infra_highway.residential + infra_highway.path +
                infra_segregated + infra_unsegregated + infra_lit + infra_asphalt +
                infra_maxspeed.20_or_less + infra_maxspeed.30 + infra_maxspeed.40 + infra_maxspeed.60 +
                road_20 + road_30 + road_40 + road_60 +
                road_primary + road_secondary + road_tertiary + road_residential + road_cycleway + road_path,
                data = l, weights = all11)
summary(m_all_lm_roads) # all in direction expected, e.g. avwidth etc
m = lm(p_uptake ~
         infra_cycleway.lane + infra_highway.cycleway +
         infra_highway.primary + infra_highway.secondary + infra_highway.tertiary +
         infra_highway.residential + infra_highway.path +
         infra_maxspeed.20_or_less + infra_maxspeed.40 + infra_maxspeed.60 +
         road_40 + road_60 +
         road_cycleway,
       data = l, weights = all11)
summary(m) # infra_length not significant
m2 = lm(p_uptake ~ infra_length + all01 +
          infra_cycleway.lane + infra_highway.cycleway +
          infra_highway.primary + infra_highway.secondary + infra_highway.tertiary +
          infra_highway.residential + infra_highway.path +
          infra_maxspeed.20_or_less + infra_maxspeed.30 + infra_maxspeed.40 + infra_maxspeed.60
        , data = l, weights = all11)
summary(m2)
saveRDS(m, "m.Rds")
p_lm = predict(m, l)
p = (p_lm + l$pcycle01) * l$all11
p[is.na(p)] = 0
sum(psimple)
sum(p)
# predict uptake under scenario of change
l_new = mutate(l, infra_length = dist, infra_highway.primary = 1)
uptake_new = (predict(m, l_new) + l$pcycle01) * l$all11
sum(p, na.rm = T) / sum(l$all11)
sum(uptake_new, na.rm = T) / sum(l$all11)
sum(psimple) / sum(l$all11) # a 1% increase...

# install.packages("xgboost")
library(xgboost)
train_no_road = select(l, p_uptake, all11, dist, infra_length,
                 infra_cycleway.lane, infra_highway.cycleway,
                 infra_highway.primary, infra_highway.secondary, infra_highway.tertiary,
                 infra_highway.residential, infra_highway.path,
                 infra_maxspeed.20_or_less, infra_maxspeed.30, infra_maxspeed.40, infra_maxspeed.60) %>%
  st_set_geometry(NULL) %>%
  as.matrix()
train = select(l, p_uptake, all11,
               infra_cycleway.lane, infra_highway.cycleway,
               infra_highway.primary, infra_highway.secondary, infra_highway.tertiary,
               infra_highway.residential, infra_highway.path,
               infra_maxspeed.20_or_less, infra_maxspeed.30, infra_maxspeed.40, infra_maxspeed.60,
               road_40, road_60,
               road_cycleway) %>%
  st_set_geometry(NULL) %>%
  as.matrix()
mxg = xgboost(train[, -(1:2)], train[, 1], weight = train[, 2], nrounds = 10)
saveRDS(mxg, "m-xgboost.Rds")
summary(train)
# to test very simple scenarios of change
train_new = mutate(as_data_frame(train), infra_highway.cycleway = infra_highway.cycleway + 1) %>%
  as.matrix()
p_old = (predict(mxg, train[, -c(1:2)]) + l$pcycle01) * l$all11
p_new = (predict(mxg, train_new[, -c(1:2)]) + l$pcycle01) * l$all11
sum(p_old) / sum(l$all11)
sum(p_new) / sum(l$all11) # 2 % point increase

xgb.plot.importance(xgb.importance(feature_names = colnames(train)[-(1:2)], model = mxg))
cor(l$pcycle11 * l$all11, p)^2 # substantial improvement in model fit
cor(l$all11 * l$pcycle11, psimple)^2

# learnbayes approach
install.packages("LearnBayes")

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



