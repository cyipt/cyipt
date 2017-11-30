# Aim: create predictive model of uptake of cycling following exposure to infrastructure
devtools::install_github("robinlovelace/ukboundaries")
library(tmap)
tmap_mode("view")
library(ukboundaries)
library(tidyverse)
library(stplanr)
region_name = "Bristol"

# read-in data ----
lads = readRDS("../cyipt-bigdata/boundaries/local_authority/local_authority.Rds") %>%
  st_transform(4326)
z_msoa = msoa2011_vsimple %>%
  select(geo_code = msoa11cd)
u_flow_11 = "https://github.com/npct/pct-outputs-national/raw/master/commute/msoa/l_all.Rds"
download.file(u_flow_11, "../cyipt-bigdata/l_all.Rds")

flow_11 = readRDS("../cyipt-bigdata/l_all.Rds") %>%
  st_as_sf()
# flow_11 = readRDS("~/npct/pct-outputs-regional-R/commute/msoa/avon/l.Rds") %>%
#   as(Class = "sf")
c_oa01 = st_read("../cyipt-inputs-official/Output_Areas_December_2001_Population_Weighted_Centroids.shp") %>%
  st_transform(4326)

aggzones = readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
aggzone = filter(aggzones, ttwa11nm == region_name)
# aggzone = st_buffer(aggzones, dist = 0) # for all of UK
aggzone = flow_11 %>%
  st_transform(27700) %>%
  st_buffer(1000, 4) %>%
  st_union() %>%
  st_transform(4326)
plot(aggzone)

# subset areal data to region and aggregate msoa-cas flows ----
c_oa01 = c_oa01[aggzone, ] # get points
z = z_msoa[c_oa01, ]
cas = cas2003_simple[c_oa01, ]

# read-in and process infra data ----
sc2sd = readRDS("../cyinfdat/sc2sd") %>%
  filter(OpenDate < "2010-12-01") %>%
  mutate(OpenDate = as.character(OpenDate)) %>%
  select(date = OpenDate, on_road = OnRoad)
# all before 2011
sl2sc = readRDS("../cyinfdat/ri_04_11_dft") %>%
  select(date = BuildYear, on_road = OnRoad)
old_infra = rbind(sc2sd, sl2sc)
summary(as.factor(old_infra$on_road))
qtm(old_infra, lines.col = "green")
b = old_infra %>%
  st_transform(27700) %>%
  st_buffer(dist = 500, nQuadSegs = 4) %>%
  st_union() %>%
  st_transform(4326)
qtm(b)

# subset lines of interest and aggregate them to cas level
selb = st_intersects(flow_11, b)
selb_logical = lengths(selb) > 0
flow_11b = flow_11[selb_logical, ]
flow_11nb = flow_11[!selb_logical, ]
sum(flow_11b$all) # 2.8 million affected
set.seed(20012011)
flow_11nb_sample = sample_n(flow_11nb, nrow(flow_11b))
flow_11 = rbind(flow_11b, flow_11nb_sample)

summary(flow_11$geo_code1 %in% z$geo_code)
f11 = select(flow_11, geo_code1, geo_code2, all, bicycle) %>%
  st_set_geometry(NULL) %>%
  filter(geo_code1 %in% z$geo_code, geo_code2 %in% z$geo_code) %>%
  filter(all > 20)

sum(f11$all) # 4.6m

# subset lines touching cycle infra -> sample
l11 = od2line(f11, )

# time-consuming...
system.time({od_11 = od_aggregate(flow = f11[1:99,], zones = z, aggzones = cas)})
# see look-up:
od_11 = od_11 %>%
  na.omit() %>%
  mutate(pcycle11 = bicycle / all) %>%
  select(o = flow_new_orig, d = flow_new_dest, all11 = all, pcycle11)

# process OD data ----
# od_01 = read_csv("../cyoddata/od_01.csv", skip = 4, col_names = F)
# names(od_01) = c("o", "d", "all", "mfh", "car", "bicycle", "foot")
# sum(od_01$all, na.rm = T) # 48.5 million
# od_01$all = od_01$all - od_01$mfh
# sum(od_01$all, na.rm = T) # 44 million
# od_01$mfh = NULL
# saveRDS(od_01, "../cyoddata/od_01.Rds")
od_01 = readRDS("../cyoddata/od_01.Rds")

cas_codes = select(cas2003_simple, ons_label, name) %>%
  st_set_geometry(NULL) %>%
  filter(!duplicated(name), name %in% od_01$o | name %in% od_01$d)
# join-on the ons labels
od_01 = inner_join(od_01, select(cas_codes, ons_label_o = ons_label, o = name))
sum(od_01$all, na.rm = T) # 44 million
od_01 = inner_join(od_01, select(cas_codes, ons_label_d = ons_label, d = name)) # removes ~20m ppl
sum(od_01$all, na.rm = T) # 44 million

od_01$o = od_01$ons_label_o
od_01$d = od_01$ons_label_d
od_01 = od_01 %>% mutate(pcycle01 = bicycle / all) %>%
  select(o, d, pcycle01, all01 = all)

od_01_region = od_01 %>%
  filter(o %in% cas$ons_label, d %in% cas$ons_label) # 6k results

summary(od_01_region$o %in% od_11$o) # test readiness to merge with 2011
od_01_region = od_01_region %>%
  filter(all01 > 10, o %in% od_11$o, d %in% od_11$d) %>%
  na.omit()

od = inner_join(od_01_region, od_11)
od = mutate(od, p_uptake = (pcycle11 - pcycle01))

l = od2line(flow = od, cas)
plot(l$geometry) # works
l$dist = as.numeric(st_length(l))
l = filter(l, dist > 0, dist < 10000) %>%
  na.omit()
qtm(l) + qtm(b)

# testing
sum(l$all01) # 100k
sum(l$all11) # many more in 2011
sum(l$all01 * l$pcycle01) / sum(l$all01)
sum(l$all11 * l$pcycle11) / sum(l$all11) # doubling in cycling in Avon in affected routes

# crude measure of exposure: % of route near 1 cycle path
i = 1
l$exposure = NA
for(i in 1:nrow(l)) {
  intersection = st_intersection(l$geometry[i], b)
  if(length(intersection) > 0) {
    l$exposure[i] = st_length(intersection) /
      st_length(l$geometry[i])
  }
}
summary(l$exposure)
sel_na = is.na(l$exposure)
plot(l$dist, l$exposure)
l$exposure[sel_na] = 0
m = lm(p_uptake ~ dist + exposure, l, weights = all11)
summary(m)
p = (predict(m, l) + l$pcycle01) * l$all11
sum(p)
psimple = l$pcycle01 * l$all11
sum(psimple) # 4000+ more cyclists estimated
cor(l$all11, p)^2
cor(l$all11, psimple)^2
cor(l$all01 * l$pcycle01, p)
cor(l$all01 * l$pcycle01, psimple)

# Estimate uptake
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
