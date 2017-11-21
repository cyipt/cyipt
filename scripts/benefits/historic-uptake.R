# get cas2003 OD data
devtools::install_github("robinlovelace/ukboundaries")
library(tmap)
tmap_mode("view")
library(ukboundaries)
library(tidyverse)
library(stplanr)
region_name = "Bristol"

# read-in data ----
aggzones = readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
aggzone = filter(aggzones, ttwa11nm == region_name)
z_msoa = st_read("../cyipt-inputs-official/Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales.shp") %>%
  st_transform(4326) %>%
  select(geo_code = msoa11cd)
flow_11 = readRDS("~/npct/pct-outputs-regional-R/commute/msoa/avon/l.Rds") %>%
  as(Class = "sf")
c_oa01 = st_read("../cyipt-inputs-official/Output_Areas_December_2001_Population_Weighted_Centroids.shp") %>%
  st_transform(4326)

# subset areal data to region and aggregate msoa-cas flows ----
c_oa01 = c_oa01[aggzone, ] # get points
z = z_msoa[c_oa01, ]
cas = cas[c_oa01, ]
cas = cas2003_simple[c_oa01, ]

f11 = select(flow_11, geo_code1, geo_code2, all, bicycle) %>%
  st_set_geometry(NULL) %>%
  filter(geo_code1 %in% z$geo_code, geo_code2 %in% z$geo_code)
l11 = od_aggregate(flow = f11, zones = z, aggzones = cas) %>%
  na.omit() %>%
  mutate(pcycle11 = bicycle / all) %>%
  select(o = flow_new_orig, d = flow_new_dest, all11 = all, pcycle11)

# process OD data ----
od_01 = read_csv("../cyoddata/wicid_output.csv", skip = 4, col_names = F)
names(od_01) = c("o", "d", "all", "mfh", "car", "bicycle", "foot")
sum(od_01$all, na.rm = T) # 48.5 million
od_01$all = od_01$all - od_01$mfh
sum(od_01$all, na.rm = T) # 44 million
od_01$mfh = NULL

cas_codes = select(cas2003_simple, ons_label, name) %>%
  st_set_geometry(NULL) %>%
  filter(!duplicated(name), name %in% od_01$o | name %in% od_01$d)
# join-on the ons labels
od_01 = inner_join(od_01, select(cas_codes, ons_label_o = ons_label, o = name))
od_01 = inner_join(od_01, select(cas_codes, ons_label_d = ons_label, d = name)) # removes ~20m ppl
sum(od_01$all, na.rm = T) # 44 million

od_01$o = od_01$ons_label_o
od_01$d = od_01$ons_label_d
od_01 = od_01 %>% mutate(pcycle01 = bicycle / all) %>%
  select(o, d, pcycle01, all01 = all)

od_01_region = od_01 %>%
  filter(o %in% cas$ons_label, d %in% cas$ons_label) # 6k results

summary(od_01_region$o %in% l11$o) # test readiness to merge with 2011
od_01_region = inner_join(od_01_region, l11)
od_01_region = od_01_region %>% select(o, d, all01 = all)
sum(od_01_region$all) # 100k

remove_cycle_infra = function(ways) {
  ways$cycleway.left = "no"
  ways$cycleway.right = "no"
  return(ways)
}

# test data
ways = readRDS("~/cyipt/cyipt-bigdata/osm-clean/BristolCityof/osm-lines.Rds")
cpaths = ways %>% filter(cycleway.left != "no") # 21km cycle paths...
summary(as.factor(ways$cycleway.left))
ways = remove_cycle_infra(ways)
summary # 200 cycle paths removed

# get uptake data from get-nomis data
# sustrans data - read-in from cyinfdat
sc2sd = readRDS("../cyinfdat/sc2sd")
i = readRDS("../cyinfdat/ri_04_11_dft")
qtm(l) +
  qtm(aggzone) +
  qtm(sc2sd, "green") +
  qtm(i) + # very little infrastructure there
  qtm(cpaths)

# imagine all infrastructure is new...
# lines most exposed to new infrastructure (within a 1km buffer around them)
l_buf = geo_buffer(l, dist = 1000) # looks good
st_crs(l_buf)
cpaths = st_transform(cpaths, 4326)
cpaths$length = as.numeric(st_length(cpaths))
l$cpath_length_buff = aggregate(cpaths["length"], l_buf, sum)$length
l$cpath_length_buff[is.na(l$cpath_length_buff)] = 0
summary(l)
plot(l$cpath_length_buff, l$uptake)
cor(l$cpath_length_buff, l$uptake, use = "complete.obs")
m1 = lm(formula = uptake ~ cpath_length_buff, data = l, weights = all11)

cor(predict(m1, l) * l$all, l$cpath_length_buff)^2

# subset those with high flows and high exposure
l_sub = l %>% filter(all11 > median(all11)) %>%
  filter(cpath_length_buff > median(.$cpath_length_buff))
r = line2route(l_sub)
