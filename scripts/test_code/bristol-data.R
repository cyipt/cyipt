# save data for bristol
# Aim: create predictive model of uptake of cycling following exposure to infrastructure
devtools::install_github("robinlovelace/ukboundaries")
library(tmap)
tmap_mode("view")
library(ukboundaries)
library(tidyverse)
library(stplanr)
region_name = "Bristol"

# read-in data ----
z = msoa2011_vsimple %>%
  select(geo_code = msoa11cd)
# if using aggregating zones
region_shape = readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
region_shape = filter(region_shape, ttwa11nm == region_name) %>%
  st_transform(27700) %>%
  st_buffer(10000) %>%
  st_transform(4326)
plot(region_shape$geometry)

# subset areal data to region and aggregate msoa-cas flows ----
z = z[region_shape, ]
# u_flow_11 = "https://github.com/npct/pct-outputs-national/raw/master/commute/msoa/l_all.Rds"
# download.file(u_flow_11, "../cyipt-bigdata/l_all.Rds")
# u_rf = "https://github.com/npct/pct-outputs-national/raw/master/commute/msoa/rf_all.Rds"
# download.file(u_rf, "../cyipt-bigdata/rf.Rds")
rf_all_orig = readRDS("../cyipt-bigdata/rf.Rds")
rf11 = rf_all_orig %>%
  st_as_sf() %>%
  mutate(pcycle11 = bicycle / all) %>%
  select(o = geo_code1, d = geo_code2, all11 = all, pcycle11) %>%
  filter(o %in% z$geo_code, d %in% z$geo_code) %>%
  filter(all11 > 20) # 130k when >20

# flow_11_orig = readRDS("~/npct/pct-outputs-regional-R/commute/msoa/avon/l.Rds") %>%
#   as(Class = "sf")
flow_11_orig = readRDS("../cyipt-bigdata/l_all.Rds")

l11 = flow_11_orig %>%
  st_as_sf() %>%
  mutate(pcycle11 = bicycle / all) %>%
  select(o = geo_code1, d = geo_code2, all11 = all, pcycle11) %>%
  filter(o %in% z$geo_code, d %in% z$geo_code) %>%
  filter(all11 > 20) # 130k when >20

# add straight line geometry to rf11 for faster default plotting
rf11$geometry_rf = rf$geometry
rf11$geometry = l11$geometry

od_01_new = readRDS("../cyoddata/od_01_new.Rds")
l_joined = left_join(rf11, od_01_new) %>%
  na.omit()

# read-in and process infra data ----
date_switch = function(d){
  d = sapply(d, switch,
             "2004/5" = "2005-01-01",
             "2005/6" = "2006-01-01",
             "2006/7" = "2007-01-01",
             "2007/8" = "2008-01-01",
             "2008/9" = "2009-01-01",
             "2009/2010" = "2010-01-01",
             "2010/2011" = "2011-01-01",
             NA
  )
  as.Date(unlist(d))
}
sc2sd = readRDS("../cyinfdat/sc2sd") %>%
  mutate(OpenDate = as.character(OpenDate)) %>%
  filter(OpenDate < "2010-12-01") %>%
  select(date = OpenDate, on_road = OnRoad) %>%
  mutate(funding = "Connect 2") %>%
  mutate(date = as.Date(date))
# all before 2011
sl2sc = readRDS("../cyinfdat/ri_04_11_dft") %>%
  select(date = BuildYear, on_road = OnRoad) %>%
  mutate(funding = "Links to schools") %>%
  mutate(date = date_switch(date))
sndft = readRDS("../cyinfdat/ri_01_11_non_dft") %>%
  select(date = OpenDate, on_road = OnRoad) %>%
  mutate(funding = "Non-DfT")
# old_infra = rbind(sc2sd, sl2sc, sndft) %>%
old_infra = rbind(sc2sd, sl2sc, sndft) %>%
  st_transform(4326)  %>%
  mutate(date = as.Date(date))
summary(as.factor(old_infra$on_road))
summary(old_infra)

o = old_infra[region_shape, ]
l = l_joined[region_shape, ]

# save the results
dir.create("bristol-case-study-data")
saveRDS(z, "bristol-case-study-data/z.Rds")
saveRDS(o, "bristol-case-study-data/o.Rds")
saveRDS(l, "bristol-case-study-data/l.Rds")
saveRDS(region_shape, "bristol-case-study-data/region_shape.Rds")
wd_old = setwd("bristol-case-study-data/")
# setwd(wd_old)

# read-in data
o = readRDS("o.Rds")
l = readRDS("l.Rds")
z = readRDS("z.Rds")
region_shape = readRDS("region_shape.Rds")


qtm(region_shape) +
  qtm(l) +
  qtm(o, lwd = 3, lines.col = "blue")


