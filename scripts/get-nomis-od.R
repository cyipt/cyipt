# get cas2003 OD data
devtools::install_github("robinlovelace/ukboundaries")
library(tmap)
tmap_mode("view")
library(ukboundaries)
library(tidyverse)
library(stplanr)
region_name = "Bristol"

aggzones = readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
aggzone = filter(aggzones, ttwa11nm == region_name)
c_oa01 = st_read("../cyipt-inputs-official/Output_Areas_December_2001_Population_Weighted_Centroids.shp") %>%
  st_transform(4326)
flow_11 = readRDS("~/npct/pct-outputs-regional-R/commute/msoa/avon/l.Rds") %>%
  as(Class = "sf")
cas = cas2003_simple[c_oa01, ]
z_msoa = st_read("../cyipt-inputs-official/Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales.shp") %>%
  st_transform(4326) %>%
  select(geo_code = msoa11cd)
# z_msoa = readRDS("~/npct/pct-outputs-regional-R/commute/msoa/avon/z.Rds") %>%
#   st_as_sf() %>%
#   select(geo_code, all, bicycle) %>%
#   mutate(pcycle = bicycle / all)

c_oa01 = c_oa01[aggzone, ] # get points
# check input data for region

# subset areal data to region
z = z_msoa[c_oa01, ]
cas = cas[c_oa01, ]
# qtm(cas, borders = "blue") +
#   qtm(aggzones, borders = "red")

# generate OD matrix between all zones in region

l_all = readr::read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_124_1.data.csv?area_of_residence=1308626136...1308626161,1308626164,1308626165,1308626162,1308626163,1308626166...1308626170&area_of_workplace=1308626136...1308626161,1308626164,1308626165,1308626162,1308626163,1308626166...1308626170&cell=415957249&date=latest&measures=20100&select=area_of_residence_code,area_of_workplace_code,obs_value") # works
l_bicycle = readr::read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_124_1.data.csv?area_of_residence=1308626136...1308626161,1308626164,1308626165,1308626162,1308626163,1308626166...1308626170&area_of_workplace=1308626136...1308626161,1308626164,1308626165,1308626162,1308626163,1308626166...1308626170&cell=415959553&date=latest&measures=20100&select=obs_value") # works
l_all = cbind(l_all[1:2], all = l_all$OBS_VALUE, bicycle = l_bicycle$OBS_VALUE)
l01 = od2line(flow = l_all, zones = cas)
l01$dist = as.numeric(st_length(l01))
l01 = l01 %>% filter(dist > 0) %>%
  mutate(pcycle01 = bicycle / all) %>%
  select(o = AREA_OF_RESIDENCE_CODE, d = AREA_OF_WORKPLACE_CODE, all, pcycle01)
plot(l01$geometry, lwd = l01$all / mean(l01$all), col = "grey")
tm_shape(l01) +
  tm_lines(lwd = "all", scale = 5, alpha = 0.9, col = "pcycle01", palette = "RdYlBu", breaks = c(0, 0.05, 0.1, 0.3), auto.palette.mapping = F)

# find uptake from 2001
f11 = select(flow_11, geo_code1, geo_code2, all, bicycle) %>%
  st_set_geometry(NULL) %>%
  filter(geo_code1 %in% z$geo_code, geo_code2 %in% z$geo_code)
l11 = od_aggregate(flow = f11, zones = z, aggzones = cas) %>%
  na.omit()
nrow(f11)
nrow(l11) # 2/3 the original
sum(f11$all)
sum(l11$all)

# cut to chase: get to pcycle11
l11 = l11 %>% mutate(pcycle11 = bicycle / all) %>%
  select(o = flow_new_orig, d = flow_new_dest, pcycle11, all11 = all)
l = inner_join(l01, l11) %>%
  filter(all11 > 20) %>% # observation: fit goes > 0.4 as all > 100
  mutate(uptake = (pcycle11 - pcycle01) * 100)
plot(l$pcycle01, l$pcycle11, cex = l$all / mean(l$all))
cor(l$pcycle01, l$pcycle11, use = "complete.obs")
tm_shape(l) +
  tm_lines(lwd = "all", scale = 5, alpha = 0.9, col = "uptake", palette = "RdYlBu", auto.palette.mapping = F)
summary(l)



# # Testing / development:
# od = points2odf(cas)
# l = od2line(flow = od, cas)
# plot(l$geometry)
# l$dist = st_length(l) %>% as.numeric()
# summary(l$dist)
# # remove all those > 20 km
# l = l %>% filter(dist < 20000 & dist > 0)
# # try to get data on 00HBNW to 00HBNX (from nomis)
# l_test = readr::read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_124_1.data.csv?area_of_residence=1308626144&area_of_workplace=1308626145&cell=415959553&date=latest&measures=20100&select=area_of_residence_code,area_of_workplace_code,cell_name,date_name,measures_name,obs_value,obs_status_name") # works
# # l_test = readr::read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_124_1.data.csv?area_of_residence_code=00HBNW&area_of_workplace_code=00HBNX&cell=415959553&date=latest&measures=20100&select=area_of_residence_code,area_of_workplace_code,cell_name,date_name,measures_name,obs_value,obs_status_name") # fails

# m = readr::read_csv("~/cyipt/example-data/chapeltown-road/1239714089.csv", skip = 9) %>%
#   slice(- n()) %>%
#   select(-1) %>%
#   rename(o = X2)
# summary(m$X1 %in% cas$name) # names match
# summary(m$o %in% cas$ons_label) # labels match
# summary(m$o %in% names(m)) # col headings match labels - go with this
# casod = tidyr::gather(m, d, b, -o)
# head(cas)
# casod_sp = od2line(flow = casod, cas)
# plot(aggzones$geometry)
# plot(casod_sp, lwd = casod_sp$b / 5, add = TRUE)
# # qtm(casod_sp, lines.lwd = "b") # fails - nas
# casod_api = readr::read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_124_1.data.csv?area_of_residence=1308629274...1308629306&area_of_workplace=1308629274...1308629306&cell=415959553&measures=20100&select=area_of_residence_code,area_of_workplace_code,obs_value")
#
# summary(casod_api$AREA_OF_RESIDENCE_CODE %in% cas$ons_label)
# casod_sp2 = od2line(casod_api, cas)
# plot(casod_sp2, lwd = casod_sp2$OBS_VALUE / 10, col = "red", add = T)
#
# casod_car = readr::read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_124_1.data.csv?area_of_residence=1308629274...1308629306&area_of_workplace=1308629274...1308629306&cell=415959553&date=latest&measures=20100&select=area_of_residence_name,area_of_workplace_name,cell_name,date_name,measures_name,obs_value,obs_status_name")
# z_cas = st_join(z, cas)
# l11_cas = od_aggregate(f11, cas, z)
#
#
# # Another method (not working)
# library(UKCensusAPI)
#
# api = instance(cacheDir = "/home/robin/data/")
# list.files("~/data/")
#
#
# list.files("~/R/x86_64-pc-linux-gnu-library/3.4/UKCensusAPI/scripts/")
#
# UKCensusAPI::getMetadata(api = api, tableName = "T203")
