# get cas2003 OD data
devtools::install_github("robinlovelace/ukboundaries")
library(tmap)
tmap_mode("view")
library(ukboundaries)
library(tidyverse)
library(stplanr)

aggzones = readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
aggzone = filter(aggzones, ttwa11nm == "Bristol")
c_oa01 = st_read("../cyipt-inputs-official/Output_Areas_December_2001_Population_Weighted_Centroids.shp") %>%
  st_transform(4326)
flow_11 = readRDS("~/npct/pct-outputs-regional-R/commute/msoa/avon/l.Rds") %>%
  as(Class = "sf")
z_msoa = st_read("../cyipt-inputs-official/Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales.shp") %>%
  st_transform(st_crs(cas))

c_oa01 = c_oa01[aggzone, ]
# check input data for region

cas = cas2003_simple[c_oa01, ]
z = z_msoa[c_oa01, ]
qtm(cas, borders = "blue") +
  qtm(aggzones, borders = "red")

# generate OD matrix between all zones in region
od = points2odf(cas)
l = od2line(flow = od, cas)
plot(l$geometry)
l$dist = st_length(l) %>% as.numeric()
summary(l$dist)
# remove all those > 20 km
l = l %>% filter(dist < 20000 & dist > 0)
# try to get data on 00HBNW to 00HBNX (from nomis)
l_test = readr::read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_124_1.data.csv?area_of_residence=1308626144&area_of_workplace=1308626145&cell=415959553&date=latest&measures=20100&select=area_of_residence_code,area_of_workplace_code,cell_name,date_name,measures_name,obs_value,obs_status_name") # works
# l_test = readr::read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_124_1.data.csv?area_of_residence_code=00HBNW&area_of_workplace_code=00HBNX&cell=415959553&date=latest&measures=20100&select=area_of_residence_code,area_of_workplace_code,cell_name,date_name,measures_name,obs_value,obs_status_name") # fails
l_all = readr::read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_124_1.data.csv?area_of_residence=1308626136...1308626161,1308626164,1308626165,1308626162,1308626163,1308626166...1308626170&area_of_workplace=1308626136...1308626161,1308626164,1308626165,1308626162,1308626163,1308626166...1308626170&cell=415957249&date=latest&measures=20100&select=area_of_residence_code,area_of_workplace_code,obs_value") # works
l_bicycle = readr::read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_124_1.data.csv?area_of_residence=1308626136...1308626161,1308626164,1308626165,1308626162,1308626163,1308626166...1308626170&area_of_workplace=1308626136...1308626161,1308626164,1308626165,1308626162,1308626163,1308626166...1308626170&cell=415959553&date=latest&measures=20100&select=obs_value") # works
l_all = cbind(l_all[1:2], all = l_all$OBS_VALUE, bicycle = l_bicycle$OBS_VALUE)
l_test = od2line(flow = l_all, zones = cas)
l_test$dist = as.numeric(st_length(l_test))
l_test = l_test %>% filter(dist > 0) %>%
  mutate(pcycle = bicycle / all)
plot(l_test, lwd = l_test$all / mean(l_test$all), col = "grey")
tm_shape(l_test) +
  tm_lines(lwd = "all", scale = 5, alpha = 0.9, col = "pcycle", palette = "RdYlBu", breaks = c(0, 0.05, 0.1, 0.3), auto.palette.mapping = F)

m = readr::read_csv("~/cyipt/example-data/chapeltown-road/1239714089.csv", skip = 9) %>%
  slice(- n()) %>%
  select(-1) %>%
  rename(o = X2)
summary(m$X1 %in% cas$name) # names match
summary(m$o %in% cas$ons_label) # labels match
summary(m$o %in% names(m)) # col headings match labels - go with this
casod = tidyr::gather(m, d, b, -o)
head(cas)
casod_sp = od2line(flow = casod, cas)
plot(aggzones$geometry)
plot(casod_sp, lwd = casod_sp$b / 5, add = TRUE)
# qtm(casod_sp, lines.lwd = "b") # fails - nas
casod_api = readr::read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_124_1.data.csv?area_of_residence=1308629274...1308629306&area_of_workplace=1308629274...1308629306&cell=415959553&measures=20100&select=area_of_residence_code,area_of_workplace_code,obs_value")

summary(casod_api$AREA_OF_RESIDENCE_CODE %in% cas$ons_label)
casod_sp2 = od2line(casod_api, cas)
plot(casod_sp2, lwd = casod_sp2$OBS_VALUE / 10, col = "red", add = T)

casod_car = readr::read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_124_1.data.csv?area_of_residence=1308629274...1308629306&area_of_workplace=1308629274...1308629306&cell=415959553&date=latest&measures=20100&select=area_of_residence_name,area_of_workplace_name,cell_name,date_name,measures_name,obs_value,obs_status_name")


# Another method (not working)
library(UKCensusAPI)

api = instance(cacheDir = "/home/robin/data/")
list.files("~/data/")


list.files("~/R/x86_64-pc-linux-gnu-library/3.4/UKCensusAPI/scripts/")

UKCensusAPI::getMetadata(api = api, tableName = "T203")
