# get cas2003 OD data
devtools::install_github("robinlovelace/ukboundaries")
library(tmap)
tmap_mode("view")
library(ukborders)
library(tidyverse)
library(stplanr)
library(sf)

aggzones = readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
aggzone = filter(aggzones, ttwa11nm == "Bristol")
c_oa01 = st_read("../cyipt-inputs-official/Output_Areas_December_2001_Population_Weighted_Centroids.shp") %>%
  st_transform(4326)
flow_11 = readRDS("~/npct/pct-outputs-regional-R/commute/msoa/avon/l.Rds") %>%
  as(Class = "sf")
z_msoa = st_read("../cyipt-inputs-official/Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales.shp")

c_oa01 = c_oa01[aggzone, ]
cas = cas2003_vsimple[c_oa01, ]
qtm(cas, borders = "blue") +
  qtm(aggzones, borders = "red")




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
