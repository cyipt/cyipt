library(tidyverse)
library(sf)

# u_oa01 = "https://opendata.arcgis.com/datasets/0ee1ff772c7049ffa5f13b699474be55_0.zip?outSR=%7B%22wkid%22%3A27700%2C%22latestWkid%22%3A27700%7D"
# download.file(u_oa01, "out.zip")
# unzip(zipfile = "out.zip", exdir = "../cyipt-inputs-official/")
# u_msoa11 = "https://opendata.arcgis.com/datasets/826dc85fb600440889480f4d9dbb1a24_3.zip?outSR=%7B%22wkid%22%3A27700%2C%22latestWkid%22%3A27700%7D"
# download.file(u_msoa11, "out.zip")
# unzip(zipfile = "out.zip", exdir = "../cyipt-inputs-official/")
oa_lookup = readRDS("../cyipt-inputs-official/OA01_OA11_LAD11_EW_LU.Rds")
aggzones = readRDS("~/npct/pct-outputs-regional-R/commute/msoa/west-yorkshire/z.Rds")

aggzones = st_as_sf(aggzones)
flow_11 = readRDS("~/npct/pct-outputs-regional-R/commute/msoa/west-yorkshire/l.Rds")
c_oa01 = st_read("../cyipt-inputs-official/Output_Areas_December_2001_Population_Weighted_Centroids.shp")
z_msoa = st_read("../cyipt-inputs-official/Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales.shp")

od01_orig = read_csv("~/data/W301_OUT.csv", col_names = FALSE)
od01 = left_join(od01_orig, oa_lookup, by = c("X1" = "OA01CDO")) %>%
  distinct(X1, X2, .keep_all = TRUE)
sum(od01$X3)
sum(od01$X3) # check totals

od01 = filter(od01, LAD11NM == "Leeds", )

names_modes = c("all", "mfh", "light_rail", "train", "bus", "taxi", "car", "car_p","moto", "cycle", "foot", "other")

od01 = od01[c(1:2, (1:12) * 3)]
names(od01) = c("o", "d", names_modes)
summary(od01)

summary(od01$o %in% c_oa01$oa01cdold) # check codes match
c_oa01 = select(c_oa01, oa01cdold)
flow = stplanr:::od2line(flow = od01, zones = c_oa01)
st_crs(c_oa01)
st_crs(aggzones)
c_oa01 = st_transform(c_oa01, 4326)
c_oa01 = filter(c_oa01, oa01cdold %in% flow$o)
aggzones = aggzones[c_oa01,]
plot(aggzones$geometry)
plot(c_oa01, add = T)
od01 <- filter(od01, d %in% o)
flow_ag <- stplanr::od_aggregate(flow = od01, zones = c_oa01, aggzones) # breaks
flow_ag_sp = stplanr::od2line(flow_ag, aggzones)
plot(flow_ag_sp$geometry[1:(nrow(flow_ag) - 1) ]) # last on is boshed
flow_ag_sp = filter(flow_ag_sp, !is.na(flow_new_orig) & !is.na(flow_new_dest))
plot(aggzones$geometry)
# plot(flow_ag_sp, add = T, lwd = flow_ag_sp$all / 99)
plot(flow_ag_sp, add = T, lwd = flow_ag_sp$cycle / 30)

flow_11 = st_as_sf(flow_11)
flow_ag_sp = mutate(flow_ag_sp, id = paste(flow_new_orig, flow_new_dest))
summary(flow_11$id %in% flow_ag_sp$id)
head(cbind(flow_ag_sp$id, flow_11$id))
flow_11j = left_join(flow_11, st_set_geometry(flow_ag_sp, NULL), by = "id") %>%
  filter(!is.na(all.y))
sum(flow_11j$all.x) / sum(flow_ag_sp$all)
flow_high = filter(flow, all > 20) # 70% of flows are in there
plot(flow_11j$all.x, flow_11j$all.y)
cor(flow_11j$all.x, flow_11j$all.y)
mapview::mapview(flow_high)


# testing
colsums = colSums(od01[3:ncol(od01)])
length(colsums)
plot(colsums[1:18])
plot(colsums[(1:18) + 18])
sum(colsums[1:18])
sum(colsums[(1:18) + 18(1:18) + 18(1:18) + 18(1:18) + 18(1:18) + 18(1:18) + 18])
cs_all = colsums[(1:12) * 3 - 2]
names(cs_all) = names_modes
barplot(cs_all) # looks right
