# Get all tags from osm for old infrastructure
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

region_shape = readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
region_shape = filter(region_shape, grepl("Cambr", ttwa11nm))

# road net and old-infra data ----
td = st_read("../td/dft-england-cycling-data-2011.geojson")
td_type = st_geometry_type(td)
summary(td_type)
td_p = td[td_type == "POINT", ]
summary(td_p)
# plot(td_p$geometry)
td = td[td_type == "LINESTRING", ]
old_infra = td # rely on td data for now...
# todo: add a bit with all ways, not just busy (40mph+ ones)
# ways_uk = ...
ways_uk <- readRDS("../cyipt-bigdata/osm-prep/Cambridge/osm-lines.Rds")
old_infra = old_infra[region_shape, ]
# qtm(old_infra)
old_infra = st_transform(old_infra, 27700)
old_infra_buff = st_buffer(old_infra, 10)
# old_infra_buff_union = st_union(old_infra_buff) # not used - time consuming
# osm_lookup_all = st_contains(old_infra_buff_union, ways_uk)
osm_lookup = st_contains(old_infra_buff, ways_uk)
osm_lookup_unlisted = unique(unlist(osm_lookup))

n = 5
x = old_infra[n, ]
xb = old_infra_buff[n, ]
y = ways_uk[osm_lookup[[n]], ]
qtm(x) +
  qtm(xb) +
  qtm(y)

# j1 = st_join(x = x, y = y, join = st_is_within_distance, dist = 10) # returns 3 results
x$maxspeed = aggregate(y[c("maxspeed")], xb, FUN = mean)$maxspeed
most_common = function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))] }

x$maxspeed = aggregate(y[c("maxspeed")], xb, FUN = mean, join = st_contains)$maxspeed
x$highway = aggregate(y[c("highway")], xb, FUN = most_common, join = st_contains)
qtm(x) +
  qtm(y, lines.col = "green")

# scaling it across cambridge
ways_uk_infra = ways_uk[osm_lookup_unlisted, ]
qtm(ways_uk_infra$geometry) +
  qtm(old_infra_buff)

old_infra$maxspeed = aggregate(ways_uk_infra[c("maxspeed")], old_infra_buff, FUN = median)$maxspeed
old_infra$aadt = aggregate(ways_uk_infra[c("aadt")], old_infra_buff, FUN = median)$aadt
clean_cycleway = function(ways, cycleway_vars = c("cycleway.left", "cycleway.right", "cy"))

old_infra$cycleway.left.clean = aggregate(ways_uk_infra[c("cycleway.left")], old_infra_buff, FUN = most_common)$cycleway.left
summary(factor(old_infra$cycleway.left.clean))
summary(factor(old_infra$cycleway.left)) # more nas
old_infra$cycleway = as.character(old_infra$cycleway)
sel_na_cycleway = is.na(old_infra$cycleway)
old_infra$cycleway[sel_na_cycleway] = old_infra$cycleway.left.clean[sel_na_cycleway]
summary(factor(old_infra$cycleway))
sel_na_cycleway = is.na(old_infra$cycleway) | old_infra$cycleway == "no"
old_infra$cycleway.right.clean = aggregate(ways_uk_infra[c("cycleway.right")], old_infra_buff, FUN = most_common)$cycleway.right
summary(factor(old_infra$cycleway.right.clean))
old_infra$cycleway[sel_na_cycleway] = old_infra$cycleway.right.clean[sel_na_cycleway]
summary(factor(old_infra$cycleway))

qtm(old_infra, lines.col = "maxspeed", lines.lwd = 3)
qtm(old_infra, lines.col = "cycleway", lines.lwd = 3)
# mapview::mapview(old_infra, color = old_infra$maxspeed)

infra_minimal = select(old_infra, highway, maxspeed, segregated, cycleway)
