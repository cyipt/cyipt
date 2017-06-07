# Aim: download all ways, relations and osm objects

# install and load packages
devtools::install_github("cyipt/cyipt")
library(sf)
library(osmdata)
library(tmap)
bristol = st_read("areas/bristol-poly.geojson")
tmap_mode("view")
qtm(bristol)
q = opq(st_bbox(bristol)) %>%
  add_feature(key = "highway")
res = osmdata_sf(q = q)

plot(res$osm_lines[1]) # plot all the highways in the area
saveRDS(res, "../example-data/bristol/osm-all-highways.Rds")

# merging data in
source("R/dl-paths.R")
merge_osm_cs # check code
cs_obj = readr::read_csv("input-data/scorings/bristol.csv")
library(dplyr)
osml = merge_osm_cs(osm_obj = res$osm_lines, cs_obj = cs_obj) # use it
