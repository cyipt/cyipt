# get interventions
# example way: 206848638
# source: 1st complete one here:
# http://wiki.openstreetmap.org/wiki/WikiProject_Sustrans_Connect2
library(osmdata)
library(sf)
bb_england = getbb("England")
q = opq(bb_england) %>% add_osm_feature(key = "name", value = "Stans Way")
res = osmdata_sf(q)
plot(res$osm_lines)
