# get interventions
# example way: 206848638
# source: 1st complete one here:
# http://wiki.openstreetmap.org/wiki/WikiProject_Sustrans_Connect2
library(osmdata)
library(sf)
bb_england = getbb("England")
q = opq(bb_england) %>% add_osm_feature(key = "name", value = "Stans Way")
res = osmdata_sf(q)
plot(res$osm_lines$geometry)
p = st_cast(res$osm_lines, "POINT")
plot(p$geometry)

h = getbb("Hampi, India") %>%
  opq() %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()
plot(h$osm_lines)
h = h$osm_lines
head(h)
p = st_cast(h, "POINT")
nrow(p)

library(dodgr)
?hampi
p = st_cast(hampi, "POINT")
hampi2 = dodgr::dodgr_streetnet("hampi, india")
p = st_cast(hampi2, "POINT")
