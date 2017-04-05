#' Download cycle paths from OSM
#'
#' Use the development version of the osmdata package.

#' @examples
#' library(osmdata)
#' library(magrittr)
#' library(sf)
#' library(leaflet)
#' b = getbb("Bristol")
#' q = opq(b) %>% add_feature(key = "highway", value = "cycleway")
#' q1 = opq(b) %>% add_feature(key = "name", value = "Atlantic Coast Route - part United Kingdom")
#' cq1 = osmdata_sp(q = q1)
#' sp::plot(cq1$osm_lines)
#' cycleway = osmdata_sp(q = q)

#' sp::plot(cycleway$osm_lines)

#' mapview::mapview(cycleway$osm_lines, lwd = 5, map.types = "Thunderforest.OpenCycleMap", color = "black") +
#'   mapview::mapview(cq1$osm_lines, lwd = 5, map.types = "Thunderforest.OpenCycleMap", color = "green")


#' leaflet() %>%
#'   # addProviderTiles("Thunderforest.OpenCycleMap") %>%
#'   addPolylines(data = cycleway$osm_lines)

#' # Download all data for a small area
#' bct = getbb("Chapeltown Road")
#' qct = opq(bct)
#' osmdata_xml(q = qct, filename = "../example-data/chapeltown.osm")
ct = osmdata_sp(qct)

# la data
library(sf)
library(tmap)
library(dplyr)
library(osmdata)
# las = st_read("~/npct/pct-bigdata/las-pcycle.geojson")
# summary(las)
# bristol = las[grepl(pattern = "Bristol", las$NAME),]
# plot(bristol)
# bristol = st_read("http://geoportal.statistics.gov.uk/datasets/686603e943f948acaa13fb5d2b0f1275_3.kml")
# bristol = filter(bristol, grepl("Bristol", lad16nm))
# st_write(bristol, "areas/bristol-poly.geojson", update = TRUE)
bristol = st_read("areas/bristol-poly.geojson")
tmap_mode("view")
qtm(bristol)
q = opq(st_bbox(bristol)) %>%
  add_feature(key = "highway", value = "[a-z]", value_exact = FALSE)
res = osmdata_sf(q = q)
plot(res$osm_lines[1])

cyclestreets_attributes = readr::read_csv("input-data/scorings/bristol.csv")
head(cyclestreets_attributes)
head(res$osm_lines)
class(res$osm_lines$osm_id)
res$osm_lines$osm_id = as.integer(as.character(res$osm_lines$osm_id))
cyclestreets_attributes = rename(cyclestreets_attributes, osm_id = id)
osml = left_join(res$osm_lines, cyclestreets_attributes)

osml_full = osml
osmp_sub = osmdata_sf(q = opq(bbox = "Broadmead") %>% add_feature(key = "building", value = "house"))
qtm(osmp_sub$osm_polygons[2:15,])
bb_square = stplanr::bb2poly(as(osmp_sub$osm_polygons[2:15,], "Spatial"))
osml = osml_full[st_as_sf(bb_square),]
qtm(osml, lines.col = "quietness")
dir.create("../example-data/bristol")
# st_write(osml, "../example-data/bristol/osm-lines-quietness.gpkg") # fails
saveRDS(osml, "../example-data/bristol/osm-lines-quietness.Rds")
