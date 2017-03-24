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
