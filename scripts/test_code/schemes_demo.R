#Visualise Data

library(sf)
library(leaflet)

#Read in data
osm <- readRDS("bristolSchemesDemo.Rds")

osm <- osm %>%
  group_by(group_id) %>%
  summarise()
#osm$group_id <- as.character(osm$group_id)


#Clean Up
osm <- st_transform(osm, 4326)


#Set Up leaflet
pal <- colorFactor(
  palette = "Paired",
  domain = osm$group_id)

bounds <- readRDS("../cyipt-bigdata/osm-raw/Bristol/bounds.Rds")
bounds <- st_transform(bounds, 4326)


leaflet(osm) %>%
  #Base map options
  addProviderTiles("OpenMapSurfer.Grayscale", group = "Greyscale") %>%
  addProviderTiles("Thunderforest.OpenCycleMap", group = "Open Cycle Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%

  #Existing Infrastructure
  addPolylines(data = osm,
               color = ~pal(group_id),
               weight = 4,
               #popup = popup,
               group = "Existing",
               highlightOptions = highlightOptions(color = "black", weight = 4,
                                                   bringToFront = TRUE)) %>%

  addPolylines(data = bounds,
               color = "black",
               weight = 4) %>% #,
               #popup = popup,
               #group = "Existing",
               #highlightOptions = highlightOptions(color = "black", weight = 4,
                                                   #bringToFront = TRUE)) %>%

  #Mini Map
  addMiniMap(tiles = "OpenMapSurfer.Grayscale", toggleDisplay = TRUE, position = "bottomleft") %>%

  #Layers Control
  addLayersControl(
    baseGroups = c("Greyscale", "Open Cycle Map", "Satelite"),
    position = "topleft",
    options = layersControlOptions(collapsed = FALSE))



