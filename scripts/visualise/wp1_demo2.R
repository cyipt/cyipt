#Visualise Data

library(sf)
library(leaflet)

#Read in data
osm <- readRDS("../example-data/bristol/results/osm-select-infra.Rds")

#Clean Up
osm <- osm[,c("id","existing_infra","infra_score","pct_census","speed","highway","aadt")]
osm$infra_score <- as.factor(osm$infra_score)
osm$existing_infra <- as.factor(osm$existing_infra)
osm <- osm[!(osm$infra_score == "None") | !is.na(osm$existing_infra),]
osm <- st_transform(osm, 4326)

osm_exist <- osm[!is.na(osm$existing_infra),]
osm_pro <- osm[!(osm$infra_score == "None"),]

#Set Up leaflet
pal_ext <- colorFactor(c("red","green","blue","orange","yellow","brown","purple"), osm_exist$existing_infra)
pal_pro <- colorFactor(c("red","green","blue","orange","yellow","purple"), osm_pro$infra_score)
popup_ext <- paste("id", osm_exist$id, "<br>",
              "Existing Infrastructure: ", osm_exist$existing_infra, "<br>",
              "Recomended Infrastructure: ", osm_exist$infra_score, "<br>",
              "Number of Cyclists: ", osm_exist$pct_census, "<br>",
              "Speed Limit: ", osm_exist$speed, "<br>",
              "Road Type: ", osm_exist$highway, "<br>",
              "Annual Average Daily Trafic: ", osm_exist$aadt)

popup_pro <- paste("id", osm_pro$id, "<br>",
                   "Existing Infrastructure: ", osm_pro$existing_infra, "<br>",
                   "Recomended Infrastructure: ", osm_pro$infra_score, "<br>",
                   "Number of Cyclists: ", osm_pro$pct_census, "<br>",
                   "Speed Limit: ", osm_pro$speed, "<br>",
                   "Road Type: ", osm_pro$highway, "<br>",
                   "Annual Average Daily Trafic: ", osm_pro$aadt)


leaflet(osm_pro) %>%
  #Base map options
  addProviderTiles("OpenMapSurfer.Grayscale", group = "Greyscale") %>%
  addProviderTiles("Thunderforest.OpenCycleMap", group = "Open Cycle Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%

  #Existing Infrastructure
  addPolylines(data = osm_exist,
               color = ~pal_ext(existing_infra),
               weight = 4,
               popup = popup_ext,
               group = "Existing") %>%
  addLegend(pal = pal_ext,
            values = ~existing_infra,
            opacity = 1,
            title = "Existing Cycling Infrastructure"
            ) %>%

  #Proposed Infrastructure
  addPolylines(data = osm_pro,
               color = ~pal_pro(infra_score),
               weight = 4,
               popup = popup_pro,
               group = "Recommended") %>%
  addLegend(pal = pal_pro,
            values = ~infra_score,
            opacity = 1,
            title = "Recommended Cycling Infrastructure"
            ) %>%

  #Mini Map
  addMiniMap(tiles = "OpenMapSurfer.Grayscale", toggleDisplay = TRUE, position = "bottomleft") %>%

  #Layers Control
  addLayersControl(
    baseGroups = c("Greyscale", "Open Cycle Map", "Satelite"),
    overlayGroups = c("Recommended","Existing"),
    position = "topleft",
    options = layersControlOptions(collapsed = FALSE)) %>%

  hideGroup("Existing")

