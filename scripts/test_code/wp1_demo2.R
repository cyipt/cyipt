#Visualise Data

library(sf)
library(leaflet)

#Read in data
osm <- readRDS("../example-data/cambridge/results/osm-select-infra.Rds")

#Clean Up
osm <- osm[,c("id","existing_infra","infra_score","pct_census","speed","highway","aadt","action","change","cost.total")]
osm$infra_score <- as.factor(osm$infra_score)
osm$existing_infra <- as.factor(osm$existing_infra)
osm$highway <- as.factor(osm$highway)
osm$action <- as.factor(osm$action)
osm$change <- as.factor(osm$change)

osm <- osm[!(osm$infra_score == "None") | !(osm$existing_infra == "None"),]
osm <- st_transform(osm, 4326)

osm_exist <- osm[!(osm$existing_infra == "None"),]
osm_pro <- osm[!(osm$infra_score == "None"),]

#Set Up leaflet
pal_ext <- colorFactor(c("red","green","blue","orange","yellow","brown","purple"), osm_exist$existing_infra)
pal_pro <- colorFactor(c("red","green","blue","orange","yellow","purple"), osm_pro$infra_score)
popup_ext <- paste("id", osm_exist$id, "<br>",
              "Number of Cyclists: ", osm_exist$pct_census, "<br>",
              "Speed Limit: ", osm_exist$speed, "<br>",
              "Road Type: ", osm_exist$highway, "<br>",
              "Annual Average Daily Trafic: ", osm_exist$aadt, "<br>",
              "Existing Infrastructure: ", osm_exist$existing_infra, "<br>",
              "Recomended Infrastructure: ", osm_exist$infra_score
              )

popup_pro <- paste("id", osm_pro$id, "<br>",
                   "Number of Cyclists: ", osm_pro$pct_census, "<br>",
                   "Speed Limit: ", osm_pro$speed, "<br>",
                   "Road Type: ", osm_pro$highway, "<br>",
                   "Annual Average Daily Trafic: ", osm_pro$aadt, "<br>",
                   "Existing Infrastructure: ", osm_pro$existing_infra, "<br>",
                   "Recomended Infrastructure: ", osm_pro$infra_score, "<br>",
                   "Cost for segment: £", osm_pro$cost.total,"<br>",
                   "Action: ",osm_pro$action, "<br>",
                   "Change: ",osm_pro$change)


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
               group = "Existing",
               highlightOptions = highlightOptions(color = "black", weight = 4,
                                                   bringToFront = TRUE)) %>%
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
               group = "Recommended",
               highlightOptions = highlightOptions(color = "black", weight = 4,
                                                   bringToFront = TRUE)) %>%
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

