#Visualise Data

library(sf)
library(leaflet)

#Read in data
osm <- readRDS("../example-data/cambridge/results/osm-select-infra.Rds")

#Clean Up
osm <- osm[,c("id","roadtype2","infra_score","pct_census","speed","highway","aadt","action","change","cost.total")]
osm$infra_score <- as.factor(osm$infra_score)
osm$roadtype2 <- as.factor(osm$roadtype2)
osm$highway <- as.factor(osm$highway)
osm$action <- as.factor(osm$action)
osm$change <- as.factor(osm$change)

osm <- osm[!(osm$infra_score == "None") | !(osm$roadtype2 == "Road - Cycling Allowed None None"),]
osm <- st_transform(osm, 4326)

osm_exist <- osm[!(osm$roadtype2 == "Road - Cycling Allowed None None") & !(osm$roadtype2 == "Shared Path None None"),]
osm_pro <- osm[!(osm$infra_score == "None"),]

#Set Up leaflet
pal_ext <- colorFactor(c("darkorchid","darkorchid1","darkorchid2",
                         "darkorchid4","darkgreen","forestgreen",
                         "firebrick","deepskyblue","deepskyblue4",
                         "deepskyblue4","darkgrey","dodgerblue",
                         "dodgerblue4","blue2","dodgerblue",
                         "blue3","dodgerblue3","darkred",
                         "darkorange","darkorange1","darkgrey"), osm_exist$roadtype2)


pal_pro <- colorFactor(c("red","green","blue","orange","yellow","purple"), osm_pro$infra_score)
popup_ext <- paste("id", osm_exist$id, "<br>",
              "Number of Cyclists: ", osm_exist$pct_census, "<br>",
              "Speed Limit: ", osm_exist$speed, "<br>",
              "Road Type: ", osm_exist$highway, "<br>",
              "Annual Average Daily Trafic: ", osm_exist$aadt, "<br>",
              "Existing Infrastructure: ", osm_exist$roadtype2, "<br>",
              "Recomended Infrastructure: ", osm_exist$infra_score
              )

popup_pro <- paste("id", osm_pro$id, "<br>",
                   "Number of Cyclists: ", osm_pro$pct_census, "<br>",
                   "Speed Limit: ", osm_pro$speed, "<br>",
                   "Road Type: ", osm_pro$highway, "<br>",
                   "Annual Average Daily Trafic: ", osm_pro$aadt, "<br>",
                   "Existing Infrastructure: ", osm_pro$roadtype2, "<br>",
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
               color = ~pal_ext(roadtype2),
               weight = 4,
               popup = popup_ext,
               group = "Existing",
               highlightOptions = highlightOptions(color = "black", weight = 4,
                                                   bringToFront = TRUE)) %>%
  addLegend(pal = pal_ext,
            values = ~roadtype2,
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

