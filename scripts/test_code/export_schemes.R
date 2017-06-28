#Split schemes

osm <- readRDS("../example-data/bristol/results/osm-schemes.Rds")
osm$group_id[is.na(osm$group_id)] <- 0


osm2 <- st_transform(osm,3785)

#Save out each file
for(a in 1:max(osm$group_id)){
  sub <- osm[osm2$group_id == a,]
  saveRDS(sub,paste0("../cyipt-shiny/data/s",a,".Rds"))
}



leaflet(sub) %>%
  #Base map options
  addProviderTiles("OpenMapSurfer.Grayscale", group = "Greyscale") %>%

  #Existing Infrastructure
  addPolylines(data = sub,
               #color = ~pal_ext(existing_infra),
               weight = 4,
               #popup = popup_ext,
               #group = "Existing",
               highlightOptions = highlightOptions(color = "black", weight = 4,
                                                   bringToFront = TRUE))

