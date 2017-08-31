#Visualise Data

library(sf)
library(tmap)

tmap_mode("view")

#Read in data
osm <- readRDS("../example-data/bristol/results/osm-select-infra.Rds")

#Clean Up
infra <- osm[,c("id","osm_id","infra_score","pct_census","speed","highway","aadt")]
infra <- infra[infra$infra_score != "None",]
infra$infra_score <- as.factor(infra$infra_score)

#qtm(infra, lines.col = "infra_score", lines.lwd = 5, style = "choropleth", alpha = 1)

tm_shape(infra) +
tm_lines(col = "infra_score", lwd = 5, alpha = 1,
         title.col = "Recommended Cycling Infrastructure",
         popup.vars = c("pct_census","speed", "highway","aadt"))



exist <- osm[!is.na(osm$cycleway) | !is.na(osm$cycleway.left) | !is.na(osm$cycleway.right) | !is.na(osm$cycleway.otherside),]
exist <- exist[,c("id","osm_id","infra_score","pct_census","speed","highway","aadt","cycleway","segregated")]
exist$cycleway <- as.factor(exist$cycleway)

tm_shape(exist) +
  tm_lines(col = "cycleway", lwd = 5, alpha = 1,
           title.col = "Existing Cycling Infrastructure",
           popup.vars = c("pct_census","speed", "highway","aadt", "cycleway", "segregated"))

