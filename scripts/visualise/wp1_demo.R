#Visualise Data

library(sf)
library(tmap)

tmap_mode("view")

#Read in data
osm <- readRDS("../example-data/bristol/results/osm-select-infra.Rds")

#Clean Up
infra <- osm[,c("id","osm_id","infra_score")]
infra <- infra[infra$infra_score != "None",]
infra$infra_score <- as.factor(infra$infra_score)

#qtm(infra, lines.col = "infra_score", lines.lwd = 5, style = "choropleth", alpha = 1)

tm_shape(infra) +
tm_lines(col = "infra_score", lwd = 5, alpha = 1, title.col = "Recommended Cycling Infrastructure")



