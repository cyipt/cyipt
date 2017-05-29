#Demo Infrastrucutre data
library(sf)


#Reading and clena up some data
osm <- readRDS("../example-data/bristol/osm-lines-quietness-full.Rds")
osm <- osm[,c("osm_id","name","highway","cycleway",
              "cs_quietness","cs_speed","cs_pause","cs_cyclable","cs_walkable")]
osm <- osm[!is.na(osm$cs_quietness),]
#osm <- osm[1:1000,]


#Add some made up data
osm$pcu <- floor(rnorm(n = nrow(osm), mean = 2000, sd = 200))
osm$pcu <- sqrt(osm$pcu * osm$pcu)
hist(osm$pcu)
osm$width <- round(runif(nrow(osm), min = 5, max = 15), 1)
osm$widthpath <- osm$width + round(runif(nrow(osm), min = 1, max = 5), 1)
osm$pct <- floor(rnorm(n = nrow(osm), mean = 200, sd = 300))
osm$pct <- sqrt(osm$pct * osm$pct)
hist(osm$pct)
#Quick Evaluation

osm$cycle_network <- NA
for(a in 1:nrow(osm)){
  if(osm$pct[a] >= 1000){
    osm$cycle_network[a] <- "Primary"
  }else if(osm$pct[a] >= 500 & osm$pct[a] < 1000){
    osm$cycle_network[a] <- "Secondary"
  }else if(osm$pct[a] >= 250 & osm$pct[a] < 500){
    osm$cycle_network[a] <- "Tertiary"
  }else{
    osm$cycle_network[a] <- "Off Network"
  }
}

plot(osm[osm$cycle_network == "Primary","cycle_network"])

osm$upgrade <- NA
for(b in 1:nrow(osm)){
  if(osm$cycle_network[b] == "Primary" & osm$cs_quietness[b] < 80){
    osm$upgrade[b] <- T
  }else if(osm$cycle_network[b] == "Secondary" & osm$cs_quietness[b] < 70){
    osm$upgrade[b] <- T
  }else if(osm$cycle_network[b] == "Tertiary" & osm$cs_quietness[b] < 60){
    osm$upgrade[b] <- T
  }
  else{
    osm$upgrade[b] <- F
  }
}
plot(osm[osm$upgrade == T,"cs_quietness"])


