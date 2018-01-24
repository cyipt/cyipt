# Check Traffic
library(dplyr)

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


osm <- read.csv("../cyipt-bigdata/forDB/roads_16Jan2018.csv")
osm <- osm[!is.na(osm$aadt),]

osm.sum <- osm[,c("highway","aadt","idGlobal")]

osm.sum <- osm.sum %>%
  group_by(highway) %>%
  summarise(min = min(aadt),
            count = length(aadt),
            Q1 = quantile(aadt, probs = 0.25),
            mean = mean(aadt),
            median = median(aadt),
            Q3 = quantile(aadt, probs = 0.75),
            max = max(aadt),
            mode = getmode(aadt))

