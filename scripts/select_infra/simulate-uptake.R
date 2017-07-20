# road scenarios
knitr::knit(input = "model-uptake.Rmd")
typology = readr::read_csv("input-data/roadtypes4.csv")
rc = readRDS("../example-data/bristol/results/osm-schemes.Rds")
source("R/geobuffer.R")
ways$length = geo_projected(ways, st_length)
summary(ways$length)
ways_long = dplyr::filter(ways, length > 500)
l_joined = st_join(lfq$rf, ways_no_quiet["quietness"], FUN = mean) # old way
