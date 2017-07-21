# road scenarios
knitr::knit(input = "model-uptake.Rmd")
typology = readr::read_csv("input-data/roadtypes4.csv")
rc = readRDS("../example-data/bristol/results/osm-schemes.Rds")
summary(ways$length)
select(ways, contains("cycle")) %>% summary()
ways_long = dplyr::filter(ways, length > 500)
ways_long$length_cycleway = ways_long$length
summary(ways_long$cycleway)
l_joined = stjoin_old(lfq$rf, ways_long["length_cycleway"], FUN = sum) # old way
summary(l_full$length_cycleway)
summary(l_joined$length_cycleway) # 5 fold increase in cycling
l_full$length_cycleway = l_joined$length_cycleway
uptake = predict(m8, as.matrix(l_full[-c(1, 2)]))
cor(uptake * l$all, l_full$bicycle)^2
mean(uptake)
mean(l_full$bicycle / l_full$all)

# communicate result:
# knitr::spin(hair = "scripts/select_infra/simulate-uptake.R")
