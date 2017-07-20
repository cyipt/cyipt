# road scenarios

typology = readr::read_csv("input-data/roadtypes4.csv")
rc = readRDS("../example-data/bristol/results/osm-schemes.Rds")
osm_data = readRDS("../example-data/bristol/osm-all-highways.Rds")
ways = osm_data$osm_lines
lfq = readRDS("../example-data/bristol/lfq.Rds")

