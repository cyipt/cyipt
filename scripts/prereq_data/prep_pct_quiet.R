# Prepare Quietes LSOA Routes

library(sp)
library(sf)
library(dplyr)

rq <- readRDS("../pct-outputs-national/commute/lsoa/rq_all.Rds")

rq <- st_as_sf(rq)
names(rq)
head(rq)
rq <- rq[,c("id","e_dist_km","rf_dist_km","rq_dist_km","dist_rf_e",
            "dist_rq_rf","rf_avslope_perc", "rq_avslope_perc",
            "rf_time_min","rq_time_min","geometry")]

rq <- st_transform(rq, 27700)

saveRDS(rq,"../cyipt-securedata/pct-routesquiet-all.Rds")
