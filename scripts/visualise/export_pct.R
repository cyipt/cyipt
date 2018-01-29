# Export Regions for Database
library(sf)
library(tmap)
library(dplyr)
tmap_mode("view")


# pcts

rf <- readRDS("../cyipt-securedata/pct-routes-all.Rds")
rq <- readRDS("../cyipt-securedata/pct-routesquiet-all.Rds")
l <- readRDS("../cyipt-securedata/pct-lines-all.Rds")

#Select Relevant Columns
rf <- rf[,c("ID","all_16p","onfoot","carorvan","pct.census","pct.gov","pct.gen","pct.dutch","pct.ebike","length","time","av_incline","geometry")]
rq <- rq[,c("id","rq_dist_km","rq_avslope_perc","rq_time_min","geometry")]

#rename columms
names(rf) <- c("id","all","onfoot","carorvan","pctcensus","pctgov","pctgen","pctdutch","pctebike","fastDist","fastTime","fastSlope","geometry")
names(rq) <- c("id","quietDist","quietSlope","quietTime","geometry")
names(l) <- c("id","length","geometry")

#Add Variaibles
l$straightDist <- l$length / 1000
l$length <- NULL
rf$fastDist <- rf$fastDist / 1000

#FIlter Out Low Numbers for Privacy
rf <- rf[rf$all > 2,]
rq <- rq[rq$id %in% rf$id, ]
l <- l[l$id %in% rf$id, ]

#Some Missing Quiet Routes?

#Simplify Geometry
rf <- st_simplify(rf, dTolerance = 10)
rq <- st_simplify(rq, dTolerance = 10)

#Transform to WSG84
rf <- st_transform(rf, 4326)
rq <- st_transform(rq, 4326)
l <- st_transform(l, 4326)

#convert to well known text
rf$geometry <- st_as_text(rf$geometry, digits = 5)
rf <- as.data.frame(rf)

rq$geometry <- st_as_text(rq$geometry, digits = 5)
rq <- as.data.frame(rq)

l$geometry <- st_as_text(l$geometry, digits = 5)
l <- as.data.frame(l)

#Rename Columns
rf$fastGeom <- rf$geometry
rf$geometry <- NULL
rq$quietGeom <- rq$geometry
rq$geometry <- NULL
l$straightGeom <- l$geometry
l$geometry <- NULL

#Join Togther
pct <- left_join(rf,rq, by = c("id" = "id"))
pct <- left_join(pct,l, by = c("id" = "id"))

pct$fastCircuity <- pct$fastDist / pct$straightDist
pct$quietCircuity <- pct$quietDist / pct$straightDist
pct$quietDiversion <- pct$quietDist / pct$fastDist
pct$quietDiverTime <- pct$quietTime / pct$fastTime

# add ID column
pct$idn <- 1:nrow(pct)

# rearrange column
pct <- pct[,c("idn","id","all","onfoot","carorvan","pctcensus","pctgov","pctgen","pctdutch","pctebike",
              "fastDist","fastTime","fastSlope","fastGeom","quietDist","quietSlope","quietTime","quietGeom","straightDist",
              "straightGeom","fastCircuity","quietCircuity","quietDiversion","quietDiverTime")]

#Change names for DB
names(pct) <- c("idn","id","allCommuters","onfoot","carorvan","pctcensus","pctgov","pctgen","pctdutch","pctebike",
                "fastDist","fastTime","fastSlope","fastGeom","quietDist","quietSlope","quietTime","quietGeom","straightDist",
                "straightGeom","fastCircuity","quietCircuity","quietDiversion","quietDiverTime")



write.csv(pct,"../cyipt-bigdata/forDB/pct.csv", row.names = F, na = "")
