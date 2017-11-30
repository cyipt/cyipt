# Export Regions for Database
library(sf)


# pcts

pct <- readRDS("../cyipt-securedata/pct-routes-all.Rds")
pct <- pct[,c("ID","all_16p","pct.census","carorvan","av_incline")]

names(pct) <- c("id","total","cyclists","drivers","hilliness","geometry") #Remove punctuation from names for POSTGIS

#Reduce precison of data to reduce file size
pct$geometry <- st_as_binary(pct$geometry, precision = 100000)
pct$geometry <- st_as_sfc(pct$geometry)

#convert to well known text
pct$geotext <- st_as_text(pct$geometry)
pct <- as.data.frame(pct)
pct$geometry <- NULL

pct$idn <- 1:nrow(pct)

pct <- pct[,c("idn","id","total","cyclists","drivers","hilliness","geotext")]

write.csv(pct,"../cyipt-bigdata/forDB/pct.csv", row.names = F, na = "")
