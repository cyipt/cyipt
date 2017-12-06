# Export Regions for Database
library(sf)
library(tmap)
tmap_mode("view")


# pcts

pct <- readRDS("../cyipt-securedata/pct-routes-all.Rds")
pct <- pct[,c("ID","all_16p","pct.census","carorvan","av_incline")]

names(pct) <- c("id","total","cyclists","drivers","hilliness","geometry") #Remove punctuation from names for POSTGIS


pct <- pct[pct$total > 2,]
pct <- st_simplify(pct, dTolerance = 10) # 5.3GB unsimplified
pct <- st_transform(pct, 4326)


#Reduce precison of data to reduce file size
pct$geometry <- st_as_binary(pct$geometry, precision = 100000)
pct$geometry <- st_as_sfc(pct$geometry)

qtm(pct[1:10,]) # Check the resutls are ok

#convert to well known text
pct$geotext <- st_as_text(pct$geometry)
pct <- as.data.frame(pct)
pct$geometry <- NULL

pct$idn <- 1:nrow(pct)

pct <- pct[,c("idn","id","total","cyclists","drivers","hilliness","geotext")]

write.csv(pct,"../cyipt-bigdata/forDB/pct.csv", row.names = F, na = "")
