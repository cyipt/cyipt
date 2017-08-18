library(stringr)
library(sf)

map <- readRDS("../cyipt-bigdata/osm-prep/BristolCityof/osm-lines.Rds")
#map <- map[1:1000,]
map <- st_transform(map, 4326) #convert to lat lngs for leaflet

#remove punctuation in column names
names(map) <- str_replace_all(names(map),"[[:punct:]]","")


#convert to well known text
map <- as.data.frame(map)
wnt <- st_as_text(map$geometry)
map$geometry <- NULL
map$geotext <- wnt

#put id at front
col.names <- names(map)[names(map) != "id"]
map <- map[,c("id",col.names)]


write.csv(map,"../cyipt/postgres-test/testdata.csv",row.names = F)

