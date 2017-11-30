# Export Regions for Database
library(sf)


# TTWAs

ttwa <- readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
ttwa <- ttwa[,c("objectid","ttwa11cd","ttwa11nm")]

names(ttwa) <- c("id","code","name","geometry") #Remove punctuation from names for POSTGIS

#Reduce precison of data to reduce file size
ttwa$geometry <- st_as_binary(ttwa$geometry, precision = 100000)
ttwa$geometry <- st_as_sfc(ttwa$geometry)

#convert to well known text
ttwa$geotext <- st_as_text(ttwa$geometry)
ttwa <- as.data.frame(ttwa)
ttwa$geometry <- NULL

write.csv(ttwa,"../cyipt-bigdata/forDB/ttwa.csv", row.names = F, na = "")
