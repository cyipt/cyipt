#Add PCT values to OSM lines
#Quite slow about 1h for bristol

library(sf)
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(dplyr)
library(utils)
library(velox)



#Setting to boost performance
rasterOptions(maxmemory = 1e+10) #10+8 is default
rasterOptions(tmpdir = "F:/RasterTmp")

#Read in Data
osm <- readRDS("../example-data/bristol/osm_data/roads_osm.Rds")
osm <- osm[as.numeric(st_length(osm))>40,] #Discard short stub lines as cause touble and mostly are not important
osm <- osm[,c("id","geometry")] #Dump unneede data
#osm <- osm[1:100, ] # Testing subset

#Remove Ends of Lines
total <- nrow(osm)
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(a in 1:total){
  line <- osm$geometry[a] #Get the line
  points <- st_cast(line, "POINT") # convert to points
  ends <- points[c(1,length(points))] # take first and last point
  dist <- as.numeric(st_distance(ends[1],ends[2]))
  if(!identical(ends[[1]],ends[[2]])){ #Roundabouts need to be treaded differently so skip
    if(dist > 40){ #Any small bits buffer by smaller area
      buff <- st_buffer(ends, dist = 20) # buffer points by 5m
    }else{
      buff <- st_buffer(ends, dist = dist/2.1)
    }
    buff <- st_cast(buff, "MULTIPOLYGON", group_or_split = TRUE, ids = c(1,1))# make buffer into a single mulitpolygon
    osm$geometry[a] <- st_difference(line,buff)# subtract the difference from the line and overwrite the geometry
  }
  setTxtProgressBar(pb, a)
}
close(pb)

#Buffer Lines to small polygons
osm <- as(osm, "Spatial") #Raster does not work with sf so converte back to SP
osm_poly <- gBuffer(osm, byid = T, width = 1)
pct <- raster("../example-data/pct/census-all.tif")
pct <- crop(pct,extent(osm))
vx <- velox(pct)
test <- vx$extract(sp = osm_poly, fun=mean)
osm$censu2 <- test[,1]
writeOGR(osm,"../example-data/bristol/for_checking",layer = "veloxpct",driver = "ESRI Shapefile", overwrite_layer = T)

stop()




osm <- as(osm, "Spatial") #Raster does not work with sf so converte back to SP
pct <- raster("../example-data/pct/census-all.tif")
pct <- crop(pct,extent(osm))

print(paste0("Starting extraction of ",nrow(osm)," lines at ", Sys.time()))
beginCluster(6, type='SOCK', exclude = c("sf","tmap")) #Extra Raster Cell Values Unsing a parallel cluster
ext <- extract(pct,osm, buffer = 2.5, fun = mean, df = T, cellnumbers = T)
endCluster()
print(paste0("Finished extraction at ", Sys.time()))

ext$census.all <- floor(ext$census.all)#Round Values Down toi nearest whole number
ext$ID <- osm$id #Copy Original ID numbers across as they are lost in the extract process
rm(osm)
plot()
osm <- readRDS("../example-data/bristol/osm_data/roads_osm.Rds") #Read in clean osm data
osm <- left_join(x = osm,y =  ext, by = c("id" = "ID")) #Join in the PCT values

#Remove NAs
for(b in 1:nrow(osm)){
  if(is.na(osm$census.all[b])){
    osm$census.all[b] <- 0
  }
}


saveRDS(osm,"../example-data/bristol/osm_data/roads_osm_pct_2011.Rds")
