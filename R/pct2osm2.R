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
osm <- readRDS("../example-data/bristol/osm_data/osm-split.Rds")
################## Testing This should not be needed
osm <- osm[!duplicated(osm$geometry),]
#########################


#osm <- osm[as.numeric(st_length(osm))>40,] #Discard short stub lines as cause touble and mostly are not important
lines <- osm[,c("id","geometry")] #Create a working dataset


junctions <- readRDS("../example-data/bristol/osm_data/osm-split-points.Rds")
#osm <- osm[1:100, ] # Testing subset

#Remove Lines around Junctions
#buff <- st_buffer(junctions, 20)
#buff2 <- aggregate(buff, by = as.list(rep(1,nrow(buff))), FUN = sum)
#buff3 <- st_combine(buff)
#buff2 <- st_union(buff)
#diff <- st_difference(lines, buff) #about 5 min for 2000 lines
#line_buff <- st_buffer(lines, 1)
#diff2 <- st_difference(line_buff, buff)
#diff3 <- st_difference(line_buff, buff)
#diff2 <- diff2[!duplicated(diff2$geometry),]
#diff <- st_difference(lines, buff)
#diff <- diff[!duplicated(diff$geometry),]
#st_write(buff,"../example-data/bristol/for_checking/buff2.shp")
#st_write(lines,"../example-data/bristol/for_checking/lines.shp")
#st_write(diff,"../example-data/bristol/for_checking/diff.shp")
#st_write(diff2,"../example-data/bristol/for_checking/diff2.shp")

#Start Again With a loop
#Some lines are lost if they are totally inside the buffer
inter <- st_intersects(lines,buff)
cut <- lines[0,]
pb <- txtProgressBar(min = 0, max = nrow(lines), style = 3)
for(a in 1:nrow(lines)){
  setTxtProgressBar(pb, a)
  line_sub <- lines[a,]
  buff_sub <- buff[inter[[a]],]
  #plot(line_sub[1], col = "Black", lwd = 3)
  #plot(buff_sub[1], add = T)
  if(nrow(buff_sub) == 0){
    line_cut <- line_sub
  }else{
    buff_sub <- st_union(buff_sub)
    line_cut <- st_difference(line_sub, buff_sub)
  }
  #plot(line_cut[1], col = "Red", lwd = 2)
  cut <- rbind(cut,line_cut)
}
close(pb)
cut <- cut[!duplicated(cut$geometry),]
st_write(cut,"../example-data/bristol/for_checking/cut.shp")



#Buffer Lines to small polygons
cut <- as(cut, "Spatial") #Raster does not work with sf so converte back to SP
lines_poly <- gBuffer(cut, byid = T, width = 1)
pct <- raster("../example-data/pct/census-all.tif")
pct <- crop(pct,extent(cut))
vx <- velox(pct)
counts <- vx$extract(sp = lines_poly, fun=mean)
cut$pct_census <- floor(counts[,1])

#COnvert Back to sf
cut <- st_as_sf(cut)
st_write(cut,"../example-data/bristol/for_checking/cut_velox.shp")

#writeOGR(lines,"../example-data/bristol/for_checking",layer = "veloxpct",driver = "ESRI Shapefile", overwrite_layer = T)




#










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
