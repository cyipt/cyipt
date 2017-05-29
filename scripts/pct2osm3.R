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
#rasterOptions(maxmemory = 1e+10) #10+8 is default
#rasterOptions(tmpdir = "F:/RasterTmp")

#Read in Data
osm <- readRDS("../example-data/bristol/osm_data/osm-split.Rds")
################## Testing This should not be needed
#osm <- osm[!duplicated(osm$geometry),]
#########################


#osm <- osm[as.numeric(st_length(osm))>40,] #Discard short stub lines as cause touble and mostly are not important
lines <- osm[,c("id","geometry")] #Create a working dataset


junctions <- readRDS("../example-data/bristol/osm_data/osm-split-points.Rds")
#osm <- osm[1:100, ] # Testing subset

#Apply Method For Cutting Lines
buff <- st_buffer(junctions, 10)
inter <- st_intersects(lines,buff)

#FUnction to cut lines at points
cutlines <- function(a){
  line_sub <- lines[a,]
  buff_sub <- buff[inter[[a]],]
  if(nrow(buff_sub) == 0){
    line_cut <- line_sub
  }else{
    buff_sub <- st_union(buff_sub)
    line_cut <- st_difference(line_sub, buff_sub)
  }
  return(line_cut)
}

cut_list <- lapply(1:nrow(lines), cutlines)
cut <- do.call("rbind",cut_list)
cut <- cut[!duplicated(cut$geometry),]
#rm(cut_list)

st_write(cut,"../example-data/bristol/for_checking/cut.shp")

#Buffer Lines to small polygons
cut <- as(cut, "Spatial") #Raster does not work with sf so converte back to SP
lines_poly <- gBuffer(cut, byid = T, width = 3)
pct <- raster("../example-data/pct/census-all.tif")
pct <- crop(pct,extent(cut))
vx <- velox(pct)
counts <- vx$extract(sp = lines_poly, fun=mean)
cut$pct_census <- floor(counts[,1])

#COnvert Back to sf
cut <- st_as_sf(cut)

#Convert to df
cut_df <- as.data.frame(cut)
cut_df$geometry <- NULL

#Join Back
osm <- left_join(osm,cut_df, by = c("id" = "id"))


st_write(osm,"../example-data/bristol/for_checking/cut_velox4.shp")
st_write(buff,"../example-data/bristol/for_checking/cut_buffs.shp")
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
