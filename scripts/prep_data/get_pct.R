#Add PCT values to OSM lines
#Now fast about 10 min for bristol

library(sf)
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(dplyr)
library(utils)
library(velox)


#Read in Data
osm <- readRDS("../example-data/bristol/osm_data/osm-split.Rds")
lines <- osm[,c("id","geometry")] #Create a working dataset
junctions <- readRDS("../example-data/bristol/osm_data/osm-split-points.Rds")
#osm <- osm[1:100, ] # Testing subset

#Cut off 10m around junctions so that iccorect pct values doe not get applied to lines
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

#st_write(cut,"../example-data/bristol/for_checking/cut.shp")

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
osm$pct_census[is.na(osm$pct_census)] <- 0
saveRDS(osm,"../example-data/bristol/osm_data/osm-split-pct-2011.Rds")
