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
osm <- readRDS("../example-data/bristol/results/osm-lines.Rds")
lines <- osm[,c("id","geometry")] #Create a working dataset
junctions <- readRDS("../example-data/bristol/results/junction-points.Rds")

#osm <- osm[1:100, ] # Testing subset
bounds <- st_read("../example-data_old/bristol/mini_bristol.shp")
bounds <- st_transform(bounds, 27700)
osm <- osm[bounds,]
lines <- lines[bounds,]
junctions <- junctions[bounds,]

#Looking for lines that are trimmed at both ends out of existance
#buff <- st_buffer(junctions, 10)
#inter <- st_intersects(lines,buff)
#buff_overlap <- st_intersects(buff)
#buff_overlap <- buff_overlap[lengths(buff_overlap) > 1]
#buff_overlap <- unlist(buff_overlap)
#buff_overlap <- buff_overlap[!duplicated(buff_overlap)]
#buff_over <- buff[buff_overlap,]
#buff_over <- st_union(buff_over, by_feature = F)
#buff_over <- st_cast(buff_over,"POLYGON")

#trimmed <- unlist(st_contains(buff_over,lines))
#lines_trim <- lines[trimmed,]




#Cut off 10m around junctions so that iccorect pct values doe not get applied to lines
#Apply Method For Cutting Lines


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


#Add back in lines that have been completly removed by the buffer
missing <- lines$id[!(lines$id %in% cut$id)]
missing_lines <- lines[lines$id %in% missing,]
cut <- rbind(cut,missing_lines)

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
res <- left_join(osm,cut_df, by = c("id" = "id"))
res2 <- res

#Check for matching osm_id with a pct value
for(a in 1:nrow(res)){
  if(is.na(res$pct_census[a])){
    sts <- res$pct_census[a]
    chng <- round(mean(res$pct_census[res$osm_id == res$osm_id[a]], na.rm = TRUE),0)
    res$pct_census[a] <- chng
    print(paste0("changed line ", a," from ",sts," to ",chng))
  }
}

stop()
res$pct_census[is.nan(res$pct_census)] <- 0

vals <- as.data.frame(res)
vals <- vals[,c("id","osm_id","pct_census")]

write.csv(vals,"../example-data/bristol/results/pct-census.csv")


