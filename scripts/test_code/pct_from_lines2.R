#Need to fix
#Roads that touch the same road at both ends still get the incorect PCT value


library(sp)
library(sf)
library(rgdal)
library(geojsonio)
library(dplyr)
library(tmap)
library(parallel)

source("R/functions.R")

tmap_mode("view")
#Playing with routes
bounds <- st_read("../cyipt/areas/bristol-poly.geojson")
bounds <- as(bounds,"Spatial")
bounds <- spTransform(bounds, CRS("+proj=longlat +init=epsg:3857 +a=6378137 +b=6378137 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
rf <- readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_4plus_fix.Rds")
rf@data <- rf@data[,c("ID","busyness")]
rf <- rf[bounds,]
rf <- spTransform(rf,CRS("+init=epsg:27700"))
gc()
rf <- st_as_sf(rf)

flow <- readRDS("../pct-lsoa/Data/02_Input/LSOA_flow.Rds")
flow <- flow[,c("id","bicycle_16p")]
flow <- flow[flow$id %in% rf$ID,]

rf <- left_join(rf,flow, by = c("ID" = "id"))
rf <- rf[rf$bicycle_16p > 0,]
rm(flow)

osm <- readRDS("../example-data/bristol/results/osm-lines.Rds")
rf <- st_transform(rf, st_crs(osm))


#Performacne Tweak, Preallocate object to a gid to reduce processing time
grid <- st_make_grid(osm, n = c(100,100), "polygons")
grid_osm <- st_intersects(osm, grid) # Which grid is each osm line in?
grid_rf <- st_intersects(grid, rf)# for each grif which rf lins cross it
rm(grid)


getpctvalues <- function(a){
  osm_sub <- osm[a,]
  #Trim off the ends of the line
  points <- st_cast(osm_sub$geometry, "POINT")
  points <- points[c(1,length(points))] #Get first and last point on the lines
  len <- as.numeric(st_length(osm_sub))
  if(len < 1){ # To hanel very small lines
    cutlen <- len/2.1
  }else{
    cutlen <- 0.5
  }
  buff <- st_buffer(points, cutlen) #Make small circiels around the ends
  buff <- st_union(buff)
  osm_sub <- st_difference(osm_sub, buff) # Cut off the ends
  #rm(points,buff, len, cutlen)
  #Get grid IDS
  gridid <- grid_osm[a][[1]]
  rf_grid <- grid_rf[gridid]
  rf_grid <- unlist(rf_grid)
  rf_grid <- rf_grid[!duplicated(rf_grid)]
  rf_presub <- rf[rf_grid,]
  sel <- st_intersects(osm_sub, rf_presub)[[1]]

  if(sum(lengths(sel)) == 0){
    #SOmething the lines run paralelle very colse to each other
    #split the buff and check for intersection at both ends
    buff <- st_cast(buff,"POLYGON", group_or_split = T)
    if(length(buff) == 1){
      #Do nothing, edge case where a looped road exists whith same start and end point
      count <- 0
    }else{
      sel2 <- st_intersects(buff[1], rf_presub)[[1]]
      sel3 <- st_intersects(buff[2], rf_presub)[[1]]
      sel4 <- sel2[sel2 %in% sel3]
      if(sum(lengths(sel4)) == 0){
        #Do Nothing
        count <- 0
      }else{
        #Check for cul-de-sacs where road toched the same road at both ends
        grd <- grid_osm[[a]]
        osm_other <- osm[grid_osm %in% grd,]
        osm_other1 <- osm_other[unique(unlist(st_intersects(buff[1],osm_other))),]
        osm_other2 <- osm_other[unique(unlist(st_intersects(buff[2],osm_other))),]
        osm_other1 <- osm_other1[!(osm_other1$id %in% a),]
        osm_other2 <- osm_other2[!(osm_other2$id %in% a),]
        match <- osm_other1$id[osm_other1$id %in% osm_other2$id]
        if(length(match) == 0){
          #No cul-de-sac case
          lenother <- 0
        }else{
          lenother <- as.numeric(st_length(osm_other[osm_other$id == match,]))
        }
        #plot(osm_other1[1], add = T, col = "Green", lwd = 3)
        #plot(osm_other2[1], add = T, col = "Yellow", lwd = 3)
        if(lenother > len){
          #Cul-de-sac
          #Do Nothing
          count <- 0
        }else{
          #Split out the lines that are very close
          rf_sub <- rf_presub[sel4,]
          cuts <- st_difference(rf_sub,buff)
          cutsl <- splitmulti(cuts, "MULTILINESTRING", "LINESTRING")
          cutsl$len <- as.numeric(st_length(cutsl))
          cutsl <- cutsl[cutsl$len > (0.95 * len) & cutsl$len < (1.05 * len),] #Get segments that are withing 5% lenf of the line
          count <- sum(rf_sub$bicycle_16p)
          #lengths(cuts_geom)
          #plot(rf_sub, add = T, lwd = 2, col = "Green")
        }
      }
      rm(buff,sel2,sel3,sel4)
    }

  }else{
    #plot(rf_presub[sel,], add = T, lwd = 2, col = "Green")
    rf_sub <- rf_presub[sel,]
    count <- sum(rf_sub$bicycle_16p)
  }
  return(count)
}

rm(bounds)

m = 1
n = nrow(osm)

###########################################################
# Serial Version of Code
#start <- Sys.time()
#profvis({
#res <- lapply(1:n,getpctvalues)
#res <- unlist(res)
#})
#end <- Sys.time()
#print(paste0("Did ",n," lines in ",difftime(end,start,units = "secs")," in serial mode"))
##########################################################


##########################################################
#Parallel
start <- Sys.time()
fun <- function(cl){
  parLapply(cl, m:n,getpctvalues)
}
cl <- makeCluster(5)
clusterExport(cl=cl, varlist=c("osm", "rf","grid_osm","grid_rf"))
clusterEvalQ(cl, {library(sf); source("R/functions.R")}) #; {splitmulti()}) #Need to load splitmuliin corectly
respar <- fun(cl)
stopCluster(cl)
respar <- unlist(respar)
end <- Sys.time()
print(paste0("Did ",n," lines in ",difftime(end,start,units = "secs")," in parallel mode"))
#identical(res,respar)
##########################################################

#osm_test <- osm[m:n,]
#osm_test$pct_census <- respar
#rm(respar)

########



osm$pct_census <- respar # CHange to res for serial version

sub <- osm[osm$pct_census > 0,]
#sub2 <- osm[osm$pct_census == 0,]
#qtm(sub, lines.col = "pct_census", lines.lwd = 6, popup.vars = c("id","osm_id","pct_census") )

tm_shape(sub)+
  tm_lines(col = "pct_census",lwd = 6, popup.vars = c("id","osm_id","pct_census"))# +
#tm_shape(sub2)+
#  tm_lines(col = "black",lwd = 4, popup.vars = c("id","osm_id","pct_census"))


#qtm(rf_presub) +
#  qtm(osm_sub, lines.lwd = 5, lines.col = "black")


#rm(respar)
#osm$pct_census <- NULL
#a = 19
#osm_sub <- osm[a,]
#sel <- st_intersects(osm_sub, rf)[[1]]
#rf_sub <- rf[sel,]
#qtm(rf_sub[1], lines.col = "black") +
#  qtm(osm_sub[1], lines.col = "red", lines.lwd = 3)

#plot(osm_sub[1], col = "Black", lwd = 3)
#plot(rf_presub[1], col = "Red", add = T)
