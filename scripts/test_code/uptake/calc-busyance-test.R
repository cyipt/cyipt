#Can we recaluate buysance scoare reliably?

library(sf)
library(dplyr)
library(tmap)
tmap_mode("view")


# get some data
region <- "BristolCityof"
osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",region,"/osm-lines.Rds"))

#Get bounding box
ext <- st_bbox(osm)
ext <- st_sfc(st_polygon(list(rbind(c(ext[1],ext[2]),c(ext[3],ext[2]),c(ext[3],ext[4]),c(ext[1],ext[4]),c(c(ext[1],ext[2]))))) )
pol <- data.frame(id = 1, geometry = NA)
st_geometry(pol) <- ext
rm(ext)

#Get pct data and subset to bounding box
pct.all <- readRDS("../cyipt-securedata/pct-routes-all.Rds")
st_crs(pol) <- st_crs(pct.all) #For some reason the CRS are fractionally different
pct.all <- pct.all[pol,]
pct.all <- st_transform(pct.all, st_crs(osm)) #transfor so that crs are idetical

#join in the quietness scores
quiet <- read.csv("input-data/scorings/bristol.csv", stringsAsFactors = F)
quiet <- quiet[,c("id","quietness")]

osm$osm_id <- as.integer(as.character(osm$osm_id))
osm <- left_join(osm,quiet, by = c("osm_id" = "id"))

#calc busyness score
osm$busy <- osm$length / (osm$quietness/100)


#Pick a line to test on
line <- pct.all[501,]
qtm(line)

#get intersectin roads
line.buff <- st_buffer(line,4)
line.buff <- line.buff[,"ID"]
osm.sub <- osm[st_intersects(line.buff,osm)[[1]],]


#always end up loosing the start and end roads so clip the raods to the buffer
osm.sub <- st_intersection(line.buff, osm.sub)
osm.sub$ID <- 1:nrow(osm.sub)

qtm(line, lines.lwd = 5, lines.col = "black") +
  qtm(osm.sub, lines.lwd = 3, lines.col = "green") #+
#  qtm(osm, lines.lwd = 1, lines.col = "red")


#Check for 3 point intersection
#match <- list()
match = vector(mode = "logical", length = nrow(osm.sub))

for(a in 1:nrow(osm.sub)){
  points <- st_cast(osm.sub$geometry[a], "POINT") #convert road to points
  points.len <- length(points)
  if(points.len >= 3){
    #Get first last and a middle point
    points <- points[c(1,ceiling(points.len/2),points.len)]
    p3 <- TRUE
  }else if(points.len == 2){
    #Need 3 points so double get the last point
    points <- points[c(1,1,points.len)]
    p3 <- FALSE
  }else{
    #Somethign has gone wrong
    warning(paste0("Line ",a," is made up of less than two points. Points =  ",points.len))
    stop()
  }

  #Buffer points
  len <- as.numeric(st_distance(points[1],points[3])) #Change to distance between points to deal with curved roads
  if(len < 4 & len != 0){ # To hanel very small lines
    cutlen <- len/2.5
  }else{
    cutlen <- 2
  }
  buff <- st_buffer(points, cutlen) #Make small circles around the points
  if(p3){
    buff[2] <- st_buffer(points[2], 1) # replace middle buffer with smaller value but only if it is a unique point
  }


  #Check that lines intersect with all three points
  sel.first <- st_intersects(buff[1], line)[[1]]
  sel.middle <- st_intersects(buff[2], line)[[1]]
  sel.last <- st_intersects(buff[3], line)[[1]]
  sel.all <- sel.first[sel.first %in% sel.last]
  sel.all <- sel.all[sel.all %in% sel.middle]

  if(length(sel.all) == 0){
    match[a] <- FALSE
  }else{
    match[a] <- TRUE
  }

}

#match <- unlist(match)
osm.sub2 <- osm.sub[match,]

qtm(line, lines.lwd = 7, lines.col = "black") +
  qtm(osm.sub, lines.lwd = 5, lines.col = "red") +
    qtm(osm.sub2, lines.lwd = 3, lines.col = "blue")


st_length(line)
sum(st_length(osm.sub2))

print(paste0("Got ",round(sum(st_length(osm.sub2))/st_length(line)*100,2),"% of the line length"   ))

line$busyness[1]
sum(osm.sub2$busy, na.rm = T)
print(paste0("Got ",round(sum(osm.sub2$busy, na.rm = T)/line$busyness[1]*100,2),"% of the line busyance"   ))

