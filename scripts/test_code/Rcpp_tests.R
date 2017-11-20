# RCPP test

library("Rcpp")
library(sf)
cppFunction("
bool isOddCpp(int num = 10) {
bool result = (num % 2 == 1);
return result;
}")
isOddCpp(42L)

osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/Bristol/osm-lines.Rds"))







roadsOnLine <- function(roads,line2check, tolerance = 4){
  roads <- st_sfc(roads)
  line2check <- st_sfc(line2check)
  st_crs(roads) <- st_crs(line2check) # Assuming that they are the same as crs for roads get lost in lapply
  points <- st_cast(roads, "POINT") #convert road to points
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

  len <- sqrt((points[[1]][1] - points[[3]][1])**2 + (points[[1]][2] - points[[3]][2])**2) # for sort distances on projected coordinates faster than st_distance with same answer

  #message(paste0("len = ", len))
  if(len < (tolerance * 2) & len != 0){ # To hanel very small lines
    cutlen <- len/2.5
  }else{
    cutlen <- tolerance
  }
  buff <- st_buffer(points, cutlen, nQuadSegs = 2) #Make small circles around the points # Reduce number of segments for speed
  if(p3){
    buff[2] <- st_buffer(points[2], cutlen/1.2, nQuadSegs = 2) # replace middle buffer with smaller value but only if it is a unique point
  }

  #Check that lines intersect with all three points
  #sel <- st_intersects(buff, line)
  sel <- st_intersects(buff, line2check)
  sel.first <- sel[[1]]
  sel.middle <- sel[[2]]
  sel.last <- sel[[3]]

  sel.all <- sel.first[sel.first %in% sel.last]
  sel.all <- sel.all[sel.all %in% sel.middle]

  if(length(sel.all) == 0){
    return(FALSE)
  }else{
    return(TRUE)
  }

}
