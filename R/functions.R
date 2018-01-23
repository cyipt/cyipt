#Useful Functions

##########################################################
#Split a line into segments
#From http://rstudio-pubs-static.s3.amazonaws.com/10685_1f7266d60db7432486517a111c76ac8b.html
SegmentSpatialLines <- function(sl, length = 0, n.parts = 0, merge.last = FALSE) {
  stopifnot((length > 0 || n.parts > 0))
  id <- 0
  newlines <- list()
  sl <- as(sl, "SpatialLines")
  for (lines in sl@lines) {
    for (line in lines@Lines) {
      crds <- line@coords
      # create segments
      segments <- CreateSegments(coords = crds, length, n.parts)
      if (merge.last && length(segments) > 1) {
        # in case there is only one segment, merging would result into error
        segments <- MergeLast(segments)
      }
      # transform segments to lineslist for SpatialLines object
      for (segment in segments) {
        newlines <- c(newlines, Lines(list(Line(unlist(segment))), ID = as.character(id)))
        id <- id + 1
      }
    }
  }
  return(SpatialLines(newlines))
}

############################################################################
#Function to split mulitple geomaties into single geometires when a data frame
splitmulti <- function(x,from,to){
  #Split into single and mulit
  mp_var <- x[st_geometry_type(x) == from,]
  p_var <- x[st_geometry_type(x) == to,]
  #Separate out geometry
  mp_geom <- mp_var$geometry
  p_geom <- p_var$geometry
  #Remove geometry
  mp_var$geometry <- NULL
  p_var$geometry <- NULL
  #Convert to data frame
  mp_var <- as.data.frame(mp_var)
  p_var <- as.data.frame(p_var)
  #Split geometrys
  mp_geom_s <- st_cast(st_sfc(mp_geom), to, group_or_split = TRUE)
  p_geom <- st_cast(st_sfc(p_geom), to, group_or_split = TRUE)
  #Duplicate variaibles for multis
  if(to == "POINT"){
    len <- lengths(mp_geom)/2
  }else if(to == "LINESTRING"){
    len <- lengths(mp_geom)
  }else if(to == "POLYGON"){
    len <- lengths(mp_geom)
  }else{
    print("Can't do this")
    stop()
  }
  mp_var <- mp_var[rep(seq_len(nrow(mp_var)), len),,drop=FALSE]
  #Put polygons back togther
  geom <- c(p_geom,mp_geom_s)
  var <- rbind(p_var,mp_var)
  geom <- st_cast(st_sfc(geom), to, group_or_split = TRUE) #Incase mp or p is empty have to run again
  var$geometry <- geom
  res <- st_as_sf(var)
  return(res)
}

###################################################################
#Calcualte Widths
width_estimate <- function(x){
  x$area <- as.numeric(st_area(x))
  x$perimeter <- as.numeric(st_length(x))
  res <- mapply(FUN = width_apply,x$area, x$perimeter, SIMPLIFY = FALSE)
  return(res)
}

width_apply <- function(a,p){
  if((p ^ 2) > (16 * a)){
    width <- round(0.25 * (p - sqrt((p ^ 2) - (16 * a))), 1)
  }else{
    width <- round(2 * a / p, 1)
  }
  return(width)
}

###########################################################################
# Check if two lines overlap, with some tolerance

roadsOnLine <- function(roads,line2check, tolerance = 4){
  # CHeck for CRS Match
  line2check <- st_sfc(line2check)
  if(!st_crs(roads) == st_crs(line2check)){
    message("CRS do not match")
    stop()
  }

  if(nrow(roads) == 0 | length(line2check) == 0){
    message("Invalid Input Data")
    return(NULL)
  }else{
    #roads <- st_sfc(roads)
    roads <- roads[,c("id")]
    roads$length <- as.numeric(st_length(roads)) #Road Length
    roads$npoints <- lengths(roads$geometry) / 2 #Number of points that make up road
    roads$pointsget <- ifelse(roads$npoints > 2,3,2)
    roads$buff <- ifelse(roads$length > tolerance * 2,tolerance,roads$length/2.5) #Buffer to tolerance unless road is short

    points <- st_cast(roads, "POINT") #convert road to points
    points$occurance <- ave(points$id, points$id, FUN = seq_along) #find out which point i

    points <- points[(points$occurance == 1 |
                        points$occurance == points$npoints |
                        points$occurance == ceiling(points$npoints/2)),] #Get first last and middle points

    points.buff <- st_buffer(points, dist = points$buff, nQuadSegs = 4)
    #qtm(line2check) + qtm(points.buff)
    #qtm(roads)
    points.buff <- points.buff[st_intersects(line2check,points.buff)[[1]],]

    roads$nmatch <- sapply(roads$id, function(x) {sum(points.buff$id == x)})

    res <- roads$id[roads$nmatch == roads$pointsget]
    #res <- roads[roads$nmatch == roads$pointsget,]
    #qtm(line2check, lines.lwd = 5, lines.col = "black") + qtm(roads, lines.lwd = 3) + qtm(res, lines.col = "green", lines.lwd = 3)
    return(res)
  }
}

