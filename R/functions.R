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
  x$perimeter <- as.numeric(st_length(x, dist_fun = geosphere::distGeo))
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

####################################################################
#OLD CODE ?
#COnvert Multipart Lines/Ploygons to Single Parts as data frame

multi2single <- function(x,from,to){
  #Slit into single part and mulipart
  p_vars <- x[st_geometry_type(x) == to,]
  mp_vars <- x[st_geometry_type(x) == from,]
  #Separate geometry from variaibles
  p_geom <- p_vars$geometry
  mp_geom <- mp_vars$geometry
  #Change to Data Frame
  p_vars <- as.data.frame(p_vars)
  p_vars <- p_vars[,!(names(p_vars) %in% "geometry"), drop = FALSE]
  mp_vars <- as.data.frame(mp_vars)
  mp_vars <- mp_vars[,!(names(mp_vars) %in% "geometry"), drop = FALSE]

  #Duplicate Variables
  mp_vars <- mp_vars[rep(seq_len(nrow(mp_vars)), lengths(mp_geom)),,drop = FALSE]

  #Convert Multipolygons into single polygons
  p_geom <- st_cast(st_sfc(p_geom), to , group_or_split = TRUE)
  mp_geom <- st_cast(st_sfc(mp_geom), to , group_or_split = TRUE)

  #Put back togther
  geom <- c(p_geom,mp_geom)
  geom <- st_cast(st_sfc(geom), to, group_or_split = TRUE) #Incase mp or p is empty have to run again
  vars <- rbind(p_vars,mp_vars)
  vars$geometry <- geom

  #Remove Duplicates
  vars <- vars[!duplicated(vars$geometry),]
  vars <- st_sf(vars)
  return(vars)
}
