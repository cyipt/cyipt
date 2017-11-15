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

###########################################################################
# Check if two lines overlap, with some tolerance

roadsOnLine <- function(roads,line2check, tolerance = 4){
  # CHeck for CRS Match
  line2check <- st_sfc(line2check)
  if(!st_crs(roads) == st_crs(line2check)){
    message("CRS do not match")
    stop()
  }

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


###########################################################################
# Based on https://www.spatialanalytics.co.nz/post/2017/09/11/a-parallel-function-for-spatial-analysis-in-r/


# Paralise any simple features analysis.
st_par <- function(sf_df, sf_func, n_cores, ...){

  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))

  # Perform GIS analysis
  split_results <- split(sf_df, split_vector) %>%
    #mclapply.hack(function(x) sf_func(x, ...), mc.cores = n_cores)
    mclapply.hack(function(x) sf_func(x, ...))

  # Combine results back together. Method of combining depends on the output from the function.
  if ( class(split_results[[1]]) == 'list' ){
    result <- do.call("c", split_results)
    names(result) <- NULL
  } else {
    result <- do.call("rbind", split_results)
  }

  # Return result
  return(result)
}






########################################################################
# from https://www.r-bloggers.com/implementing-mclapply-on-windows-a-primer-on-embarrassingly-parallel-computation-on-multicore-systems-with-r/
# allows mclapply to run on windows

##
## mclapply.hack.R
##
## Nathan VanHoudnos
## nathanvan AT northwestern FULL STOP edu
## July 14, 2014
##
## A script to implement a hackish version of
## parallel:mclapply() on Windows machines.
## On Linux or Mac, the script has no effect
## beyond loading the parallel library.


## Define the hack
mclapply.hack <- function(...) {
  ## Create a cluster
  size.of.list <- length(list(...)[[1]])
  cl <- makeCluster( min(size.of.list, detectCores()) )

  ## Find out the names of the loaded packages
  loaded.package.names <- c(
    ## Base packages
    sessionInfo()$basePkgs,
    ## Additional packages
    names( sessionInfo()$otherPkgs ))

  tryCatch( {

    ## Copy over all of the objects within scope to
    ## all clusters.
    this.env <- environment()
    while( identical( this.env, globalenv() ) == FALSE ) {
      clusterExport(cl,
                    ls(all.names=TRUE, env=this.env),
                    envir=this.env)
      this.env <- parent.env(environment())
    }
    clusterExport(cl,
                  ls(all.names=TRUE, env=globalenv()),
                  envir=globalenv())

    ## Load the libraries on all the clusters
    ## N.B. length(cl) returns the number of clusters
    parLapply( cl, 1:length(cl), function(xx){
      lapply(loaded.package.names, function(yy) {
        require(yy , character.only=TRUE)})
    })

    ## Run the lapply in parallel
    return( parLapply( cl, ...) )
  }, finally = {
    ## Stop the cluster
    stopCluster(cl)
  })
}

## Warn the user if they are using Windows
if( Sys.info()[['sysname']] == 'Windows' ){
  message(paste(
    "\n",
    "   *** Microsoft Windows detected ***\n",
    "   \n",
    "   For technical reasons, the MS Windows version of mclapply()\n",
    "   is implemented as a serial function instead of a parallel\n",
    "   function.",
    "   \n\n",
    "   As a quick hack, we replace this serial version of mclapply()\n",
    "   with a wrapper to parLapply() for this R session. Please see\n\n",
    "     http://www.stat.cmu.edu/~nmv/2014/07/14/implementing-mclapply-on-windows \n\n",
    "   for details.\n\n"))
}

## If the OS is Windows, set mclapply to the
## the hackish version. Otherwise, leave the
## definition alone.
mclapply <- switch( Sys.info()[['sysname']],
                    Windows = {mclapply.hack},
                    Linux   = {mclapply},
                    Darwin  = {mclapply})

## end mclapply.hack.R



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
