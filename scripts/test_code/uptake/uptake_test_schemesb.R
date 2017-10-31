# Test of calcualting uptake for each scenario in bristol

# set up
library(sf)
library(dplyr)
library(tmap)
library(pbapply)
library(parallel)
source("R/functions.R")
tmap_mode("view")
region <- "BristolCityof"
osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",region,"/osm-lines.Rds"))


#Functions
roadsOnLine2 <- function(roads,line2check){
  #points <- st_cast(osm.sub$geometry[a], "POINT") #convert road to points
  #message(paste0("Class of roads is = ", class(roads)))
  #message(paste0("Class of line2check is = ", class(line2check)))
  roads <- st_sfc(roads)
  line2check <- st_sfc(line2check)
  st_crs(roads) <- st_crs(line2check) # Assuming that they are the same as crs for roads get lost in lapply
  points <- st_cast(roads, "POINT") #convert road to points
  points.len <- length(points)
  #message(paste0("Points.len = ", points.len))
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
  #message(paste0("Class of points is = ", class(points)))
  #message(paste0("p3 = ", p3))
  #message(paste0("Points lenght is now = ", length(points)))
  #message(points)
  #Buffer points
  #len <- as.numeric(st_distance(points[1],points[3])) #Change to distance between points to deal with curved roads
  len <- sqrt((points[[1]][1] - points[[3]][1])**2 + (points[[1]][2] - points[[3]][2])**2) # for sort distances on projected coordinates faster than st_distance with same answer

  #message(paste0("len = ", len))
  if(len < 8 & len != 0){ # To hanel very small lines
    cutlen <- len/2.5
  }else{
    cutlen <- 4
  }
  buff <- st_buffer(points, cutlen, nQuadSegs = 2) #Make small circles around the points # Reduce number of segments for speed
  if(p3){
    buff[2] <- st_buffer(points[2], 2, nQuadSegs = 2) # replace middle buffer with smaller value but only if it is a unique point
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


##########################################################################################
calcChangeBusy <- function(k){
  line <- pct.scheme[k,]
  #qtm(line)

  #get intersectin roads
  line.buff <- st_buffer(line,4,nQuadSegs = 2)
  line.buff <- line.buff[,"ID"]
  osm.sub <- osm[st_intersects(line.buff,osm)[[1]],]

  #always end up loosing the start and end roads so clip the raods to the buffer
  osm.sub <- st_intersection(line.buff, osm.sub)
  osm.sub$ID <- 1:nrow(osm.sub)

  #Check for 3 point intersection
  #match = sapply(1:nrow(osm.sub), roadsOnLine)
  match = mapply(roadsOnLine2,osm.sub$geometry,line$geometry) #internatised the function
  #match <- unlist(match)
  osm.sub2 <- osm.sub[match,]

  # CHang in busyness
  busyance.new <- vector(mode= "numeric", length = nrow(osm.sub2))
  for(l in 1:nrow(osm.sub2)){
    #Check for NA first
    if(is.na(osm.sub2$group_id[l])){
      busyance.new[l] <- osm.sub2$busy[l]
    }else if(osm.sub2$group_id[l] == j){
      busyance.new[l] <- osm.sub2$busyAfter[l]
    }else{
      busyance.new[l] <- osm.sub2$busy[l]
    }
  }

  s1 <- sum(osm.sub2$busy, na.rm = T)
  s2 <-  sum(busyance.new, na.rm = T)

  #message(paste0("Line ",k," has had its busyance changed by ",round((s1-s2)/s1 * 100,2)," %" ))
  result <- data.frame(id = line$ID, before = s1, after = s2)
  return(result)

}


###################################################################
# Move this to the get_pct and save out each regions resutls

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
rm(pol)

#######################################################################


##############################################################
# Repace with CyiPT specific busyness calucaltion


#join in the quietness scores
quiet <- read.csv("input-data/scorings/bristol.csv", stringsAsFactors = F)
quiet <- quiet[,c("id","quietness")]

osm$osm_id <- as.integer(as.character(osm$osm_id))
osm <- left_join(osm,quiet, by = c("osm_id" = "id"))

#calc busyness score
osm$busy <- osm$length / (osm$quietness/100)
rm(quiet)

####################################################

#Update Busyness scaore based on Reccomended infra
# simple test make quietness always equal to 100
osm$busyAfter <- osm$busy
for(i in 1:nrow(osm)){
  if(osm$Recommended[i] != "None"){
    osm$busyAfter[i] <- osm$length[i]
  }
}


#get the list of scheme_nos
scheme_nos <- unique(osm$group_id)
scheme_nos <- scheme_nos[!is.na(scheme_nos)]
scheme_nos <- scheme_nos[order(scheme_nos)]

osm.nona <- osm[!is.na(osm$group_id),]
# Loop over schemse
uptake.list <- list()

for(j in scheme_nos){
  #Get the roads in the schemes
  osm.scheme <- osm.nona[osm.nona$group_id == j,]

  #create a single row dataframe buffer of the whole scheme
  osm.scheme.buf <- st_buffer(osm.scheme, 1)
  osm.scheme.buf <- osm.scheme.buf[,c("id")]
  buf.union <- st_union(osm.scheme.buf)
  osm.scheme.buf <- osm.scheme.buf[1,] # Take the first row of the df
  osm.scheme.buf$geometry <- buf.union # insert the union geomtery

  # Get the PCT lines that intersect the scheme
  pct.scheme <- pct.all[unique(unlist(st_intersects(osm.scheme.buf, pct.all))),]

  #check the interesctions
  inter <- st_intersection(osm.scheme.buf, pct.scheme)
  inter <- splitmulti(inter, "MULTILINESTRING", "LINESTRING")

  #remove interecsion less than 10 m
  # this removes routes that just cross the scheme or onyl travel of it for trival part of the journey
  #It also massivel reduced the number of lines to worry about
  inter$inter_length <- as.numeric(st_length(inter))
  inter <- inter[inter$inter_length > 10,]
  pct.scheme <- pct.scheme[pct.scheme$ID %in% unique(inter$ID),]
  rm(inter)

  #qtm(osm.scheme.buf) +
  #  qtm(pct.scheme)

  #Now loop over each pct route and find the change in busyness
  #res <- lapply(pct.scheme,calcChangeBusy2)
  #res <- pblapply(1:nrow(pct.scheme),calcChangeBusy)
  #res <- do.call("rbind",res)
  #message(paste0("DOne Scheme ",j," at ",Sys.time()))



  ##########################################################
  #Parallel
  start <- Sys.time()
  fun <- function(cl){
    parLapply(cl, 1:nrow(pct.scheme),calcChangeBusy)
  }
  cl <- makeCluster(4) #make clusert and set number of cores
  clusterExport(cl=cl, varlist=c("osm", "pct.scheme","j"))
  clusterExport(cl=cl, c('roadsOnLine2', 'calcChangeBusy') )
  clusterEvalQ(cl, {library(sf)})
  respar <- fun(cl)
  stopCluster(cl)
  respar <- do.call("rbind",respar)
  end <- Sys.time()
  message(paste0("Did in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
  #identical(res,respar)
  ##########################################################

  pct.up <- left_join(pct.scheme, respar, by = c("ID" = "id"))
  pct.up$total <- pct.up$pct.census + pct.up$onfoot + pct.up$motorvehicle + pct.up$publictransport + pct.up$other
  pct.up$model.before <-  round(exp(-2.2679333871 -0.0001207765 * pct.up$length + 0.0242742120 * sqrt(pct.up$length) -0.0075675068 * sqrt(pct.up$before) -1.9347971306 * pct.up$av_incline + -6.8377711996 * sqrt(pct.up$av_incline)),1) * pct.up$total
  pct.up$model.after <-  round(exp(-2.2679333871 -0.0001207765 * pct.up$length + 0.0242742120 * sqrt(pct.up$length) -0.0075675068 * sqrt(pct.up$after) -1.9347971306 * pct.up$av_incline + -6.8377711996 * sqrt(pct.up$av_incline)),1) * pct.up$total

  #Uptake for scheme is
  uptake <- data.frame(scheme = j, census = sum(pct.up$pct.census), model.now = sum(pct.up$model.before), model.future = sum(pct.up$model.after))
  uptake.list[[j]] <- uptake

  message(paste0("Done scheme ",j," at ",Sys.time()))

}

uptake.fin <- do.call("rbind",uptake.list)
uptake.fin$pup <- uptake.fin$model.future / uptake.fin$model.now
uptake.fin$expect <- round(uptake.fin$pup * uptake.fin$census,0)
