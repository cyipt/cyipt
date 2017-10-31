# Test of calcualting uptake for each scenario in bristol

# set up
library(sf)
library(dplyr)
library(tmap)
tmap_mode("view")
region <- "BristolCityof"
osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",region,"/osm-lines.Rds"))


#Functions

roadsOnLine <- function(a){
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
  #len <- as.numeric(st_distance(points[1],points[3])) #Change to distance between points to deal with curved roads
  len <- sqrt((points[[1]][1] - points[[3]][1])**2 + (points[[1]][2] - points[[3]][2])**2) # for sort distances on projected coordinates faster than st_distance with same answer
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
  sel <- st_intersects(buff, line)
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
  #osm.sub2 <- osm.sub[match,]

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

  message(paste0("Line ",k," has had its busyance changed by ",round((s1-s2)/s1 * 100,2)," %" ))
    #return(match)
  #return(s2)

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
for(j in scheme_nos){
  #Get the roads in the schemes
  osm.scheme <- osm.nona[osm.nona$group_id == j,]
  # Get the PCT lines that intersect the scheme
  osm.scheme.buf <- st_buffer(osm.scheme, 1)
  osm.scheme.buf <- osm.scheme.buf[,c("id")]

  #pct.scheme2 <- pct.all[st_intersects(osm.scheme.buf, pct.all)[[1]],]
  pct.scheme <- pct.all[st_intersects(osm.scheme.buf, pct.all)[[1]],]
  ############################################################
  # Selection will need refining

  #qtm(pct.scheme) +
  #  qtm(osm.scheme.buf)

  #check the interesctions
  inter <- st_intersection(osm.scheme.buf, pct.scheme)


  #remove interecsion less than 3 m
  inter$inter_length <- as.numeric(st_length(inter))
  inter <- inter[inter$inter_length > 3,]

  pct.scheme <- pct.scheme[pct.scheme$ID %in% unique(inter$ID),]
  #qtm(pct.scheme, lines.lwd = 4, lines.col = "red") +
  #  qtm(pct.scheme2, lines.lwd = 2, lines.col = "blue")

  #tab <- as.data.frame(table(inter$ID))


  #inter2 <- st_intersection(pct.scheme, osm.scheme.buf)
  #inter3 <- st_intersection(foo, pct.scheme)
  #qtm(inter)


  ##########################################################

  #Now loop over each pct route and find the change in busyness

  test <- lapply(1:5,calcChangeBusy)
  foo <- calcChangeBusy(5)
  bar <- sapply(osm.sub$geometry,roadsOnLine2, line2check = line)

  for(k in 1:5){
  #for(k in 1:nrow(pct.scheme)){
  #Pick a line to test on
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
  match = sapply(1:nrow(osm.sub), roadsOnLine)

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

  message(paste0("Line ",k," has had its busyance changed by ",round((s1-s2)/s1 * 100,2)," %" ))


}


}


