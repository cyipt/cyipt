# Repoduce Robin's Uptake Work
library(sf)
library(sp)
library(dplyr)
library(stplanr)
library(parallel)
library(tmap)
tmap_mode("view")
source("../cyipt/R/st_parallel.R")

############################
# Step 1: Get the routes with the number of cyclists in 2001 and 2011

if(file.exists("../cyipt-securedata/uptakemodel/routes01_11.Rds")){
  rf01_11 <- readRDS("../cyipt-securedata/uptakemodel/routes01_11.Rds")
}else{
  #Read In data
  od01 <- readRDS("../ROBIN/cyoddata/od_01_new.Rds")
  rf <- readRDS("../ROBIN/cyoddata/rf.Rds")
  rf <- st_as_sf(rf)
  rf <- st_transform(rf, 27700)

  #discard uneeded data
  rf <- rf[,c("id","geo_code1","geo_code2","all","bicycle")]
  od01 <- as.data.frame(od01)
  od01 <- od01[!grepl("W",od01$o),]
  od01 <- od01[!grepl("W",od01$d),]

  od01$bicycle01 <- od01$all01 * od01$pcycle01

  #convert two way pairs to oneway
  od01 <- onewayid(od01, attrib = c("all01","bicycle01"), id1 = "o", id2 = "d")
  rf <- onewayid(rf, attrib = c("all","bicycle"), id1 = "geo_code1", id2 = "geo_code2")

  #join togther

  rf01_11 <- left_join(rf,od01, by = c("geo_code1" = "o", "geo_code2" = "d"))
  summary(is.na(rf01_11$all01))

  rf01_11$geo_code1 <- NULL
  rf01_11$geo_code2 <- NULL
  names(rf01_11) <- c("id","all11","bicycle11","all01","bicycle01","is_two_way","geometry" )

  rm(od01,rf)
  saveRDS(rf01_11,"../cyipt-securedata/uptakemodel/routes01_11.Rds")
}

############################
# Step 2: Get the National OSM Data

if(file.exists("../cyipt-securedata/uptakemodel/osm_clean.Rds")){
  osm <- readRDS("../cyipt-securedata/uptakemodel/osm_clean.Rds")
}else{
  files <- list.files("../cyipt-bigdata/osm-prep", full.names = T, recursive = T, pattern = "osm-lines")
  osm <- list()
  for(i in seq_len(length(files)) ){
    osm.tmp <- readRDS(files[i])
    osm.tmp <- osm.tmp[,c("id","highway","maxspeed","segregated","cycleway.left",
                          "cycleway.right","geometry")]
    osm[[i]] <- osm.tmp
    message(paste0(Sys.time()," Done ",files[i]))
  }
  rm(i,files,osm.tmp)

  osm <- bind_rows(osm)
  osm <- as.data.frame(osm)
  osm$geometry <- st_sfc(osm$geometry)
  osm <- st_sf(osm)
  st_crs(osm) <- 27700

  saveRDS(osm,"../cyipt-securedata/uptakemodel/osm_clean.Rds")
}


###################################
# Step 3: Buffer the Routes
if(file.exists("../cyipt-securedata/uptakemodel/rf_buff.Rds")){
  rf_buff <- readRDS("../cyipt-securedata/uptakemodel/rf_buff.Rds")
  rf_buff <- rf_buff[order(match(rf_buff$id,rf01_11$id)),]
}else{
  rf_buff <- st_parallel(sf_df = rf01_11, sf_func = st_buffer, n_cores = 6, dist = 5, nQuadSegs = 2)
  saveRDS(rf_buff,"../cyipt-securedata/uptakemodel/rf_buff.Rds")
}


#Sanity check
summary(rf_buff$id == rf01_11$id)
rnumbs <- runif(10,1,nrow(rf_buff))
qtm(rf01_11[rnumbs,], lines.lwd = 5, lines.col = "red") +
  qtm(rf_buff[rnumbs,])



#################################
# Step 4: get the historic infrastrucutre data
if(file.exists("../cyipt-securedata/uptakemodel/infra_change.Rds")){
  infra.change <- readRDS("../cyipt-securedata/uptakemodel/infra_change.Rds")
}else{
  #Sustrans Data?
  infra.sc2sd <- readRDS("N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyinfdat/sc2sd")
  infra.sl2sc <- readRDS("N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyinfdat/ri_04_11_dft")
  infra.sndft <- readRDS("N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyinfdat/ri_01_11_non_dft")

  #Merge Together and clean up
  infra.change <- bind_rows(infra.sc2sd, infra.sl2sc, infra.sndft)
  rm(infra.sc2sd, infra.sl2sc, infra.sndft)
  infra.change <- as.data.frame(infra.change)
  infra.change$geometry <- st_sfc(infra.change$geometry)
  infra.change <- st_sf(infra.change)
  st_crs(infra.change) <- 27700

  infra.change$C2_Scheme <- NULL
  infra.change$LastUpdate <- NULL
  saveRDS(infra.change,"../cyipt-securedata/uptakemodel/infra_change.Rds")
}

###################################
# Step 5: Get the Transport Direct Data
if(file.exists("../cyipt-securedata/uptakemodel/infra_td.Rds")){
  infra.td <- readRDS("../cyipt-securedata/uptakemodel/infra_td.Rds")
}else{
  #Transport Direct Data
  infra.td <- st_read("N:/Earth&Environment/Research/ITS/Research-1/CyIPT/td/dft-england-cycling-data-2011.geojson")
  infra.td <- st_transform(infra.dft, 27700)

  #remove points
  infra.td <- infra.td[st_geometry_type(infra.td) == "LINESTRING", ]

  saveRDS(infra.td,"../cyipt-securedata/uptakemodel/infra_td.Rds")
}

###################################
# Step 6: Find the OSM roads that intersect the buffered routes
if(file.exists("../cyipt-securedata/uptakemodel/osm_rf_contains_5m.Rds")){
  inter <- readRDS("../cyipt-securedata/uptakemodel/osm_rf_contains_5m.Rds")
  rf_buff <- readRDS("../cyipt-securedata/uptakemodel/rf_buff.Rds")
  osm <- readRDS("../cyipt-securedata/uptakemodel/osm_clean.Rds")
}else{
  osm_id <- osm[,"id"]
  rf_buff_id <- rf_buff[,"id"]
  rm(rf_buff, osm, rf01_11)
  inter <- st_parallel(sf_df = rf_buff_id, sf_func = st_contains, n_cores = 3, y = osm_id)
  saveRDS(inter,"../cyipt-securedata/uptakemodel/osm_rf_contains_5m.Rds")

  ####################################
  # Sanity Check
  qtm(rf_buff_id[1,], fill = NULL ) +
    qtm(osm_id[inter[[1]], ], lines.lwd = 1, lines.col = "blue")

  rm(osm_id, rf_buff_id)
  rf_buff <- readRDS("../cyipt-securedata/uptakemodel/rf_buff.Rds")
  osm <- readRDS("../cyipt-securedata/uptakemodel/osm_clean.Rds")

}

#################################
# Step 7: Combine the two hsitoric datasets
if(file.exists("../cyipt-securedata/uptakemodel/infra_historic.Rds")){
  infra.historic <- readRDS("../cyipt-securedata/uptakemodel/infra_historic.Rds")
}else{

  infra.change.buff <- st_buffer(infra.change, dist = 5, nQuadSegs = 2)

  infra.historic = st_join(infra.td, infra.change.buff, join = st_intersects, left = TRUE, largest = FALSE)
  #infra.historic.old <- infra.historic
  #clena up the tags
  names(infra.historic)

  #Summarise if on a cycle network
  infra.historic$rcn <- as.character(infra.historic$rcn)
  infra.historic$lcn <- as.character(infra.historic$lcn)
  infra.historic$ncn <- as.character(infra.historic$ncn)

  infra.historic$rcn[is.na(infra.historic$rcn)] <- "no"
  infra.historic$lcn[is.na(infra.historic$lcn)] <- "no"
  infra.historic$ncn[is.na(infra.historic$ncn)] <- "no"

  infra.historic$network <- ifelse(infra.historic$rcn == "yes" | infra.historic$lcn == "yes" | infra.historic$ncn == "yes",
                                   TRUE, FALSE)

  #Clean cycle infrastrucutre
  infra.historic$cycleway <- as.character(infra.historic$cycleway)
  infra.historic$cycleway[infra.historic$cycleway %in% c("Lane","yes")] <- "lane"


  infra.historic$cycleway.left.width <- as.numeric(as.character(infra.historic$cycleway.left.width))
  infra.historic$cycleway.oneside.width <-  as.numeric(as.character(infra.historic$cycleway.oneside.width))
  infra.historic$cycleway.otherside.width <- as.numeric(as.character(infra.historic$cycleway.otherside.width))
  infra.historic$cycleway.right.width <- as.numeric(as.character(infra.historic$cycleway.right.width))

  for(i in 1:nrow(infra.historic)){
    left <- infra.historic$cycleway.left.width[i]
    oneside <- infra.historic$cycleway.oneside.width[i]
    otherside <- infra.historic$cycleway.otherside.width[i]
    right <- infra.historic$cycleway.right.width[i]
    if(is.na(otherside) | is.na(oneside)){
      #no data do nothing
    }else{
      message(paste0("Line ",i," ",left," ",oneside," ",otherside," ",right))
      #dat so do something
      if(!is.na(oneside) & is.na(left)){
        infra.historic$cycleway.left.width[i] <- oneside
        infra.historic$cycleway.oneside.width[i] <- NA
      }

      if(!is.na(otherside) & is.na(right)){
        infra.historic$cycleway.right.width[i] <- otherside
        infra.historic$cycleway.otherside.width[i] <- NA
      }
    }


  }

  infra.historic$cycleway.left <- as.character(infra.historic$cycleway.left)
  infra.historic$cycleway.oneside <-  as.character(infra.historic$cycleway.oneside)
  infra.historic$cycleway.otherside <- as.character(infra.historic$cycleway.otherside)
  infra.historic$cycleway.right <- as.character(infra.historic$cycleway.right)

  infra.historic$cycleway.left[infra.historic$cycleway.left %in% c("Lane","yes")] <- "lane"
  infra.historic$cycleway.oneside[infra.historic$cycleway.oneside %in% c("Lane","yes")] <- "lane"
  infra.historic$cycleway.otherside[infra.historic$cycleway.otherside %in% c("Lane","yes")] <- "lane"
  infra.historic$cycleway.right[infra.historic$cycleway.right %in% c("Lane","yes")] <- "lane"
  infra.historic$cycleway.right[infra.historic$cycleway.right %in% c("20111212","Wide bus >4.2m")] <- NA

  for(i in 1:nrow(infra.historic)){
    left <- infra.historic$cycleway.left[i]
    oneside <- infra.historic$cycleway.oneside[i]
    otherside <- infra.historic$cycleway.otherside[i]
    right <- infra.historic$cycleway.right[i]
    if(is.na(otherside) | is.na(oneside)){
      #no data do nothing
    }else{
      message(paste0("Line ",i," ",left," ",oneside," ",otherside," ",right))
      #dat so do something
      if(!is.na(oneside) & is.na(left)){
        infra.historic$cycleway.left[i] <- oneside
        infra.historic$cycleway.oneside[i] <- NA
      }

      if(!is.na(otherside) & is.na(right)){
        infra.historic$cycleway.right[i] <- otherside
        infra.historic$cycleway.otherside[i] <- NA
      }
    }


  }

  #Clean up dates

  date_switch = function(d){
    d = sapply(d, switch,
               "2004/5" = "2005-01-01",
               "2005/6" = "2006-01-01",
               "2006/7" = "2007-01-01",
               "2007/8" = "2008-01-01",
               "2008/9" = "2009-01-01",
               "2009/2010" = "2010-01-01",
               "2010/2011" = "2011-01-01",
               NA
    )
    as.Date(unlist(d))
  }

  infra.historic$BuildYear <- date_switch(infra.historic$BuildYear)
  infra.historic$date <- ifelse(is.na(infra.historic$BuildYear),as.character(infra.historic$OpenDate),as.character(infra.historic$BuildYear))
  infra.historic$date <- as.Date(infra.historic$date)



  #remove unneed columns

  infra.historic <- infra.historic[,!(names(infra.historic) %in% c("ncn","ncn_ref","rcn","rcn_ref","lcn","lcn_ref",
                                                                   "ncn_name","rcn_name","lcn_name","bicycle","layer",
                                                                   "railway","tunnel","horse","name","ref","barrier",
                                                                   "ford","leisure","depth","ford","cycle","RouteType",
                                                                   "bridge","motorcar","cycleway.otherside","crossing",
                                                                   "cycleway.otherside.width","cycleway.oneside",
                                                                   "cycleway.oneside.width","step_count","BuildYear",
                                                                   "OpenDate","CycleRoute"))]

  saveRDS(infra.historic,"../cyipt-securedata/uptakemodel/infra_historic.Rds")
}


####################################
# Sanity Check
#qtm(infra.historic[!is.na(infra.historic$date),], lines.lwd = 2, lines.col = "red" ) #+
#  qtm(infra.change, lines.lwd = 1, lines.col = "blue")

rm(infra.change, infra.td)
osm$idnew <- 1:nrow(osm)

#############
# Remove OSM lines which are not on any of the PCT routes
inter.unique <- unlist(inter)
inter.unique <- unique(inter.unique)
inter.unique <- inter.unique[order(inter.unique)]

osm.onroutes <- osm[inter.unique,]
bounds <- readRDS("../cyipt-bigdata/osm-raw/Bristol/bounds.Rds")

#infra.historic.changed <- infra.historic[!is.na(infra.historic$date),]
infra.historic.changed <- infra.historic # keep so we can find the post 2011 infra and ignore
#summary(is.na(infra.historic$date))


#qtm(osm.onroutes[bounds,], lines.lwd = 4, lines.col = "blue") +
#  qtm(infra.historic.onroute[bounds,], lines.lwd = 2, lines.col = "red")


######
# Find the changing infrastrucutre that is near the osm routes

if(file.exists("../cyipt-securedata/uptakemodel/infra_historic_onroutes.Rds")){
  infra.historic.onroute <- readRDS("../cyipt-securedata/uptakemodel/infra_historic_onroutes.Rds")

}else{
  osm.onroutes.buffer <- st_parallel(sf_df = osm.onroutes, sf_func = st_buffer, n_cores = 6, dist = 30, nQuadSegs = 2)
  inter.historic <- st_parallel(sf_df = osm.onroutes.buffer, sf_func = st_intersects, n_cores = 6, y = infra.historic.changed)
  inter.historic.unique <- unlist(inter.historic)
  inter.historic.unique <- unique(inter.historic.unique)
  inter.historic.unique <- inter.historic.unique[order(inter.historic.unique)]

  infra.historic.onroute <- infra.historic.changed[inter.historic.unique,]

  saveRDS(infra.historic.onroute,"../cyipt-securedata/uptakemodel/infra_historic_onroutes.Rds")
  #qtm(osm.onroutes[bounds,], lines.lwd = 5, lines.col = "blue") +
  #qtm(osm.onroutes.buffer[bounds,], fill = NULL) +
  #  qtm(infra.historic.changed[bounds,], lines.lwd = 3, lines.col = "red") +
  #  qtm(infra.historic.onroute[bounds,], lines.lwd = 3, lines.col = "green")

}



# clean up
rm(infra.change, infra.historic, infra.historic.changed, infra.td, osm, inter.historic, inter.historic.unique)


###################################
# Step 8: Join the hisotric infrastrucutre onto the osm networks
# now use a tighter buffer looking for exact matches

if(file.exists("../cyipt-securedata/uptakemodel/osm_onroutes_withchange.Rds")){
  osm.final <- readRDS("../cyipt-securedata/uptakemodel/osm_onroutes_withchange.Rds")
}else{
  osm.onroutes.buffer <- st_parallel(sf_df = osm.onroutes, sf_func = st_buffer, n_cores = 6, dist = 20, nQuadSegs = 2)
  osm.onroutes2 <- st_join(osm.onroutes.buffer, infra.historic.onroute, join = st_intersects, largest = TRUE)

  qtm(osm.onroutes2[bounds,], fill = "highway.y", borders = NULL, alpha = 1) +
    qtm(infra.historic.onroute[bounds,], lines.lwd = 2, lines.col = "green")

  osm.final <- osm.onroutes2
  osm.final <- osm.final[order(match(osm.final$idnew,osm.onroutes$idnew)),]
  summary(osm.final$idnew == osm.onroutes$idnew)
  osm.final$geometry <- osm.onroutes$geometry

  saveRDS(osm.final,"../cyipt-securedata/uptakemodel/osm_onroutes_withchange.Rds")
}

#redo the inter for the new osm dataset
if(file.exists("../cyipt-securedata/uptakemodel/osm_onroutes_rf_inter.Rds")){
  inter2 <- readRDS("../cyipt-securedata/uptakemodel/osm_onroutes_rf_inter.Rds")
}else{
  #inter2 <- st_parallel(sf_df = rf_buff, sf_func = st_contains, n_cores = 4, y = osm.final)
  inter2 <- st_contains(rf_buff, osm.final)
  saveRDS(inter2,"../cyipt-securedata/uptakemodel/osm_onroutes_rf_inter.Rds")
}

names(osm.final)

if(file.exists("../cyipt-securedata/uptakemodel/routes01_11_withlookup.Rds")){
  rf01_11 <- readRDS("../cyipt-securedata/uptakemodel/routes01_11_withlookup.Rds")
}else{
  # recorder the routes
  rf01_11 <- rf01_11[order(match(rf01_11$id,rf_buff$id)),]
  summary(rf01_11$id == rf_buff$id)
  rf01_11$osmids <- inter2
  saveRDS(rf01_11,"../cyipt-securedata/uptakemodel/routes01_11_withlookup.Rds")
}


#samity checks
n = sample(1:nrow(rf01_11),1)
qtm(rf01_11$geometry[n], lines.lwd = 4, lines.col = "red") +
  qtm(osm.final[rf01_11$osmids[[n]],], lines.lwd = 4 , lines.col = "green")# +
#  qtm(rf_buff$geometry[n])



#summarise the osm data
names(osm.final)
osm.final$id.y <- as.character(osm.final$id.y)


check.changestatus <- function(i){
  tmp_date <- osm.final$date[i]
  tmp_idy <- osm.final$id.y[i]
  if(is.na(tmp_date) & is.na(tmp_idy)){
    #Only Appears in the 2018 OSM data
    result <- "Only in OSM"
  }else if(is.na(tmp_date) & !is.na(tmp_idy)){
    #In the 2011 CYcling INfrastrucutre but has no change date
    result <- "Exists in 2011 unknown date"
  }else if(!is.na(tmp_date) & !is.na(tmp_idy)){
    #Have change information
    if(tmp_date >= as.Date("2001-01-01") & tmp_date <= as.Date("2011-12-31")){
      result <- "Built between 2001 and 2011"
    }else if(tmp_date < as.Date("2001-01-01")){
      result <- "Built before 2001"
    }else if(tmp_date > as.Date("2011-12-31")){
      result <- "Built after 2011"
    }else{
      result <- "Error Type 1"
    }
  }else{
    result <- "Error Type 2"
  }

  return(result)
}

osm.final$changestatus <- sapply(1:nrow(osm.final), check.changestatus)

# get the infrastrucutre before and after
osm.final$highway_01 <- NA
osm.final$highway_11 <- NA
osm.final$highway.x <- as.character(osm.final$highway.x)
osm.final$highway.y <- as.character(osm.final$highway.y)

for(i in 1:nrow(osm.final)){
  highwayX <- osm.final$highway.x[i]
  highwayY <- osm.final$highway.y[i]
  change <- osm.final$changestatus[i]

  if(change == "Only in OSM"){
    highway_01 <- highwayX
    highway_11 <- highwayX
  }else if(change == "Built after 2011"){
    highway_01 <- highwayX
    highway_11 <- highwayX
  }else if(change == "Built before 2001"){
    highway_01 <- highwayY
    highway_11 <- highwayY
  }else if(change == "Built between 2001 and 2011"){
    highway_01 <- highwayX
    highway_11 <- highwayY
  }else if(change == "Exists in 2011 unknown date"){
    highway_01 <- highwayY
    highway_11 <- highwayY
  }else{
    message(paste0("Error on ",i))
  }
  osm.final$highway_01[i] <- highway_01
  osm.final$highway_11[i] <- highway_11

  if(i %% 1000 == 0){
    message(paste0("Done ",i))
  }
  rm(highway_01,highway_11)
}

osm.final$highway_01[is.na(osm.final$highway_01)] <- osm.final$highway.x[is.na(osm.final$highway_01)]
osm.final$highway_11[is.na(osm.final$highway_11)] <- osm.final$highway.x[is.na(osm.final$highway_11)]

osm.final$maxspeed_01 <- NA
osm.final$maxspeed_11 <- NA
osm.final$highway.x <- as.character(osm.final$highway.x)

#add on a maxspeed for the infrastrucutre change
for(i in 1:nrow(osm.final)){
  highwayX <- osm.final$highway.x[i]
  highway_01 <- osm.final$highway_01[i]
  highway_11 <- osm.final$highway_11[i]
  maxspeedX <- osm.final$maxspeed[i]
  #change <- osm.final$changestatus[i]

  if(highway_01 == highwayX){
    maxspeed_01 <- maxspeedX
  }else{
    if(highway_01 %in% c("motorway","motorway_link")){
      maxspeed_01 <- 70
    }else if(highway_01 %in% c("trunk","trunk_link","bus_guideway")) {
      maxspeed_01 <- 60
    }else if(highway_01 %in% c("primary","residential","road","primary_link","secondary","secondary_link","tertiary","tertiary_link")){
      maxspeed_01 <- 30
    }else if(highway_01 == "service" ){
      maxspeed_01 <- 20
    }else if(highway_01 %in% c("path","bridleway","construction","cycleway","demolished","escalator","footway","living_street","steps","track","unclassified","pedestrian")){
      maxspeed_01 <- 10
    }else{
      maxspeed_01 <- 60
    }
  }

  if(highway_11 == highwayX){
    maxspeed_11 <- maxspeedX
  }else{
    if(highway_01 %in% c("motorway","motorway_link")){
      maxspeed_11 <- 70
    }else if(highway_01 %in% c("trunk","trunk_link","bus_guideway")) {
      maxspeed_11 <- 60
    }else if(highway_01 %in% c("primary","residential","road","primary_link","secondary","secondary_link","tertiary","tertiary_link")){
      maxspeed_11 <- 30
    }else if(highway_01 == "service" ){
      maxspeed_11 <- 20
    }else if(highway_01 %in% c("path","bridleway","construction","cycleway","demolished","escalator","footway","living_street","steps","track","unclassified","pedestrian")){
      maxspeed_11 <- 10
    }else{
      maxspeed_11 <- 60
    }
  }
  if(i %% 10000 == 0){
    message(paste0(Sys.time(),": Done ",i))
  }

  osm.final$maxspeed_11[i] <- maxspeed_11
  osm.final$maxspeed_01[i] <- maxspeed_01
}

saveRDS(osm.final,"../cyipt-securedata/uptakemodel/osm_onroutes_withchange_0111.Rds")
saveRDS(osm.final,"N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyipt-securedata/uptakemodel/osm_onroutes_withchange_0111.Rds")

# summarise the cycleway

osm.final$cycleway_osm <- NA

for(i in 1:nrow(osm.final)){
  left <- osm.final$cycleway.left.x[i]
  right <- osm.final$cycleway.right.x[i]

  if(left == right ){
    result <- left
  }else if(left == "no"){
    result <- right
  }else if(right == "no"){
    result <- left
  }else if(right %in% c("share_busway","lane") & left %in% c("lane","track") ){
    result <- left
  }else if(left %in% c("share_busway","lane") & right %in% c("lane","track") ){
    result <- right
  }else{
    message(paste0("Unusual case for row ",i," left = ",left," right = ",right))
    result <- left
  }
  osm.final$cycleway_osm[i] <- result
}

names(osm.final)

saveRDS(osm.final,"../cyipt-securedata/uptakemodel/osm_onroutes_withchange_0111.Rds")
saveRDS(osm.final,"N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyipt-securedata/uptakemodel/osm_onroutes_withchange_0111.Rds")

#summarise other cycleway data

osm.final$cycleway_change <- NA
osm.final$cycleway.right.y[is.na(osm.final$cycleway.right.y)] <- "no"
osm.final$cycleway.left.y[is.na(osm.final$cycleway.left.y)] <- "no"

for(i in 1:nrow(osm.final)){
  left <- osm.final$cycleway.left.y[i]
  right <- osm.final$cycleway.right.y[i]

  if(left == right ){
    result <- left
  }else if(left == "no"){
    result <- right
  }else if(right == "no"){
    result <- left
  }else{
    message(paste0("Unusual case for row ",i," left = ",left," right = ",right))
    result <- left
  }
  osm.final$cycleway_change[i] <- result
}

saveRDS(osm.final,"../cyipt-securedata/uptakemodel/osm_onroutes_withchange_0111.Rds")
saveRDS(osm.final,"N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyipt-securedata/uptakemodel/osm_onroutes_withchange_0111.Rds")

# Approximage the 2001 and 2011 infrastrucutre

osm.final$cycleway_01 <- NA

for(i in 1:nrow(osm.final)){
  change <- osm.final$changestatus[i]
  cycleway_change <- osm.final$cycleway_change[i]
  cycleway_osm <- osm.final$cycleway_osm[i]

  if(change %in% c("Built after 2011", "Built between 2001 and 2011")){
    result <- "no"
  }else if(change %in% c("Built before 2001")){
    result <- cycleway_change
  }else if(change %in% c("Exists in 2011 unknown date")){
    result <- cycleway_change
  }else if(change %in% c("Only in OSM")){
    result <- cycleway_osm
  }else{
    message(paste0("error on line ",i))
  }

  osm.final$cycleway_01[i] <- result
}


osm.final$cycleway_11 <- NA

for(i in 1:nrow(osm.final)){
  change <- osm.final$changestatus[i]
  cycleway_change <- osm.final$cycleway_change[i]
  cycleway_osm <- osm.final$cycleway_osm[i]

  if(change %in% c("Built after 2011")){
    result <- "no"
  }else if(change %in% c("Built before 2001","Built between 2001 and 2011")){
    result <- cycleway_change
  }else if(change %in% c("Exists in 2011 unknown date")){
    result <- cycleway_change
  }else if(change %in% c("Only in OSM")){
    result <- cycleway_osm
  }else{
    message(paste0("error on line ",i))
  }

  osm.final$cycleway_11[i] <- result
}

saveRDS(osm.final,"../cyipt-securedata/uptakemodel/osm_onroutes_withchange_0111.Rds")
saveRDS(osm.final,"N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyipt-securedata/uptakemodel/osm_onroutes_withchange_0111.Rds")

osm.final$length <- as.numeric(st_length(osm.final))

#Clean the Highway Tags

osm.final$highway_01 <- sub("_link","",osm.final$highway_01)
osm.final$highway_11 <- sub("_link","",osm.final$highway_11)

osm.final$highway_01[osm.final$highway_01 %in% c("track", "pedestrian","steps","bridleway","byway", "footway")] <- "path"
osm.final$highway_11[osm.final$highway_11 %in% c("track", "pedestrian","steps","bridleway","byway", "footway")] <- "path"

osm.final$highway_01[osm.final$highway_01 %in% c("unclassified", "service","living_street","road","byway", "Other","bus_guideway","BOAT")] <- "other"
osm.final$highway_11[osm.final$highway_11 %in% c("unclassified", "service","living_street","road","byway", "Other","bus_guideway","BOAT")] <- "other"


#summarise Infrastrucutre change for each route

get.infrachange <- function(x){
  id.tmp <- rf01_11$id[x]
  route <- rf01_11$osmids[[x]]
  osm_sub <- osm.final[route,]
  osm_sub <- as.data.frame(osm_sub)
  osm_sub <- osm_sub[,c("highway_01","highway_11","maxspeed_01","maxspeed_11","cycleway_01","cycleway_11","length")]

  osm_summary01 <- osm_sub[,c("highway_01","maxspeed_01","cycleway_01","length")]
  osm_summary01 <- group_by(osm_summary01, highway_01,maxspeed_01,cycleway_01)
  osm_summary01 <- summarise(osm_summary01, length = sum(length))

  osm_summary11 <- osm_sub[,c("highway_11","maxspeed_11","cycleway_11","length")]
  osm_summary11 <- group_by(osm_summary11, highway_11,maxspeed_11,cycleway_11)
  osm_summary11 <- summarise(osm_summary11, length = sum(length))

  result <- list()
  result[[1]] <- as.data.frame(osm_summary01)
  result[[2]] <- as.data.frame(osm_summary11)
  attr(result,'routeID') <- id.tmp
  return(result)
}

#foo <- lapply(1:10, get.infrachange)

##########################################################
#Parallel
m = 1 #Start
n = nrow(rf01_11) #End

start <- Sys.time()
fun <- function(cl){
  parLapply(cl, m:n, get.infrachange)
}
cl <- makeCluster(6) #make clusert and set number of cores
clusterEvalQ(cl, {library(sf); library(dplyr)})
clusterExport(cl=cl, varlist=c("osm.final", "rf01_11"), envir=environment())
respar <- fun(cl)
stopCluster(cl)
#respar <- bind_rows(respar)
end <- Sys.time()
message(paste0("Did ",n-m + 1," routes in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
rm(n,m,cl,start,end,fun)
##################################################

saveRDS(respar,"../cyipt-securedata/uptakemodel/route_infra_BeforeAfter.Rds")
saveRDS(respar,"N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyipt-securedata/uptakemodel/route_infra_BeforeAfter.Rds")







