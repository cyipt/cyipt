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
}else{
  rf_buff <- st_parallel(sf_df = rf01_11, sf_func = st_buffer, n_cores = 7, dist = 20, nQuadSegs = 2)
  saveRDS(rf_buff,"../cyipt-securedata/uptakemodel/rf_buff.Rds")
}

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
if(file.exists("../cyipt-securedata/uptakemodel/osm_rf_inter.Rds")){
  inter <- readRDS("../cyipt-securedata/uptakemodel/osm_rf_inter.Rds")
}else{
  inter <- st_parallel(sf_df = rf_buff, sf_func = st_intersects, n_cores = 2, y = osm[,"id"])
  saveRDS(inter,"../cyipt-securedata/uptakemodel/osm_rf_inter.Rds")
}



#################################
# Stetp 7: Combine the two hsitoric datasets
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
qtm(infra.historic[!is.na(infra.historic$BuildYear) | !is.na(infra.historic$OpenDate),], lines.lwd = 2, lines.col = "red" ) +
  qtm(infra.change, lines.lwd = 1, lines.col = "blue")








###################################
# Step 9: Subset the OSM to only the parts that intersect with the routes

#list the osmids
foo <- inter[1:5]
inter.unique <- unlist(foo)
inter.unique <- unique(inter.unique)

qtm(rf_buff[1,], fill = NULL) +
  qtm(osm[inter[[1]],], lines.col = "red", lines.lwd = 4)

osm



foo <- st_parallel(sf_df = rf_buff[1:10, ], sf_func = st_intersects, n_cores = 6, y = osm)
foo2 <- do.call("c", foo)
foo3 <- unique(foo2)
qtm(osm[foo3,])


##################################
# Step 5: For each PCT route Summariase the infrastructure

# create a single aggregate variaible in the osm

osm$infra_summary <- NA

names(osm)




