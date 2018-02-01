# Repoduce Robin's Uptake Work
library(sf)
library(sp)
library(dplyr)
library(stplanr)
library(parallel)
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
  files <- list.files("../cyipt-bigdata/osm-clean", full.names = T, recursive = T)
  osm <- list()
  for(i in seq_len(length(files)) ){
    osm.tmp <- readRDS(files[i])
    osm.tmp <- osm.tmp[,c("osm_id","highway","maxspeed","surface","segregated","cycleway.left",
                          "cycleway.right","cycleway.left.width","cycleway.right.width","region","geometry")]
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


###################################
# Step 4: Find the OSM roads that intersect the buffered routes
if(file.exists("../cyipt-securedata/uptakemodel/osm_rf_inter.Rds")){
  inter <- readRDS("../cyipt-securedata/uptakemodel/osm_rf_inter.Rds")
}else{
  inter <- st_parallel(sf_df = rf_buff, sf_func = st_intersects, n_cores = 6)
  saveRDS(osm,"../cyipt-securedata/uptakemodel/osm_rf_inter.Rds")
}

