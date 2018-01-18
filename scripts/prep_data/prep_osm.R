#Prepare  OSM Data
#The code take in OSM data and remove unneeded columns, then splits lines at junctions
#It returns two outputs:
# 1) the split lines with the attributes of the original data
# 2) The points where the lines are split, i.e. junction locations


#FUnction to Splitlines
splitlines <- function(a){
  line_sub <- osm[a,]
  buff_sub <- buff[inter[[a]],]
  if(nrow(buff_sub) == 0){
    line_cut <- line_sub
  }else{
    buff_sub <- st_union(buff_sub)
    line_cut <- st_difference(line_sub, buff_sub)
  }
  line_cut<- st_cast(line_cut, "LINESTRING") #needed for new bind_rows() call
  return(line_cut)
}

#Settings
regions <- regions.todo

#create directory
if(!dir.exists(paste0("../cyipt-bigdata/osm-prep"))){
  dir.create(paste0("../cyipt-bigdata/osm-prep"))
}

for(a in 1:length(regions)){
  message(paste0("Splitting Lines for ",regions[a]," at ",Sys.time()))
  if(file.exists(paste0("../cyipt-bigdata/osm-clean/",regions[a],"/osm-lines.Rds"))){
    if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-lines.Rds")) & skip){
      message("Skipping as done before")
    }else{
      #Create output dir
      dir.create(paste0("../cyipt-bigdata/osm-prep/",regions[a]))

      #Reading in data
      osm <- readRDS(paste0("../cyipt-bigdata/osm-clean/",regions[a],"/osm-lines.Rds"))
      points <- readRDS(paste0("../cyipt-bigdata/osm-raw/",regions[a],"/osm-junction-points.Rds"))

      #Loop To Split Lines
      buff <- st_buffer(points,0.01, nQuadSegs = 2)
      inter <- st_intersects(osm,buff)
      cut_list <- lapply(1:nrow(osm), splitlines)

      cut <- bind_rows(cut_list) #much faster than rbind but mangle the sf format, all geometies must be same type
      rm(cut_list)

      #rebuild the sf object
      cut <- as.data.frame(cut)
      cut$geometry <- st_sfc(cut$geometry)
      cut <- st_sf(cut)
      st_crs(cut) <- 27700

      # check for dupliates
      cut <- cut[!duplicated(cut$geometry),]

      #Split MULTILINESTRING into LINESTRING

      #Add ID
      cut$id <- 1:nrow(cut)
      row.names(cut) <- cut$id

      #Save Out Data
      saveRDS(cut, paste0("../cyipt-bigdata/osm-prep/",regions[a],"/osm-lines.Rds"))

      if(verbose){message(paste0("Started with ",nrow(osm)," lines, finished with ",nrow(cut)))}
      rm(osm, cut, bounds,buff,inter, points)
      gc()

    }
  }else{
    message(paste0("Input File missing for ",regions[a]," at ",Sys.time()))
  }
}
rm(a,regions)








