regions <- regions.todo
regions.list <- list()
date.raw <- list()
date.recc <- list()

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds"))){
    #Get file
    osm <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds"))
    message(paste0(Sys.time()," Processing ",regions[b]," with ",nrow(osm)," lines"))

    #Get file info
    date.recc[[b]] <- as.Date(file.mtime(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds")))
    date.raw[[b]] <- as.Date(file.mtime(paste0("../cyipt-bigdata/osm-raw/",regions[b],"/osm-lines.Rds")))

    #print(nrow(osm))
    if(!"region" %in% names(osm)){
      osm$region <- regions[b]
      print(paste0("Region Missing from ", regions[b]))
    }

    osm <- st_transform(osm, 4326) #convert to lat lngs for leaflet mapping
    names(osm) <- str_replace_all(names(osm),"[[:punct:]]","") #Remove punctuation from names for POSTGIS

    #Reduce precison of data to reduce file size
    osm$geometry <- st_as_binary(osm$geometry, precision = 100000)
    osm$geometry <- st_as_sfc(osm$geometry)

    #convert to well known text
    osm$geotext <- st_as_text(osm$geometry)
    osm <- as.data.frame(osm)
    osm$geometry <- NULL

    #put id column first
    col.names <- names(osm)[names(osm) != "id"]
    osm <- osm[,c("id",col.names)]
    #osm$region <- regions[b] # now added at dwonload stage

    regions.list[[b]] <- osm
    rm(osm)

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}


updates <- data.frame(regions = regions, raw =  do.call("c", date.raw), recc = do.call("c", date.recc))
rm(b,regions, date.raw, date.recc)

message(paste0(Sys.time()," Combining Regions into master file "))

#Bind all the regions toghter
#osm.all <- do.call("rbind",regions.list)
osm.all <- bind_rows(regions.list)
rm(regions.list)

message(paste0(Sys.time()," Making Final Corrections "))

#change osmids to intergers
osm.all$osmid <- as.integer(osm.all$osmid)

#Add masrster ID column
osm.all$idGlobal <- 1:nrow(osm.all)

#Spilit out unique road types
roadtypes <- unique(osm.all[,c("roadtype","onewaysummary","sidewalk","cyclewayleft","lanespsvforward","lanesforward","lanesbackward","lanespsvbackward","cyclewayright")])
roadtypes <- roadtypes[order(roadtypes$roadtype, roadtypes$onewaysummary, roadtypes$lanesforward),]
roadtypes$rtid <- 1:nrow(roadtypes)

#Reorder columns
roadtypes <- roadtypes[,c("rtid","roadtype","onewaysummary","sidewalk","cyclewayleft","lanespsvforward","lanesforward","lanesbackward","lanespsvbackward","cyclewayright")]


#Add rtid and remove data
osm.all <- left_join(osm.all, roadtypes, by = c("roadtype" = "roadtype", "onewaysummary" = "onewaysummary","sidewalk" = "sidewalk","cyclewayleft" = "cyclewayleft","lanespsvforward" = "lanespsvforward","lanesforward" = "lanesforward","lanesbackward" = "lanesbackward","lanespsvbackward" = "lanespsvbackward","cyclewayright" = "cyclewayright"))


osm.all <- osm.all[,names(osm.all)[!names(osm.all) %in% c("roadtype","onewaysummary","sidewalk","cyclewayleft","lanespsvforward","lanesforward","lanesbackward","lanespsvbackward","cyclewayright")]]

#Clean Up Numerics to Intergers
osm.all$aadt <- as.integer(osm.all$aadt)
osm.all$ncycles <- as.integer(osm.all$ncycles)

osm.all$width <- as.integer(osm.all$width)
osm.all$widthpath <- as.integer(osm.all$widthpath)

#Rearange Columns
osm.all <- osm.all[,c("idGlobal","id","osmid","region","name","ref","highway","junction","elevation","maxspeed","segregated","pctcensus",
  "pctgov", "pctgen", "pctdutch", "pctebike","pcttotal",
  "width","widthpath", "calcwidthnow","calcwidthrec","widthdiffnow","widthdiffrec","widthstatus",
  "ncollisionsSlight","ncollisionsSerious", "ncollisionsFatal","bikeCasSlight","bikeCasSerious","bikeCasFatal","totalCasSlight","totalCasSerious","totalCasFatal","totalVeh",
  "aadt","ncycles","Recommended","DesWidth", "MinWidth", "DesSeparation",
  "MinSeparation",  "Existing",  "Change",  "costperm",  "length",  "costTotal",  "groupid",  "rtid",  "geotext")]

for(i in 1:ncol(osm.all)){
  print(paste0("Column ",colnames(osm.all)[i]," is a ",class(osm.all[,i])))
  if(class(osm.all[,i]) == "character"){
    print(paste0("Max length is ",max(nchar(osm.all[,i]), na.rm = T)))
  }
}



#Trim text columns
osm.all$ref <- substr(osm.all$ref, 1, 20)
osm.all$name <- substr(osm.all$name, 1, 255)

message(paste0(Sys.time()," Saving CSV Files "))

#Save Out
write.csv(roadtypes,"../cyipt-bigdata/forDB/roadtypes.csv", row.names = F, na = "")
write.csv(osm.all,"../cyipt-bigdata/forDB/roads.csv", row.names = F, na = "")
write.csv(updates,"../cyipt-bigdata/forDB/updates.csv", row.names = F, na = "")

