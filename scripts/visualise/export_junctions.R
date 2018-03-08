regions <- regions.todo
regions.list <- list()

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-junctions.Rds"))){
    #Get file
    junc <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-junctions.Rds"))
    message(paste0(Sys.time()," Processing ",regions[b]," with ",nrow(junc)," points"))
    #print(nrow(osm))
    if(!"region" %in% names(junc)){
      junc$region <- regions[b]
      print(paste0("Region Missing from ", regions[b]))
    }

    junc <- st_transform(junc, 4326) #convert to lat lngs for leaflet mapping
    names(junc) <- str_replace_all(names(junc),"[[:punct:]]","") #Remove punctuation from names for POSTGIS

    # Convert factor to numeric
    junc$osmid <- as.numeric(as.character(junc$osmid))

    #Reduce precison of data to reduce file size
    junc$geometry <- st_as_binary(junc$geometry, precision = 1000000)
    junc$geometry <- st_as_sfc(junc$geometry)

    #convert to well known text
    junc$geotext <- st_as_text(junc$geometry)
    junc <- as.data.frame(junc)
    junc$geometry <- NULL

    #put id column first
    col.names <- names(junc)[names(junc) != "osmid"]
    junc <- junc[,c("osmid",col.names)]

    regions.list[[b]] <- junc
    rm(junc)

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)

message(paste0(Sys.time()," Combining Regions into master file "))

#Bind all the regions toghter
#osm.all <- do.call("rbind",regions.list)
junc.all <- bind_rows(regions.list)

#check for duplicate ids
summary(duplicated(junc.all$osmid))
junc.all <- junc.all[!duplicated(junc.all$osmid),]
options("scipen"=100, "digits"=4)
junc.all$osmid <- as.character(junc.all$osmid)
junc.all$osmid[43887]

#Save Out

write.csv(junc.all,"../cyipt-bigdata/forDB/junctions.csv", row.names = F, na = "")


