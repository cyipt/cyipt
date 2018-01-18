# Simplify Data for Publication
# Focu on Reucing Data Size and Increasing Performance

regions <- regions.todo


#Read In Each File and Combine
regions.list <- list()
for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes-simplified.Rds"))){
    #Get file
    schemes <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes-simplified.Rds"))

    if("data.frame" %in% class(schemes)){
      print(nrow(schemes))
      if(!"region" %in% names(schemes)){
        schemes$region <- regions[b]
        print(paste0("Region Missing from ", regions[b]))
      }

      schemes <- st_transform(schemes, 4326) #convert to lat lngs for leaflet mapping
      names(schemes) <- str_replace_all(names(schemes),"[[:punct:]]","") #Remove punctuation from names for POSTGIS

      #Remove any invlaid geometry
      schemes <- schemes[!is.na(st_dimension(schemes)),]

      #Reduce precison of data to reduce file size
      schemes$geometry <- st_as_binary(schemes$geometry, precision = 100000)
      schemes$geometry <- st_as_sfc(schemes$geometry)

      #convert to well known text
      schemes$geotext <- st_as_text(schemes$geometry)
      schemes <- as.data.frame(schemes)
      schemes$geometry <- NULL


      regions.list[[b]] <- schemes
      rm(schemes)
    }else{
      message(paste0("Invalid Data for ",regions[b]))
    }



  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)

#Bind all the regions toghter
schemes.all <- do.call("rbind",regions.list)

#Add masrster ID column
schemes.all$idGlobal <- 1:nrow(schemes.all)

#Reorder columns
schemes.all <- schemes.all[,c("idGlobal","groupid","region","costTotal","costperperson","ncyclebefore","ncycleafter",
                              "type","change","per","length",
                              "ndrivebefore","ndriveafter","carkmbefore","carkmafter","carkm",
                              "absenteeismbenefit","healthdeathavoided","healthbenefit",
                              "qualitybenefit","accidentsbenefit","co2saved","ghgbenefit",
                              "congestionbenefit","totalBen","costBenRatio","geotext")]

#Rename for Database
#costTotal and type
names(schemes.all) <- c("idGlobal","groupid","region","cost","costperperson","ncyclebefore","ncycleafter",
                        "infratype","change","per","length",
                        "ndrivebefore","ndriveafter","carkmbefore","carkmafter","carkm",
                        "absenteeismbenefit","healthdeathavoided","healthbenefit",
                        "qualitybenefit","accidentsbenefit","co2saved","ghgbenefit",
                        "congestionbenefit","totalBen","costBenRatio","geotext")

print(summary(as.factor(schemes.all$region)))

#Save Out
write.csv(schemes.all,"../cyipt-bigdata/forDB/schemes.csv", row.names = F, na = "")



