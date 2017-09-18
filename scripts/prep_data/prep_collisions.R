library(foreign)
library(sf)
library(memisc)
library(stplanr)

folder <- "../cyipt-securedata/stats19/"
tmp.folder <- "F:/ziptmp"
files <- list.files(path = folder, full.names = FALSE) # Now get regions from the master file

acc.list <- list()
cas.list <- list()
veh.list <- list()

source("../stats19/R/import_functions.R")



for(a in 21:length(files)){
  record <- unzip(paste0(folder,files[a]), exdir = tmp.folder, overwrite = T)
  message("Doing ",record[1]," at ",Sys.time())

  #Read In data
  file.type <- substring(record[1],nchar(record[1]) - 3, nchar(record[1]))
  if(file.type  == ".sav"){
    acc <- read.spss(record[1], to.data.frame=FALSE, use.value.labels = FALSE)
    acc <- lapply(acc, Int2Factor)
    acc <- as.data.frame(acc, stringsAsFactors = FALSE)
    #acc <- try(read.spss(record[1], to.data.frame=TRUE))
    #if(class(acc) == "try-error"){
    #  #acc <- read.spss(record[1], to.data.frame=TRUE, use.value.labels = FALSE)
    #  message("Fail for",record[1])

    #  #foo <- format_stats19_ac(acc)
  }else if(file.type  == ".por"){
    acc <- as.data.frame(as.data.set(spss.portable.file(record[1])))
  }else{
    message("Unknow File type",record[1])
  }

  if(identical(names(acc), acc.names.low) | identical(names(acc), acc.names.upper)){
    names(acc) <- acc.names.new
  }else if(identical(names(acc), acc.names.low.99)){
    acc$a1_26 <- NULL #get variaible post 1999
    names(acc) <- acc.names.new
  }else if(identical(names(acc), acc.names.11)){
    names(acc) <- acc.names.new.11
    acc$Did_Police_Officer_Attend_Scene_of_Accident <- NULL
    acc <- acc[,acc.names.new]
  }else{
    message("Acc name error ",record[1])
    stop()
  }

  file.type <- substring(record[2],nchar(record[2]) - 3, nchar(record[2]))
  if(file.type  == ".sav"){
    #cas <- read.spss(record[2],to.data.frame=TRUE)
    cas <- read.spss(record[2], to.data.frame=FALSE, use.value.labels = FALSE)
    cas <- lapply(cas, Int2Factor)
    cas <- as.data.frame(cas, stringsAsFactors = FALSE)
  }else if(file.type  == ".por"){
    cas <- as.data.frame(as.data.set(spss.portable.file(record[2])))
  }else{
    message("Unknow File type",record[2])
  }


  if(identical(names(cas), cas.names.low) | identical(names(cas), cas.names.upper)){
    names(cas) <- cas.names.new
  }else if(identical(names(cas), cas.names.11)){
    names(cas) <- cas.names.new.11
    cas$Pedestrian_Road_Maintenance_Worker <- NULL
    cas$SchoolPupil <- NA
    cas$SeatBelt <- NA
    cas$CasualtyType <- NA
    cas$Year <- NA
    cas <- cas[,cas.names.new]
  }else{
    message("Cas name error ",record[2])
    stop()
  }

  file.type <- substring(record[3],nchar(record[3]) - 3, nchar(record[3]))
  if(file.type  == ".sav"){
    #veh <- read.spss(record[3],to.data.frame=TRUE)
    veh <- read.spss(record[3], to.data.frame=FALSE, use.value.labels = FALSE)
    veh <- lapply(veh, Int2Factor)
    veh <- as.data.frame(veh, stringsAsFactors = FALSE)
  }else if(file.type  == ".por"){
    veh <- as.data.frame(as.data.set(spss.portable.file(record[3])))
  }else{
    message("Unknow File type",record[3])
  }


  if(identical(names(veh),veh.names.low)){

  }else if(identical(names(veh),veh.names.low.91)){

  }else if(identical(names(veh),veh.names.low.05)){
    veh$v2_23 <- NULL #Not avialable in all years
    veh$v2_28 <- NULL
    veh$v2_29 <- NULL
    names(veh) <- veh.name.new
  }else if(identical(names(veh),veh.names.upper)){
    names(veh) <- veh.name.new
  }else if(identical(names(veh),veh.names.11)){
    names(veh) <- veh.name.new
  }else{
    message("Veh name error ",record[3])
    stop()
  }


  #Clean up data
  #Create single date time field
  acc$Month <- match(acc$Month, month.name)
  acc$date <- ISOdatetime(year = acc$Year, month = acc$Month, day = acc$Day, hour = acc$Hour, min = acc$Minute, sec = 0)
  acc <- acc[,names(acc)[!names(acc) %in% c("Month","Day","Hour","Minute","Week")]]

  #Change factors
  classes <- lapply(acc, class)
  for(b in 1:length(classes)){
    if(classes[b] == "factor"){
      acc[,b] <- as.character(acc[,b])
    }
  }

  classes <- lapply(cas, class)
  for(b in 1:length(classes)){
    if(classes[b] == "factor"){
      cas[,b] <- as.character(cas[,b])
    }
  }

  classes <- lapply(veh, class)
  for(b in 1:length(classes)){
    if(classes[b] == "factor"){
      veh[,b] <- as.character(veh[,b])
    }
  }

  #Convert to SF
  acc$Easting <- acc$Easting * 10
  acc$Northing <- acc$Northing * 10
  acc <- st_as_sf(acc, coords = c("Easting","Northing"), crs = 27700, remove = T)

  acc.list[[a]] <- acc
  cas.list[[a]] <- cas
  veh.list[[a]] <- veh

  unlink(record, recursive = T)
  rm(acc,cas,veh)


}






