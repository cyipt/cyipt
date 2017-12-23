# TO handle SPSS files with duplicated factors
# from https://dadoseteorias.wordpress.com/2017/04/29/read-spss-duplicated-levels/
Int2Factor <- function(x)
{
  if(!is.null(attr(x, "value.labels"))){
    vlab <- attr(x, "value.labels")
    if(sum(duplicated(vlab)) > 0)
      cat("Duplicated levels:", vlab, "\n")
    else if(sum(duplicated(names(vlab))) > 0)
      cat("Duplicated labels:",
          names(vlab)[duplicated(names(vlab))], "\n")
    else
      x <- factor(x, levels = as.numeric(vlab),
                  labels = names(vlab))
  }
  x
}



stats19.import <- function(file, type = c("acc", "cas", "veh")){
  #Get File Type
  #And open the file
  file.type <- substring(file,nchar(file) - 3, nchar(file))
  if(file.type  == ".sav"){
    dat <- read.spss(file, to.data.frame=FALSE, use.value.labels = FALSE)
    dat <- lapply(dat, Int2Factor)
    dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  }else if(file.type  == ".por"){
    dat <- as.data.frame(as.data.set(spss.portable.file(file)))
  }else if(file.type  == ".csv"){
    dat <- read.csv(file, header = T, stringsAsFactors = F)
  }else{
    message("Unknow File type",file)
  }

  #Get the possible name combinations
  dat.names <- read.csv(paste0("../stats19/data/",type,".csv"), stringsAsFactors = F, header = F)

  #Check which set of names we are using
  #Different years have different naming conventions and columns
  for(j in 1:nrow(dat.names)){
    compare <- as.character(as.vector(dat.names[j,2:ncol(dat.names)]))
    if(all(names(dat) %in% compare)){
      #Matched All Varaible Names but need to check order
      dat.names.sub <- data.frame(t(dat.names[c(1,j),2:ncol(dat.names)]), stringsAsFactors = F)
      names(dat.names.sub) <- c("new","old")
      dat.names.new <- dat.names.sub$new[match(names(dat), dat.names.sub$old)]
      names(dat) <- dat.names.new
      break #Stop loop as match found
    }
    if(j == nrow(dat.names)){
      #No match found
      message(paste0("Unknown Combination of Values for type = ",type))
      stop()
    }
  }

  #Check for any columns that are missing ann add them
  missing.col <- dat.names.sub$new[!(dat.names.sub$new %in% names(dat))]
  dat[,missing.col] <- NA

  #Reorder the dat into the standard order
  dat <- dat[,dat.names.sub$new]


  #Clean up dates in the Acc
  #To store a singe DateTime Field

  ##################################################################
  # Need updating with different 2015 date time format
  #########################################################

  if(type == "acc"){
    #Create single date time field
    dat$Month <- match(dat$Month, month.name)
    dat$DateTime <- ISOdatetime(year = dat$Year, month = dat$Month, day = dat$Day, hour = dat$Hour, min = dat$Minute, sec = 0)
    dat <- dat[,names(dat)[!names(dat) %in% c("Month","Day","Hour","Minute")]]
  }


  #Change factors to characters
  classes <- lapply(dat, class)
  for(b in 1:length(classes)){
    if(classes[b] == "factor"){
      dat[,b] <- as.character(dat[,b])
    }
  }

  #Convert Acc to SF
  if(type == "acc"){
    dat$Easting <- dat$Easting * 10
    dat$Northing <- dat$Northing * 10
    dat <- st_as_sf(dat, coords = c("Easting","Northing"), crs = 27700, remove = T)
  }


  #Return Data
  return(dat)

}






#Convert Stats19 Codes to Human readable strings
stats19.code2string <- function(dat,type = c("acc", "cas", "veh")) {
  #SF Data frame convert to data frame and then convert back at end
  #Otherwise numeric column subsetting does not work
  if("sf" %in% class(dat)){
    toSF <- TRUE
    dat <- as.data.frame(dat)
  }else{
    toSF <- FALSE
  }

  #Get the list of columns that can be revalued
  columns <- list.files(paste0("../stats19/data/",type,"/"))
  columns <- gsub(".csv","",columns)

  for(k in 1:ncol(dat)){
    #Skip Geometry and Date Time columns
    if(length(class(dat[,k])) == 1){
      #Check that we can do the replacement
      if((class(dat[,k]) == "integer") & (names(dat)[k] %in% columns)){
        lookup <- read.csv(paste0("../stats19/data/",type,"/",names(dat)[k],".csv"), stringsAsFactors = F, header = T)
        new.val <- lookup$label[match(dat[,k],lookup$code)]
        dat[,k] <- new.val
        message(paste0("Change Values for ",names(dat)[k]))
      }
    }

  }

  if(toSF){
    dat <- st_as_sf(dat)
  }

  return(dat)

}
