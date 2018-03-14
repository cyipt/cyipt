#Master Control Script for Cyipt
# Will load in an run all necessary scripts

#This script assumes that you have certain pre-existing datasets
#in the right places that have been preformatted for CyIPT

###########################################
#Settings

#Regions are selected using the file  ../cyipt/input-data/RegionsToDo.csv
#To do a region just put y in the do column of this csv file

skip <- FALSE #Should the code skip regions that have already been done?
overwrite <- TRUE #Some stages overwrite existing files, for example by adding an extra column of data
                   #Note that not overwriting may cause later stagest to fail if they expect earlier stages
                   #resutls to be in the starting file
ncores <- 3 #Some functions use parallel processing how many clusters should be run?
verbose <- TRUE #Get extra messages and information
all.regions <- TRUE #Ignore the regions to do file and do all regions


##########################################

library(sf)
library(osmdata)
library(stringr)
library(dplyr)
library(parallel)
library(xgboost)
library(igraph)
library(tmap)
tmap_mode("view")

source("R/functions.R")
source("scripts/benefits/benefits_functions.R")

#########################################

#Start of code
#Select regions to do using the regions to do file
regions.todo <- read.csv("../cyipt/input-data/RegionsToDo.csv", stringsAsFactors = F)
if(!all.regions){
  regions.todo <- regions.todo[!is.na(regions.todo$do),]
  regions.todo <- regions.todo$region[regions.todo$do == "y"]
}else{
  regions.todo <- regions.todo$region
}
# regions.todo <- "Bristol" # Manually Force a Region

message("CyIPT will run for the following regions:")
print(regions.todo)

tot.start <- Sys.time()

#Step 1: Download the Data
source("scripts/prep_data/download-osm.R")

#Step 2: Clean the OSM Tags
source("scripts/prep_data/clean_osm.R")

#Step 3: Get traffic counts
source("scripts/prep_data/get_traffic.R")

#Step 4:Split the lines at each junction
source("scripts/prep_data/prep_osm.R")

#Step 5: Get the PCT estimate of number of cyclists
source("scripts/prep_data/get_pct.R")

#Step 6: Get road width esitmates
source("scripts/prep_data/get_widths.R")

#Step 7: Get Collisions
source("scripts/prep_data/get_collisions.R")

#Step 7:Evaluate Infrastrucutre Options
source("scripts/select_infra/select_infra.R")

#Step 8: Compare Widths Needed to Widths Available
source("scripts/select_infra/compare_widths.R")

#Step 9: Group into schemes
source("scripts/select_infra/make_schemes2.R")

#Step 10: Get Uptakes and Benfits
source("scripts/uptake/uptake_benefits_choicemodel.R")


#Step LAST: Export for DB
source("scripts/visualise/export_postGIS.R")
source("scripts/visualise/export_schemes.R")





#Display Finishing Message
tot.end <- Sys.time()
message(paste0("Finished, did ",length(regions.todo)," regions in ", round(as.numeric(difftime(tot.end,tot.start,units = "hours")),2) ," hours, at ",Sys.time()))
rm(tot.end,tot.start)
