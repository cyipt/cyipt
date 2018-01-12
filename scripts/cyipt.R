#Master Control Script for Cyipt
# Will load in an run all necessary scripts

#This script assumes that you have certain pre-existing datasets
#in the right places that have been preformatted for CyIPT

###########################################
#Settings

#Regions are selected using the file  ../cyipt/input-data/RegionsToDo.csv
#To do a region just put y in the do column of this csv file

skip <- TRUE #Should the code skip regions that have already been done?
overwrite <- TRUE #Some stages overwrite existing files, for example by adding an extra column of data
                   #Note that not overwriting may cause later stagest to fail if they expect earlier stages
                   #resutls to be in the starting file
ncores <- 4 #Some functions use parallel processing how many clusters should be run?
verbose <- TRUE #Get extra messages and information


##########################################

library(sf)
library(osmdata)
library(tmap)
library(stringr)
library(dplyr)
library(parallel)
library(igraph)
tmap_mode("view")

#########################################

#Start of code
#Select regions to do using the regions to do file

regions.todo <- read.csv("../cyipt/input-data/RegionsToDo.csv", stringsAsFactors = F)
regions.todo <- regions.todo[!is.na(regions.todo$do),]
regions.todo <- regions.todo$region[regions.todo$do == "y"]

#regions.todo <- "Manchester"

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
source("scripts/prep_data/get_pct2.R")

#Step 6: Get road width esitmates
source("scripts/prep_data/get_widths.R")

#Step 7: Get Collisions
source("scripts/prep_data/get_collisions.R")

#Step 7:Evaluate Infrastrucutre Options
source("scripts/select_infra/select_infra.R")

#Step 8: Compare Widths Needed to Widths Available
source("scripts/select_infra/compare_widths.R")

#Step 9: Group into schemes
source("scripts/select_infra/make_schemes.R")

#Step 10: get uptake
source("scripts/uptake/calc_uptake_exposure3.R")

#Step 11: Summarise Schemes
source("scripts/select_infra/summarise_schemes.R")

#Step 12: Calculate Benefits
source("scripts/benefits/eval_benefits4.R")



#Step LAST: Export for DB
source("scripts/visualise/export_postGIS.R")
source("scripts/visualise/export_schemes.R")





#Display Finishing Message
tot.end <- Sys.time()
message(paste0("Finished, did ",length(regions.todo)," regions in ", round(as.numeric(difftime(tot.end,tot.start,units = "hours")),2) ," hours, at ",Sys.time()))
rm(tot.end,tot.start)
