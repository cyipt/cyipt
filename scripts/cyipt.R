#Master Control Script for Cyipt
# Will load in an run all necessary scripts

#This script assumes that you have certain pre-existing datasets
#in the right places that have been preformatted for CyIPT

###########################################
#Settings

#Regions are selected using the file  ../cyipt/input-data/RegionsToDo.csv
#To do a region just put y in the do column of this csv file

skip <- FALSE #Should the code skip regions that have already been done?
overwrite <- FALSE #Some stages overwrite existing files, for example by adding an extra column of data
                   #Note that not overwriting maay cause later stagest to fail if they expect earlier stages
                   #resutls to be in the starting file
ncores <- 4 #Some functions use parallel processing how many clusters should be run?


##########################################


#Start of code
#Select regions to do using the regions to do file

regions.todo <- read.csv("../cyipt/input-data/RegionsToDo.csv", stringsAsFactors = F)
regions.todo <- regions.todo[!is.na(regions.todo$do),]
regions.todo <- regions.todo$region[regions.todo$do == "y"]

message("CyIPT will run for the following regions:")
print(regions.todo)

#Step 1: Download the Data
source("scripts/prep_data/download-osm.R")

#Clean Up regions.todo for the rest of the code
library(stringr)
regions.todo <- str_replace_all(regions.todo,"[[:punct:]]","")
regions.todo <- str_replace_all(regions.todo," ","")

#Step 2: Clean the OSM Tags
source("scripts/prep_data/clean_osm.R")

#Step 3:Split the lines at each junction
source("scripts/prep_data/prep_osm.R")

#Step 4: Get the PCT estimate of number of cyclists
source("scripts/prep_data/get_pct.R")

#Step 5: Get raod width esitmates
source("scripts/prep_data/get_widths2.R")

