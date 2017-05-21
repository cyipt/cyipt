#!/usr/bin/env bash


# LICENSE: GPL2
# COPYRIGHT: CycleStreets Ltd, for CyIPT project

# Data download site at: https://data.gov.uk/dataset/gb-road-traffic-counts
# File format documentation: http://data.dft.gov.uk/gb-traffic-matrix/all-traffic-data-metadata.pdf



# Settings

# Download URL for "Major road network - shape file format"
downloadUrlNetwork=http://data.dft.gov.uk/gb-traffic-matrix/major-roads-link-network2016.zip
downloadUrlMajorRoadsByDirection=http://data.dft.gov.uk/gb-traffic-matrix/AADF-data-by-direction-major-roads.zip
downloadUrlMajorRoads=http://data.dft.gov.uk/gb-traffic-matrix/AADF-data-major-roads.zip
downloadUrlMinorRoads=http://data.dft.gov.uk/gb-traffic-matrix/AADF-data-minor-roads.zip

# Working directory (which the database server must be able to read from)
tmpDir=/tmp/


# Set to terminate if error
set -e

# Get script directory: see: http://stackoverflow.com/a/246128/180733
scriptDir="$( cd -P "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Ensure credentials file is present, and load credentials
if [ ! -f $scriptDir/.credentials.sh ] ; then
	echo "ERROR: The credentials file .credentials.sh is not present; please copy the template to create it."
	exit
fi
source $scriptDir/.credentials.sh


#
# ROAD NETWORK STRUCTURE DATA
#

# Switch to working directory
cd $tmpDir

# Download the data if not already downloaded
zipFile=major-roads-link-network.zip
if [ ! -f $zipFile ] ; then
	wget -O $zipFile $downloadUrlNetwork
fi

# Unzip the data to a directory containing the shapefile files
shapefileFolder=major-roads-link-network
if [ ! -d $shapefileFolder ] ; then
	unzip -q $zipFile -d $shapefileFolder
fi

# Convert the shapefile to a CSV file, maintaining the coordinates
csvFile=major-roads-link-network.csv
rm -f $csvFile
shapefilePath=`find $shapefileFolder -name "*.shp"`	# E.g. major-roads-link-network/major-roads-link-networkXXXX.shp where XXXX is the data release year
ogr2ogr -f CSV -t_srs crs:84 -lco GEOMETRY=AS_WKT $csvFile $shapefilePath

# Cleanup to remove the shapefile folder (safely)
rm ./"${shapefileFolder}"/*
rmdir "${shapefileFolder}"

# Convert the linestring to four separate fields
sed -i.bak 's/"LINESTRING (//g' $csvFile
sed -i.bak 's/)"//g' $csvFile
sed -i.bak 's/ /,/g' $csvFile
sed -i.bak 's/WKT/startLongitude,startLatitude,finishLongitude,finishLatitude/g' $csvFile

# Convert count point to integer
sed -i.bak 's/.00000000000,/,/g' $csvFile

# Rename fields
sed -i.bak 's/CP_Number/id/g' $csvFile
sed -i.bak 's/RoadNumber/road/g' $csvFile

# Reorder fields
sed -i.bak -E 's/^(.+),(.+),(.+),(.+),(.+),(.+)/\5,\6,\1,\2,\3,\4/g' $csvFile

# Confirm completion
rm $csvFile.bak
echo "Data successfully downloaded and converted to CSV file now at $csvFile"


#
# ROAD DATA
#

# Download and unpack each dataset
files=("$downloadUrlMajorRoadsByDirection" "$downloadUrlMajorRoads" "$downloadUrlMinorRoads")
for file in ${files[@]} ; do
	
	# Download the zip file if not already downloaded
	zipFile=`basename $file`
	if [ ! -f $zipFile ] ; then
		wget $file
	fi
	
	# Unzip the zip file
	dataFile="${zipFile%.zip}.csv"
	if [ ! -f $dataFile ] ; then
		unzip -q $zipFile
	fi
	
done

# Connect to the database and run the SQL script
mysql --local-infile -h $hostname -u $username -p$password $database < $scriptDir/trafficcounts.sql

