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

# Download URL for Ordance Survey Boundary-Line™ open data
# See: https://www.ordnancesurvey.co.uk/opendatadownload/products.html#BDLINE
# Using scriptably-downloadable MySociety cache at: http://parlvid.mysociety.org/os/
downloadUrlBoundaryLine=http://parlvid.mysociety.org/os/bdline_gb-2017-05.zip

# Working directory (which the database server must be able to read from)
tmpDir=/tmp/


# Set to terminate if error
set -e

date

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
	
	# Ensure Unix line endings, to ensure reliable strict SQL import (avoids last INT column being interpreted as e.g. "12\r")
	dos2unix $dataFile
done

# Clean up by-direction data for the following rows; see: http://stackoverflow.com/questions/2675323/mysql-load-null-values-from-csv-data
# 2006,80441 2006,80442 2006,80443 2006,80444 2006,80445 2006,80446 2006,80447 2006,80448 2006,80462 2006,80495 2006,80509 2006,80566
sed -i.bak 's/,,,,,,,,,,,,/,\\N,\\N,\\N,\\N,\\N,\\N,\\N,\\N,\\N,\\N,\\N,\\N/' AADF-data-by-direction-major-roads.csv
rm AADF-data-by-direction-major-roads.csv.bak


#
# BOUNDARY DATA
#

# Obtain boundary data
boundaryFile=boundaryline.zip
if [ ! -f $boundaryFile ] ; then
	wget -O $boundaryFile $downloadUrlBoundaryLine
fi

# Extract relevant sections of boundary file
unzip -j -o $boundaryFile "Data/GB/westminster_const_region.*" "Data/GB/district_borough_unitary_region.*" "Data/GB/district_borough_unitary_ward_region.*"

# Get intial MySQL value
mysqlInitialMaxAllowedPacket=$(mysql -s -N -h $hostname -u $username -p$password $database -e "SELECT @@global.max_allowed_packet;")

# Convert boundary data Shapefiles
# Test ogr2ogr connection to MySQL using: `ogrinfo MYSQL:$database,host=$hostname,user=$username,password=$password` ; see: http://mapserver.org/uk/input/vector/mysql.html
shapefiles=('westminster_const_region' 'district_borough_unitary_region' 'district_borough_unitary_ward_region');
for shapefile in ${shapefiles[@]} ; do
	mysql -h $hostname -u $username -p$password $database -e "SET GLOBAL max_allowed_packet=(32*1024*1024);"
	ogr2ogr -f MySQL MySQL:$database,host=$hostname,user=$username,password=$password ${shapefile}.shp -nln $shapefile -update -overwrite -lco engine=MYISAM
	mysql -h $hostname -u $username -p$password $database -e "SET GLOBAL max_allowed_packet=${mysqlInitialMaxAllowedPacket};"
	mysql -h $hostname -u $username -p$password $database -e "ALTER TABLE $shapefile CHANGE SHAPE geom GEOMETRY NOT NULL;"
	# http://stackoverflow.com/questions/3463942/change-srid-in-mysql
	# "Limitation: SRS information is stored using the OGC Simple Features for SQL layout" i.e. not in the geom field itself ; see: http://www.gdal.org/drv_mysql.html
	# 282 seconds:
	mysql -h $hostname -u $username -p$password $database -e "UPDATE $shapefile SET geom = ST_GeomFromGeoJSON(ST_AsGeoJSON(geom), 2, 27700);"
	rm ${shapefile}.*
done
mysql -h $hostname -u $username -p$password $database -e "DROP TABLE IF EXISTS geometry_columns, spatial_ref_sys;"


# Connect to the database and run the SQL script
mysql --local-infile -h $hostname -u $username -p$password $database < $scriptDir/trafficcounts.sql


date
