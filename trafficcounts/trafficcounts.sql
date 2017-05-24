-- DfT traffic count processing script
-- Based on: http://hfcyclists.org.uk/wp/wp-content/uploads/2014/02/dft-roads-all-copy.txt
-- Written for a PostgreSQL installation; adapted for MySQL


-- If you haven't imported the westminster, borough and ward boundaries, which come from OS BoundaryLine, you'll need to do so


-- Import the roads network as a shape first


SET SESSION sql_mode = 'STRICT_ALL_TABLES';


DROP TABLE IF EXISTS major_roads_link_network;
CREATE TABLE major_roads_link_network (
	cp Integer NOT NULL,
	road text NOT NULL,
	startLongitude text NOT NULL,
	startLatitude text NOT NULL,
	finishLongitude text NOT NULL,
	finishLatitude text NOT NULL
);

ALTER TABLE major_roads_link_network ADD INDEX (cp);

-- COPY major_roads_link_network FROM '/tmp/major-roads-link-network.csv' WITH CSV HEADER;
LOAD DATA INFILE '/tmp/major-roads-link-network.csv'
	INTO TABLE major_roads_link_network
	FIELDS TERMINATED BY ','
	OPTIONALLY ENCLOSED BY '"'
	IGNORE 1 LINES
;

-- Create the geometry; this is already in WGS84
ALTER TABLE major_roads_link_network ADD geom LINESTRING NULL;
UPDATE major_roads_link_network SET geom = ST_GeomFromText( CONCAT('LINESTRING(', startLongitude, ' ', startLatitude, ',' , finishLongitude, ' ', finishLatitude, ')') );
ALTER TABLE major_roads_link_network CHANGE geom geom LINESTRING NOT NULL;





-- Now create and import the three tables


DROP TABLE IF EXISTS major_roads_direction;
CREATE TABLE major_roads_direction (
Year	Integer,
CP	Integer,
Estimation_method	text,
Estimation_method_detailed	text,
ONS_GOR_Name	text,
ONS_LA_Name	text,
Road	text,
RCat	text,
iDir	text,
S_Ref_E	text,
S_Ref_N	text,
S_Ref_Latitude	text,
S_Ref_Longitude	text,
A_Junction	text,
B_Junction	text,
LenNet	Double Precision,
LenNet_miles	Double Precision,
FdPC	Double Precision,
Fd2WMV	Double Precision,
FdCAR	Double Precision,
FdBUS	Double Precision,
FdLGV	Double Precision,
FdHGVR2	Double Precision,
FdHGVR3	Double Precision,
FdHGVR4	Double Precision,
FdHGVA3	Double Precision,
FdHGVA5	Double Precision,
FdHGVA6	Double Precision,
FdHGV	Double Precision,
FdAll_MV Double Precision
);

-- COPY major_roads_direction FROM 'C:\csvimport\AADF-data-by-direction-major-roads.csv' WITH CSV HEADER;
LOAD DATA INFILE '/tmp/AADF-data-by-direction-major-roads.csv'
	INTO TABLE major_roads_direction
	FIELDS TERMINATED BY ','
	OPTIONALLY ENCLOSED BY '"'
	IGNORE 1 LINES
;


DROP TABLE IF EXISTS major_roads;
CREATE TABLE major_roads (
Year	Integer,
CP	Integer,
Estimation_method	text,
Estimation_method_detailed	text,
ONS_GOR_Name	text,
ONS_LA_Name	text,
Road	text,
RCat	text,
S_Ref_E	text,
S_Ref_N	text,
S_Ref_Latitude	text,
S_Ref_Longitude	text,
A_Junction	text,
B_Junction	text,
LenNet	Double Precision,
LenNet_miles	Double Precision,
FdPC	Double Precision,
Fd2WMV	Double Precision,
FdCar	Double Precision,
FdBUS	Double Precision,
FdLGV	Double Precision,
FdHGVR2	Double Precision,
FdHGVR3	Double Precision,
FdHGVR4	Double Precision,
FdHGVA3	Double Precision,
FdHGVA5	Double Precision,
FdHGVA6	Double Precision,
FdHGV	Double Precision,
FdAll_MV Double Precision
);

-- COPY major_roads FROM 'C:\csvimport\AADF-data-major-roads.csv' WITH CSV HEADER;
LOAD DATA INFILE '/tmp/AADF-data-major-roads.csv'
	INTO TABLE major_roads
	FIELDS TERMINATED BY ','
	OPTIONALLY ENCLOSED BY '"'
	IGNORE 1 LINES
;


DROP TABLE IF EXISTS minor_roads;
CREATE TABLE minor_roads(
Year	Integer,
CP	Integer,
Estimation_method	text,
Estimation_method_detailed	text,
ONS_GOR_Name	text,
ONS_LA_Name	text,
Road	text,
RCat	text,
S_Ref_E	text,
S_Ref_N	text,
S_Ref_Latitude	text,
S_Ref_Longitude	text,
FdPC	Double Precision,
Fd2WMV	Double Precision,
FdCar	Double Precision,
FdBUS	Double Precision,
FdLGV	Double Precision,
FdHGVR2	Double Precision,
FdHGVR3	Double Precision,
FdHGVR4	Double Precision,
FdHGVA3	Double Precision,
FdHGVA5	Double Precision,
FdHGVA6	Double Precision,
FdHGV	Double Precision,
FdAll_MV Double Precision
);

-- COPY minor_roads FROM 'C:\csvimport\AADF-data-minor-roads.csv' WITH CSV HEADER;
LOAD DATA INFILE '/tmp/AADF-data-minor-roads.csv'
	INTO TABLE minor_roads
	FIELDS TERMINATED BY ','
	OPTIONALLY ENCLOSED BY '"'
	IGNORE 1 LINES
;



-- Make a simple collated table of all the count points we need to find
DROP TABLE IF EXISTS cptolocate;
CREATE TABLE cptolocate AS
	SELECT DISTINCT CP, S_Ref_E, S_Ref_N, S_Ref_Longitude AS longitude, S_Ref_Latitude AS latitude FROM (
		SELECT CP, S_Ref_E, S_Ref_N, S_Ref_Longitude, S_Ref_Latitude FROM major_roads_direction
		UNION
		SELECT CP, S_Ref_E, S_Ref_N, S_Ref_Longitude, S_Ref_Latitude FROM minor_roads
		UNION
		SELECT CP, S_Ref_E, S_Ref_N, S_Ref_Longitude, S_Ref_Latitude FROM major_roads
	) AS grouped
;

-- Add a geometry column to the locations we are finding
-- SELECT AddGeometryColumn ('public','cptolocate', 'geom_gb', 27700, 'POINT', 2);
ALTER TABLE cptolocate ADD geom_gb POINT;

-- Create geometry points for the tables in the 27700 SRID
-- UPDATE cptolocate SET geom_gb=ST_GeomFromText('POINT('||S_Ref_E||' '||S_Ref_N||')',27700);
UPDATE cptolocate SET geom_gb = ST_GeomFromText( CONCAT('POINT(', S_Ref_E, ' ', S_Ref_N, ')') , 27700);

-- Add the spatial index
-- CREATE INDEX gist_rgb ON cptolocate USING gist (geom_gb);
ALTER TABLE cptolocate CHANGE geom_gb geom_gb POINT NOT NULL;
CREATE SPATIAL INDEX gist_rgb ON cptolocate (geom_gb);

-- As used before , these regions are shapefiles imported into postgresql using the shapefile importer (SRID 27700)
DROP TABLE IF EXISTS RoadCPLocationsWestminster;
CREATE TABLE RoadCPLocationsWestminster AS
SELECT lo.cp, wm.name, wm.code
FROM cptolocate as lo
JOIN westminster_const_region as wm on ST_Within(lo.geom_gb, wm.geom);

DROP TABLE IF EXISTS RoadCPLocationsBorough;
CREATE TABLE RoadCPLocationsBorough AS
SELECT lo.cp, bo.name, bo.code
FROM cptolocate as lo
JOIN district_borough_unitary_region as bo on ST_Within(lo.geom_gb, bo.geom);

DROP TABLE IF EXISTS RoadCPLocationsWard;
CREATE TABLE RoadCPLocationsWard AS
SELECT lo.cp, wa.name, wa.code
FROM cptolocate as lo
JOIN district_borough_unitary_ward_region as wa on ST_Within(lo.geom_gb, wa.geom);

DROP TABLE IF EXISTS RoadCPLocationsFound;
CREATE TABLE RoadCPLocationsFound AS
SELECT lo.cp, lo.latitude, lo.longitude, wm.name as westminstername,wm.code as westminstercode, bo.name as boroughname, bo.code as boroughcode, wa.name as wardname, wa.code as wardcode, geom_gb
FROM cptolocate lo
LEFT OUTER JOIN RoadCPLocationsWestminster as wm ON lo.cp = wm.cp
LEFT OUTER JOIN RoadCPLocationsBorough as bo ON lo.cp = bo.cp
LEFT OUTER JOIN RoadCPLocationsWard as wa ON lo.cp = wa.cp;

ALTER TABLE RoadCPLocationsFound ADD INDEX (cp);


-- create the PCU counts and rename the vehicle counts to something meaningful. This uses a subselect as the main query which is a union of the tables in the non-directed major roads data and the minor roads data
DROP TABLE IF EXISTS pcu_roads;

CREATE TABLE pcu_roads AS
	SELECT
		year,
		cp,
		Road,
		RCat,
		FdPC AS cycles,
		Fd2WMV AS p2w,
		FdCAR AS cars,
		FdBUS AS buses,
		FdLGV AS lgvs,
		FdHGVR2 AS mgvs,
		(FdHGVR3 + FdHGVR4 + FdHGVA3 + FdHGVA5 + FdHGVA6) AS hgvs,
		(Fd2WMV + FdCAR + FdBUS + FdLGV + FdHGVR2 + (FdHGVR3 + FdHGVR4 + FdHGVA3 + FdHGVA5 + FdHGVA6)) AS all_motors,
		FdPC * 0.2 AS cycle_pcu,
		Fd2WMV * 0.4 AS p2w_pcu,
		FdCAR AS car_pcu,
		FdBUS * 2 AS bus_pcu,
		FdLGV AS lgv_pcu,
		FdHGVR2 * 1.5 AS mgv_pcu,
		(FdHGVR3 + FdHGVR4 + FdHGVA3 + FdHGVA5 + FdHGVA6) * 2.3 AS hgv_pcu,
		Fd2WMV * 0.4 + FdCAR + FdBUS * 2 + FdLGV + FdHGVR2 * 1.5 + (FdHGVR3 + FdHGVR4 + FdHGVA3 + FdHGVA5 + FdHGVA6) * 2.3 AS all_motor_pcu
	FROM (
		SELECT year, cp, Road, RCat, FdPC, Fd2WMV, FdCAR, FdBUS, FdLGV, FdHGVR2, FdHGVR3, FdHGVR4, FdHGVA3, FdHGVA5, FdHGVA6 FROM major_roads
		UNION
		SELECT year, cp, Road, RCat, FdPC, Fd2WMV, FdCAR, FdBUS, FdLGV, FdHGVR2, FdHGVR3, FdHGVR4, FdHGVA3, FdHGVA5, FdHGVA6 FROM minor_roads
	) AS major_minor_roads
;

ALTER TABLE pcu_roads ADD INDEX (cp);


-- Create a conversion table to make the field RCat meaningful
DROP TABLE IF EXISTS x_road_cat;
CREATE TABLE x_road_cat (
	RCat text,
	road_type text
	);

INSERT INTO x_road_cat VALUES
('PM','M or Class A Principal Motorway'),
('PR','Class A Principal road in Rural area'),
('PU','Class A Principal road in Urban area'),
('TM','M or Class A Trunk Motorway'),
('TR','Class A Trunk road in Rural area'),
('TU','Class A Trunk road in Urban area'),
('BR','Class B road in Rural area'),
('BU','Class B road in Urban area'),
('CR','Class C road in Rural area'),
('CU','Class C road in Urban area'),
('UR','Class U road in Rural area'),
('UU','Class U road in Urban area');

-- Make a table to contain all the political details
DROP TABLE IF EXISTS pcu_roads_political;

CREATE TABLE pcu_roads_political AS
	SELECT
		lo.westminstername, lo.boroughname, lo.wardname,
		pcu.cp, pcu.road,
		road_type,
		pcu.year,
		cycles, p2w, cars, buses, lgvs, mgvs, hgvs, all_motors, cycle_pcu, p2w_pcu, car_pcu, bus_pcu, lgv_pcu, mgv_pcu, hgv_pcu, all_motor_pcu,
		lo.latitude, lo.longitude,
--		ST_AsKML(ST_Transform(road.geom,4326)) AS road_geom
		ST_AsGeoJSON(road.geom) AS road_geom
	FROM pcu_roads AS pcu
	INNER JOIN RoadCPLocationsFound lo ON pcu.cp = lo.cp
	INNER JOIN x_road_cat ON pcu.rcat = x_road_cat.rcat
	LEFT JOIN major_roads_link_network road ON pcu.cp = road.cp
;


-- do cross tabs for: cycles, p2w, cars, buses, lgvs, mgvs, hgvs, all_motors, all_motor_pcu

DROP TABLE IF EXISTS aadf_cycles_years;

Create TABLE aadf_cycles_years AS
  SELECT * FROM
  crosstab(
 'SELECT cp, year, cycles
 FROM pcu_roads
 ORDER BY cp ASC',
 'SELECT DISTINCT year FROM pcu_roads ORDER BY year ASC')
 AS ct(cp int, cycles_00 int, cycles_01 int, cycles_02 int, cycles_03 int, cycles_04 int, cycles_05 int, cycles_06 int, cycles_07 int, cycles_08 int, cycles_09 int, cycles_10 int, cycles_11 int, cycles_12 int);


DROP TABLE IF EXISTS aadf_p2w_years;

Create TABLE aadf_p2w_years AS
  SELECT * FROM
  crosstab(
 'SELECT cp, year,  p2w
 FROM pcu_roads
 ORDER BY cp ASC',
 'SELECT DISTINCT year FROM pcu_roads ORDER BY year ASC')
 AS ct(cp int,  p2w_00 int,  p2w_01 int,  p2w_02 int,  p2w_03 int,  p2w_04 int,  p2w_05 int,  p2w_06 int,  p2w_07 int,  p2w_08 int,  p2w_09 int,  p2w_10 int,  p2w_11 int,  p2w_12 int);


DROP TABLE IF EXISTS aadf_cars_years;

Create TABLE aadf_cars_years AS
  SELECT * FROM
  crosstab(
 'SELECT cp, year,  cars
 FROM pcu_roads
 ORDER BY cp ASC',
 'SELECT DISTINCT year FROM pcu_roads ORDER BY year ASC')
 AS ct(cp int,  cars_00 int,  cars_01 int,  cars_02 int,  cars_03 int,  cars_04 int,  cars_05 int,  cars_06 int,  cars_07 int,  cars_08 int,  cars_09 int,  cars_10 int,  cars_11 int,  cars_12 int);


DROP TABLE IF EXISTS aadf_buses_years;

Create TABLE aadf_buses_years AS
  SELECT * FROM
  crosstab(
 'SELECT cp, year, buses
 FROM pcu_roads
 ORDER BY cp ASC',
 'SELECT DISTINCT year FROM pcu_roads ORDER BY year ASC')
 AS ct(cp int, buses_00 int, buses_01 int, buses_02 int, buses_03 int, buses_04 int, buses_05 int, buses_06 int, buses_07 int, buses_08 int, buses_09 int, buses_10 int, buses_11 int, buses_12 int);

DROP TABLE IF EXISTS aadf_lgvs_years;

Create TABLE aadf_lgvs_years AS
  SELECT * FROM
  crosstab(
 'SELECT cp, year, lgvs
 FROM pcu_roads
 ORDER BY cp ASC',
 'SELECT DISTINCT year FROM pcu_roads ORDER BY year ASC')
 AS ct(cp int, lgvs_00 int, lgvs_01 int, lgvs_02 int, lgvs_03 int, lgvs_04 int, lgvs_05 int, lgvs_06 int, lgvs_07 int, lgvs_08 int, lgvs_09 int, lgvs_10 int, lgvs_11 int, lgvs_12 int);

DROP TABLE IF EXISTS aadf_mgvs_years;

Create TABLE aadf_mgvs_years AS
  SELECT * FROM
  crosstab(
 'SELECT cp, year, mgvs
 FROM pcu_roads
 ORDER BY cp ASC',
 'SELECT DISTINCT year FROM pcu_roads ORDER BY year ASC')
 AS ct(cp int, mgvs_00 int, mgvs_01 int, mgvs_02 int, mgvs_03 int, mgvs_04 int, mgvs_05 int, mgvs_06 int, mgvs_07 int, mgvs_08 int, mgvs_09 int, mgvs_10 int, mgvs_11 int, mgvs_12 int);


DROP TABLE IF EXISTS aadf_hgvs_years;

Create TABLE aadf_hgvs_years AS
  SELECT * FROM
  crosstab(
 'SELECT cp, year, hgvs
 FROM pcu_roads
 ORDER BY cp ASC',
 'SELECT DISTINCT year FROM pcu_roads ORDER BY year ASC')
 AS ct(cp int, hgvs_00 int, hgvs_01 int, hgvs_02 int, hgvs_03 int, hgvs_04 int, hgvs_05 int, hgvs_06 int, hgvs_07 int, hgvs_08 int, hgvs_09 int, hgvs_10 int, hgvs_11 int, hgvs_12 int);


DROP TABLE IF EXISTS aadf_motors_years;

Create TABLE aadf_motors_years AS
  SELECT * FROM
  crosstab(
 'SELECT cp, year, all_motors
 FROM pcu_roads
 ORDER BY cp ASC',
 'SELECT DISTINCT year FROM pcu_roads ORDER BY year ASC')
 AS ct(cp int, motors_00 int, motors_01 int, motors_02 int, motors_03 int, motors_04 int, motors_05 int, motors_06 int, motors_07 int, motors_08 int, motors_09 int, motors_10 int, motors_11 int, motors_12 int);


DROP TABLE IF EXISTS aadf_motor_pcu_years;

Create TABLE aadf_motor_pcu_years AS
  SELECT * FROM
  crosstab(
 'SELECT cp, year, all_motor_pcu
 FROM pcu_roads
 ORDER BY cp ASC',
 'SELECT DISTINCT year FROM pcu_roads ORDER BY year ASC')
 AS ct(cp int, motor_pcu_00 double precision, motor_pcu_01 double precision, motor_pcu_02 double precision, motor_pcu_03 double precision, motor_pcu_04 double precision, motor_pcu_05 double precision, motor_pcu_06 double precision, motor_pcu_07 double precision, motor_pcu_08 double precision, motor_pcu_09 double precision, motor_pcu_10 double precision, motor_pcu_11 double precision, motor_pcu_12 double precision);

-- Make a table of max year and CP
DROP TABLE IF EXISTS aadf_uk_cp_maxyear;

CREATE TABLE aadf_uk_cp_maxyear AS
	SELECT cp AS maxyear_cp, MAX(year) as maxyear
	FROM pcu_roads_political
	GROUP BY cp
;


-- Make a big table of all the count points
-- This combines all of the pivoted years and uses as the main table (aliased as pol) a subselect query which makes sure you only get the full data for the last year it had a full count. This enables us to create a very flat table ideally suited to google fusion tables.

DROP TABLE IF EXISTS aadf_uk_counts_pcu;

CREATE TABLE aadf_uk_counts_pcu AS
SELECT
	pol.westminstername, pol.boroughname, pol.wardname, pol.cp, pol.road, pol.road_type, pol.maxyear, pol.cycle_pcu, pol.p2w_pcu, pol.car_pcu, pol.bus_pcu, pol.lgv_pcu, pol.mgv_pcu, pol.hgv_pcu, pol.latitude, pol.longitude, pol.road_geom,
	cycles_00, cycles_01, cycles_02, cycles_03, cycles_04, cycles_05, cycles_06, cycles_07, cycles_08, cycles_09, cycles_10, cycles_11, cycles_12,
	p2w_00, p2w_01, p2w_02, p2w_03, p2w_04, p2w_05, p2w_06, p2w_07, p2w_08, p2w_09, p2w_10, p2w_11, p2w_12,
	cars_00, cars_01, cars_02, cars_03, cars_04, cars_05, cars_06, cars_07, cars_08, cars_09, cars_10, cars_11, cars_12,
	buses_00, buses_01, buses_02, buses_03, buses_04, buses_05, buses_06, buses_07, buses_08, buses_09, buses_10, buses_11, buses_12,
	lgvs_00, lgvs_01, lgvs_02, lgvs_03, lgvs_04, lgvs_05, lgvs_06, lgvs_07, lgvs_08, lgvs_09, lgvs_10, lgvs_11, lgvs_12,
	mgvs_00, mgvs_01, mgvs_02, mgvs_03, mgvs_04, mgvs_05, mgvs_06, mgvs_07, mgvs_08, mgvs_09, mgvs_10, mgvs_11, mgvs_12,
	hgvs_00, hgvs_01, hgvs_02, hgvs_03, hgvs_04, hgvs_05, hgvs_06, hgvs_07, hgvs_08, hgvs_09, hgvs_10, hgvs_11, hgvs_12,
	motors_00, motors_01, motors_02, motors_03, motors_04, motors_05, motors_06, motors_07, motors_08, motors_09, motors_10, motors_11, motors_12,
	motor_pcu_00, motor_pcu_01, motor_pcu_02, motor_pcu_03, motor_pcu_04, motor_pcu_05, motor_pcu_06, motor_pcu_07, motor_pcu_08, motor_pcu_09, motor_pcu_10, motor_pcu_11, motor_pcu_12
FROM
	(
		SELECT *
		FROM pcu_roads_political pol
		INNER JOIN aadf_uk_cp_maxyear my ON pol.cp = my.maxyear_cp AND my.maxyear = pol.year
	) AS pol
	LEFT JOIN aadf_cycles_years on pol.cp = aadf_cycles_years.cp
	LEFT JOIN aadf_p2w_years on pol.cp = aadf_p2w_years.cp
	LEFT JOIN aadf_cars_years on pol.cp = aadf_cars_years.cp
	LEFT JOIN aadf_buses_years on pol.cp = aadf_buses_years.cp
	LEFT JOIN aadf_lgvs_years on pol.cp = aadf_lgvs_years.cp
	LEFT JOIN aadf_mgvs_years on pol.cp = aadf_mgvs_years.cp
	LEFT JOIN aadf_hgvs_years on pol.cp = aadf_hgvs_years.cp
	LEFT JOIN aadf_motors_years on pol.cp = aadf_motors_years.cp
	LEFT JOIN aadf_motor_pcu_years on pol.cp = aadf_motor_pcu_years.cp
WHERE
	(pol.road_geom is not null OR pol.latitude is not null)


-- done!
