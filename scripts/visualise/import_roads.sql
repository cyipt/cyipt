DROP TABLE roads;
DROP TABLE roadtypes;

CREATE TABLE IF NOT EXISTS roads (
		idGlobal INT PRIMARY KEY,
		id INT NOT NULL,
		osmid INT NULL,
		region VARCHAR(255) NULL,
		name VARCHAR(255) NULL,
		ref VARCHAR(20) NULL,
		highway VARCHAR(20) NULL,
		junction VARCHAR(20) NULL,
		elevation VARCHAR(8) NULL,
		maxspeed SMALLINT NULL,
		segregated VARCHAR(3) NULL,
		pctcensus INT NULL,
		pctgov INT NULL,
		pctgen INT NULL,
		pctdutch INT NULL,
		pctebike INT NULL,
		width REAL NULL,
		widthpath REAL NULL,
		aadt REAL NULL,
		ncycle REAL NULL,
		Recommended VARCHAR(255) NULL,
		DesWidth REAL NULL,
		MinWidth REAL NULL,
		DesSeparation REAL NULL,
		MinSeparation REAL NULL,
		Existing VARCHAR(255) NULL,
		Change VARCHAR(20) NULL,
		costperm REAL NULL,
		length REAL NULL,
		costTotal INT NULL,
		groupid INT NULL,
		rtid INT NULL,
		geotext GEOMETRY NOT NULL
    );

COPY roads FROM '/home/malcolm/roads.csv'  csv HEADER;

ALTER TABLE roads OWNER to cyipt;

CREATE TABLE IF NOT EXISTS roadtypes(

		rtid INT PRIMARY KEY,
		roadtype VARCHAR(100) NULL,
		onewaysummary VARCHAR(40) NULL,
		sidewalk VARCHAR(20) NULL,
		cyclewayleft VARCHAR(20) NULL,
		lanespsvforward SMALLINT NULL,
		lanesforward SMALLINT NULL,
		lanesbackward SMALLINT NULL,
		lanespsvbackward SMALLINT NULL,
		cyclewayright VARCHAR(20) NULL
    );


COPY roadtypes FROM '/home/malcolm/roadtypes.csv'  csv HEADER;

ALTER TABLE roadtypes OWNER to cyipt;
