DROP TABLE roads;
DROP TABLE roadtypes;
DROP TABLE schemes;
DROP INDEX IF EXISTS schemes_gindx;
DROP INDEX IF EXISTS roads_gindx;

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

CREATE INDEX roads_gindx ON roads USING GIST (geotext);

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

CREATE TABLE IF NOT EXISTS schemes (
		idGlobal INT PRIMARY KEY,
		groupid  INT NOT NULL,
		region VARCHAR(255) NULL,
		cost REAL NULL,
		costperperson REAL NULL,
		ncyclebefore INT NULL,
		ncycleafter INT NULL,
		infratype VARCHAR(255) NULL,
		change INT NULL,
		per REAL NULL,
		carkm REAL NULL,
		length REAL NULL,
		jouneyqualben REAL NULL,
		healthdeathavoided REAL NULL,
		healthbenefit REAL NULL,
		congestionbenefit REAL NULL,
		totalBen REAL NULL,
		costBenRatio REAL NULL,
		geotext GEOMETRY NOT NULL
    );

COPY schemes FROM '/home/malcolm/schemes.csv'  csv HEADER;

ALTER TABLE schemes OWNER to cyipt;

CREATE INDEX schemes_gindx ON schemes USING GIST (geotext);
