DROP TABLE alt_roads;
DROP TABLE alt_roadtypes;
DROP TABLE alt_schemes;
DROP INDEX IF EXISTS alt_schemes_gindx;
DROP INDEX IF EXISTS alt_roads_gindx;

CREATE TABLE IF NOT EXISTS alt_roads (
		idGlobal INT PRIMARY KEY,
		id INT NOT NULL,
		osmid INT NULL,
		region VARCHAR(30) NULL,
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
		pcttotal INT NULL,
		width REAL NULL,
		widthpath REAL NULL,
		calcwidthnow REAL NULL,
		calcwidthrec REAL NULL,
		widthdiffnow REAL NULL,
		widthdiffrec REAL NULL,
		widthstatus VARCHAR(30) NULL,
		ncollisions INT NULL,
		bikeCas INT NULL,
		totalCas INT NULL,
		totalVeh INT NULL,
		aadt REAL NULL,
		ncycle REAL NULL,
		Recommended VARCHAR(35) NULL,
		DesWidth REAL NULL,
		MinWidth REAL NULL,
		DesSeparation REAL NULL,
		MinSeparation REAL NULL,
		Existing VARCHAR(70) NULL,
		Change VARCHAR(20) NULL,
		costperm REAL NULL,
		length REAL NULL,
		costTotal INT NULL,
		groupid INT NULL,
		rtid INT NULL,
		geotext GEOMETRY NOT NULL
    );

COPY alt_roads FROM '/home/malcolm/roads.csv'  csv HEADER;

ALTER TABLE alt_roads OWNER to cyipt;

CREATE INDEX alt_roads_gindx ON alt_roads USING GIST (geotext);

CREATE TABLE IF NOT EXISTS alt_roadtypes(

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


COPY alt_roadtypes FROM '/home/malcolm/roadtypes.csv'  csv HEADER;

ALTER TABLE alt_roadtypes OWNER to cyipt;

CREATE TABLE IF NOT EXISTS alt_schemes (
      idGlobal INT PRIMARY KEY,
      groupid INT NOT NULL,
      region VARCHAR(255) NULL,
      cost REAL NULL,
      costperperson REAL NULL,
      ncyclebefore INT NULL,
      ncycleafter INT NULL,
      infratype VARCHAR(255) NULL,
      change INT NULL,
      per REAL NULL,
      length REAL NULL,
      ndrivebefore INT NULL,
      ndriveafter INT NULL,
      carkmbefore REAL NULL,
      carkmafter REAL NULL,
      carkm REAL NULL,
      absenteeismbenefit REAL NULL,
      healthdeathavoided REAL NULL,
      healthbenefit REAL NULL,
      qualitybenefit REAL NULL,
      accidentsbenefit REAL NULL,
      co2saved REAL NULL,
      ghgbenefit REAL NULL,
      congestionbenefit REAL NULL,
      totalBen REAL NULL,
      costBenRatio REAL NULL,
      geotext GEOMETRY NOT NULL
    );

COPY alt_schemes FROM '/home/malcolm/schemes.csv'  csv HEADER;

ALTER TABLE alt_schemes OWNER to cyipt;

CREATE INDEX alt_schemes_gindx ON alt_schemes USING GIST (geotext);
