DROP TABLE roads;
DROP TABLE roadtypes;
DROP TABLE schemes;
DROP INDEX IF EXISTS schemes_gindx;
DROP INDEX IF EXISTS roads_gindx;

CREATE TABLE IF NOT EXISTS roads (
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
      absenteeismbenefit DOUBLE NULL,
      healthdeathavoided DOUBLE NULL,
      healthbenefit DOUBLE NULL,
      qualitybenefit DOUBLE NULL,
      accidentsbenefit REAL NULL,
      co2saved DOUBLE NULL,
      ghgbenefit DOUBLE NULL,
      congestionbenefit DOUBLE NULL,
      totalBen DOUBLE NULL,
      costBenRatio REAL NULL,
      geotext GEOMETRY NOT NULL
    );

COPY schemes FROM '/home/malcolm/schemes.csv'  csv HEADER;

ALTER TABLE schemes OWNER to cyipt;

CREATE INDEX schemes_gindx ON schemes USING GIST (geotext);
