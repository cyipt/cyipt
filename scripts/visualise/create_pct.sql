DROP TABLE IF EXISTS pctlsoa;
DROP INDEX IF EXISTS pctlsoa_gindx;

CREATE TABLE IF NOT EXISTS pctlsoa (
		idn INT PRIMARY KEY,
		id VARCHAR(19) NOT NULL,
		allCommuters INT NULL,
		onfoot INT NULL,
		carorvan INT NULL,
		pctcensus INT NULL,
		pctgov INT NULL,
		pctgen INT NULL,
		pctdutch INT NULL,
		pctebike INT NULL,
		fastDist REAL NULL,
		fastTime REAL NULL,
		fastSlope REAL NULL,
		fastGeom GEOMETRY NULL,
		quietDist REAL NULL,
		quietSlope REAL NULL,
		quietTime REAL NULL,
		quietGeom GEOMETRY NULL,
		straightDist REAL NULL,
		straightGeom GEOMETRY NULL,
		fastCircuity REAL NULL,
		quietCircuity REAL NULL,
		quietDiversion REAL NULL,
		quietDiverTime REAL NULL
    );

COPY pctlsoa FROM '/home/malcolm/pct.csv'  csv HEADER;

ALTER TABLE pctlsoa OWNER to cyipt;

CREATE INDEX pctlsoa_gindx ON pctlsoa USING GIST (fastGeom);
