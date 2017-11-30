DROP TABLE IF EXISTS pctlsoa;
DROP INDEX IF EXISTS pctlsoa_gindx;

CREATE TABLE IF NOT EXISTS pctlsoa (
		idn INT PRIMARY KEY,
		id VARCHAR(19) NOT NULL,
		total INT NOT NULL,
		cyclists INT NOT NULL,
		drivers INT NOT NULL,
		hilliness REAL NOT NULL,
		geotext GEOMETRY NOT NULL
    );

COPY pctlsoa FROM '/home/malcolm/pctlsoa.csv'  csv HEADER;

ALTER TABLE pctlsoa OWNER to cyipt;

CREATE INDEX pctlsoa_gindx ON pctlsoa USING GIST (geotext);
