DROP TABLE IF EXISTS schemes;
DROP INDEX IF EXISTS schemes_gindx;

CREATE TABLE IF NOT EXISTS schemes (
		idGlobal INT PRIMARY KEY,
		region VARCHAR(255) NULL,
		schgroup INT NULL,
		schtype VARCHAR(255) NULL,
		length REAL NULL,
		cost REAL NULL,
		geotext GEOMETRY NOT NULL
    );

COPY schemes FROM '/home/malcolm/schemes.csv'  csv HEADER;

ALTER TABLE schemes OWNER to cyipt;

CREATE INDEX schemes_gindx ON schemes USING GIST (geotext);
