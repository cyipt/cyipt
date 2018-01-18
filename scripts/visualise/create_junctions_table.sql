DROP TABLE IF EXISTS junctions;
DROP INDEX IF EXISTS junctions_gindx;

CREATE TABLE IF NOT EXISTS junctions (
		osmid INT PRIMARY KEY,
		ncollisions INT NULL,
		bikeCas INT NULL,
		totalCas INT NULL,
		totalVeh INT NULL,
		region VARCHAR(255) NULL,
		geotext GEOMETRY NOT NULL
    );

COPY junctions FROM '/home/malcolm/junctions.csv'  csv HEADER;

ALTER TABLE junctions OWNER to cyipt;

CREATE INDEX junctions_gindx ON junctions USING GIST (geotext);
