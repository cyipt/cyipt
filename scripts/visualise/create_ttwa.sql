DROP TABLE IF EXISTS ttwa;
DROP INDEX IF EXISTS ttwa_gindx;

CREATE TABLE IF NOT EXISTS ttwa (
		id INT PRIMARY KEY,
		code VARCHAR(9) NOT NULL,
		name VARCHAR(35) NOT NULL,
		geotext GEOMETRY NOT NULL
    );

COPY ttwa FROM '/home/malcolm/ttwa.csv'  csv HEADER;

ALTER TABLE ttwa OWNER to cyipt;

CREATE INDEX ttwa_gindx ON ttwa USING GIST (geotext);
