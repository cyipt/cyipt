CREATE TABLE IF NOT EXISTS CrossingFacilities (
		code INT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY CrossingFacilities FROM '/home/malcolm/stats19/acc/CrossingFacilities.csv'  csv HEADER;

ALTER TABLE CrossingFacilities OWNER to cyipt;
