CREATE TABLE IF NOT EXISTS RoadType (
		code INT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY RoadType FROM '/home/malcolm/stats19/acc/RoadType.csv'  csv HEADER;

ALTER TABLE RoadType OWNER to cyipt;
