CREATE TABLE IF NOT EXISTS RoadClass2 (
		code INT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY RoadClass2 FROM '/home/malcolm/stats19/acc/RoadClass2.csv'  csv HEADER;

ALTER TABLE RoadClass2 OWNER to cyipt;
