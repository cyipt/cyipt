CREATE TABLE IF NOT EXISTS RoadClass1 (
		code INT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY RoadClass1 FROM '/home/malcolm/stats19/acc/RoadClass1.csv'  csv HEADER;

ALTER TABLE RoadClass1 OWNER to cyipt;
