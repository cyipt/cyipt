CREATE TABLE IF NOT EXISTS Hazards (
		code INT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY Hazards FROM '/home/malcolm/stats19/acc/Hazards.csv'  csv HEADER;

ALTER TABLE Hazards OWNER to cyipt;
