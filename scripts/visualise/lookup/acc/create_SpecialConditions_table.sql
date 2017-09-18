CREATE TABLE IF NOT EXISTS SpecialConditions (
		code INT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );


COPY SpecialConditions FROM '/home/malcolm/stats19/acc/SpecialConditions.csv'  csv HEADER;

ALTER TABLE SpecialConditions OWNER to cyipt;
