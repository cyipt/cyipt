CREATE TABLE IF NOT EXISTS JunctionControl (
		code INT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY JunctionControl FROM '/home/malcolm/stats19/acc/JunctionControl.csv'  csv HEADER;

ALTER TABLE JunctionControl OWNER to cyipt;
