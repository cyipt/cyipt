CREATE TABLE IF NOT EXISTS Surface (
		code INT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY Surface FROM '/home/malcolm/stats19/acc/Surface.csv'  csv HEADER;

ALTER TABLE Surface OWNER to cyipt;
