CREATE TABLE IF NOT EXISTS Light (
		code INT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY Light FROM '/home/malcolm/stats19/acc/Light.csv'  csv HEADER;

ALTER TABLE Light OWNER to cyipt;
