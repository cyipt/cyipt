CREATE TABLE IF NOT EXISTS CrossingControl (
		code INT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY CrossingControl FROM '/home/malcolm/stats19/acc/CrossingControl.csv'  csv HEADER;

ALTER TABLE CrossingControl OWNER to cyipt;
