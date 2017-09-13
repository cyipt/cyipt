CREATE TABLE IF NOT EXISTS Weather (
		code INT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY Weather FROM '/home/malcolm/stats19/acc/Weather.csv'  csv HEADER;

ALTER TABLE Weather OWNER to cyipt;
