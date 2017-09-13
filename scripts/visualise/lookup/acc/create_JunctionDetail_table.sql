CREATE TABLE IF NOT EXISTS JunctionDetail (
		code INT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY JunctionDetail FROM '/home/malcolm/stats19/acc/JunctionDetail.csv'  csv HEADER;

ALTER TABLE JunctionDetail OWNER to cyipt;
