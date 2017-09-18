CREATE TABLE IF NOT EXISTS Severity (
		code INT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );


COPY Severity FROM '/home/malcolm/stats19/acc/Severity.csv'  csv HEADER;

ALTER TABLE Severity OWNER to cyipt;
