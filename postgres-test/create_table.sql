CREATE TABLE IF NOT EXISTS bristol (
		id INT PRIMARY KEY,
		osmid INT NOT NULL,
		name VARCHAR(255) NULL,
		highway VARCHAR(255) NULL,
		junction VARCHAR(255) NULL,
		roadtype VARCHAR(255) NULL,
		onewaysummary VARCHAR(255) NULL,
		elevation VARCHAR(255) NULL,
		maxspeed SMALLINT NULL,
		segregated VARCHAR(255) NULL,
		sidewalk VARCHAR(255) NULL,
		cyclewayleft VARCHAR(255) NULL,
		lanespsvforward SMALLINT NULL,
		lanesforward SMALLINT NULL,
		lanesbackward SMALLINT NULL,
		lanespsvbackward SMALLINT NULL,
		cyclewayright VARCHAR(255) NULL,
		pctcensus SMALLINT NULL,
		pctgov INT NULL,
		pctgen INT NULL,
		pctdutch INT NULL,
		pctebike INT NULL,
		geotext GEOMETRY NOT NULL
    );