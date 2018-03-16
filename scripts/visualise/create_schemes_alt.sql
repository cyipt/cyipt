DROP TABLE alt_schemes;
DROP INDEX IF EXISTS alt_schemes_gindx;

CREATE TABLE IF NOT EXISTS alt_schemes (
      idGlobal INT PRIMARY KEY,
      groupid INT NOT NULL,
      region VARCHAR(255) NULL,
      cost REAL NULL,
      costperperson REAL NULL,
      ncyclebefore INT NULL,
      ncycleafter INT NULL,
      infratype VARCHAR(255) NULL,
      change INT NULL,
      per REAL NULL,
      length REAL NULL,
      ndrivebefore INT NULL,
      ndriveafter INT NULL,
      carkmbefore REAL NULL,
      carkmafter REAL NULL,
      carkm REAL NULL,
      absenteeismbenefit REAL NULL,
      healthdeathavoided REAL NULL,
      healthbenefit REAL NULL,
      qualitybenefit REAL NULL,
      accidentsbenefit REAL NULL,
      co2saved REAL NULL,
      ghgbenefit REAL NULL,
      congestionbenefit REAL NULL,
      totalBen REAL NULL,
      costBenRatio REAL NULL,
      geotext GEOMETRY NOT NULL
    );

COPY alt_schemes FROM '/home/malcolm/schemes.csv'  csv HEADER;

ALTER TABLE alt_schemes OWNER to cyipt;

CREATE INDEX alt_schemes_gindx ON alt_schemes USING GIST (geotext);