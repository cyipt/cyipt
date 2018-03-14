DROP TABLE schemes;
DROP INDEX IF EXISTS schemes_gindx;

CREATE TABLE IF NOT EXISTS schemes (
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
      absenteeismbenefit DOUBLE NULL,
      healthdeathavoided DOUBLE NULL,
      healthbenefit DOUBLE NULL,
      qualitybenefit DOUBLE NULL,
      accidentsbenefit DOUBLE NULL,
      co2saved DOUBLE NULL,
      ghgbenefit DOUBLE NULL,
      congestionbenefit DOUBLE NULL,
      totalBen DOUBLE NULL,
      costBenRatio REAL NULL,
      geotext GEOMETRY NOT NULL
    );

COPY schemes FROM '/home/malcolm/schemes.csv'  csv HEADER;

ALTER TABLE schemes OWNER to cyipt;

CREATE INDEX schemes_gindx ON schemes USING GIST (geotext);