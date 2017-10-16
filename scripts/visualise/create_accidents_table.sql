DROP TABLE IF EXISTS accidents;
DROP INDEX IF EXISTS accidents_gindx;

CREATE TABLE IF NOT EXISTS accidents (
		AccRefGlobal INT PRIMARY KEY,
    DateTime TIMESTAMP,
    Severity INT NULL,
    nVehicles INT NULL,
    nCasualties INT NULL,
    RoadClass1 INT NULL,
    RoadNumber1 INT NULL,
    RoadType INT NULL,
    SpeedLimit INT NULL,
    JunctionDetail INT NULL,
    JunctionControl INT NULL,
    RoadClass2 INT NULL,
    RoadNumber2 INT NULL,
    CrossingControl INT NULL,
    CrossingFacilities INT NULL,
    Light INT NULL,
    Weather INT NULL,
    Surface INT NULL,
    SpecialConditions INT NULL,
    Hazards INT NULL,
    geotext  GEOMETRY NULL
    );

COPY accidents FROM '/home/malcolm/accidents.csv'  csv HEADER;

ALTER TABLE accidents OWNER to cyipt;

CREATE INDEX accidents_gindx ON accidents USING GIST (geotext);
