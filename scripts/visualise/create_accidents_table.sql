DROP TABLE IF EXISTS accidents;
DROP INDEX IF EXISTS accidents_gindx;

CREATE TABLE accidents (
		AccRefGlobal INT PRIMARY KEY,
    DateTime TIMESTAMP,
    Severity SMALLINT NULL,
    nVehicles SMALLINT NULL,
    nCasualties SMALLINT NULL,
    RoadClass1 SMALLINT NULL,
    RoadNumber1 INT NULL,
    RoadType SMALLINT NULL,
    SpeedLimit SMALLINT NULL,
    JunctionDetail SMALLINT NULL,
    JunctionControl SMALLINT NULL,
    RoadClass2 SMALLINT NULL,
    RoadNumber2 INT NULL,
    CrossingControl SMALLINT NULL,
    CrossingFacilities SMALLINT NULL,
    Light SMALLINT NULL,
    Weather SMALLINT NULL,
    Surface SMALLINT NULL,
    SpecialConditions SMALLINT NULL,
    Hazards SMALLINT NULL,
    CollisionJunc INT NULL,
    CollisionLine INT NULL,
    geotext  GEOMETRY NULL
    );

COPY accidents FROM '/home/malcolm/accidents.csv'  csv HEADER;

ALTER TABLE accidents OWNER to cyipt;

CREATE INDEX accidents_gindx ON accidents USING GIST (geotext);



DROP TABLE IF EXISTS casualties;

CREATE TABLE casualties (
		AccRefGlobal INT NOT NULL,
VehicleRef  SMALLINT NULL,
CasualtyRef  SMALLINT NULL,
CasualtyClass  SMALLINT NULL,
CasSex  SMALLINT NULL,
Age  SMALLINT NULL,
PedestrianMovement  SMALLINT NULL,
PedestrianDirection SMALLINT NULL,
SchoolPupil  SMALLINT NULL,
SeatBelt  SMALLINT NULL,
CarPassenger  SMALLINT NULL,
BusPassenger  SMALLINT NULL,
CasualtyType  SMALLINT NULL,
MaintenanceWorker SMALLINT NULL,
HomeArea SMALLINT NULL,
CasualtyIMD SMALLINT NULL
   );

COPY casualties FROM '/home/malcolm/casualties.csv'  csv HEADER;

ALTER TABLE casualties OWNER to cyipt;


DROP TABLE IF EXISTS vehicles;

CREATE TABLE vehicles (
		AccRefGlobal INT NOT NULL,
VehicleRef SMALLINT NULL,
VehicleType SMALLINT NULL,
TowingArticulation SMALLINT NULL,
Manoeuvre SMALLINT NULL,
VehFrom SMALLINT NULL,
VehTo  SMALLINT NULL,
LocationRoad SMALLINT NULL,
LocationRestrictedAway SMALLINT NULL,
Junction SMALLINT NULL,
SkiddingOverturning SMALLINT NULL,
ObjectInCarriageway SMALLINT NULL,
LeavingCarriageway SMALLINT NULL,
ObjectOffCarriageway SMALLINT NULL,
VehicleLetter SMALLINT NULL,
PointofImpact SMALLINT NULL,
OtherVehicle SMALLINT NULL,
CombinedDamage SMALLINT NULL,
RoofUndersideDamage SMALLINT NULL,
SexDriver SMALLINT NULL,
AgeDriver SMALLINT NULL,
VehAgeBand SMALLINT NULL,
HitRun SMALLINT NULL,
ForeignVehicle SMALLINT NULL,
LeftHandDrive SMALLINT NULL,
EngineSize SMALLINT NULL,
Propulsion INT NULL,
AgeVehicle INT NULL,
DriverIMD INT NULL,
DriverArea INT NULL,
VehicleIMD INT NULL,
JourneyPurpose INT NULL
   );

COPY vehicles FROM '/home/malcolm/vehicles.csv'  csv HEADER;

ALTER TABLE vehicles OWNER to cyipt;


DROP TABLE IF EXISTS veh_CombinedDamage ;

CREATE TABLE veh_CombinedDamage (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_CombinedDamage FROM '/home/malcolm/lookup/veh_CombinedDamage.csv'  csv HEADER;

ALTER TABLE veh_CombinedDamage OWNER to cyipt;


DROP TABLE IF EXISTS veh_ForeignVehicle ;

CREATE TABLE veh_ForeignVehicle (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_ForeignVehicle FROM '/home/malcolm/lookup/veh_ForeignVehicle.csv'  csv HEADER;

ALTER TABLE veh_ForeignVehicle OWNER to cyipt;


DROP TABLE IF EXISTS veh_VehFrom ;

CREATE TABLE veh_VehFrom (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_VehFrom FROM '/home/malcolm/lookup/veh_VehFrom.csv'  csv HEADER;

ALTER TABLE veh_VehFrom OWNER to cyipt;


DROP TABLE IF EXISTS veh_HitRun ;

CREATE TABLE veh_HitRun (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_HitRun FROM '/home/malcolm/lookup/veh_HitRun.csv'  csv HEADER;

ALTER TABLE veh_HitRun OWNER to cyipt;


DROP TABLE IF EXISTS veh_JourneyPurpose ;

CREATE TABLE veh_JourneyPurpose (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_JourneyPurpose FROM '/home/malcolm/lookup/veh_JourneyPurpose.csv'  csv HEADER;

ALTER TABLE veh_JourneyPurpose OWNER to cyipt;



DROP TABLE IF EXISTS veh_Junction ;

CREATE TABLE veh_Junction (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_Junction FROM '/home/malcolm/lookup/veh_Junction.csv'  csv HEADER;

ALTER TABLE veh_Junction OWNER to cyipt;


DROP TABLE IF EXISTS veh_LeavingCarriageway ;

CREATE TABLE veh_LeavingCarriageway (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_LeavingCarriageway FROM '/home/malcolm/lookup/veh_LeavingCarriageway.csv'  csv HEADER;

ALTER TABLE veh_LeavingCarriageway OWNER to cyipt;


DROP TABLE IF EXISTS veh_LeftHandDrive ;

CREATE TABLE veh_LeftHandDrive (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_LeftHandDrive FROM '/home/malcolm/lookup/veh_LeftHandDrive.csv'  csv HEADER;

ALTER TABLE veh_LeftHandDrive OWNER to cyipt;


DROP TABLE IF EXISTS veh_LocationRestrictedAway ;

CREATE TABLE veh_LocationRestrictedAway (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_LocationRestrictedAway FROM '/home/malcolm/lookup/veh_LocationRestrictedAway.csv'  csv HEADER;

ALTER TABLE veh_LocationRestrictedAway OWNER to cyipt;


DROP TABLE IF EXISTS veh_LocationRoad ;

CREATE TABLE veh_LocationRoad (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_LocationRoad FROM '/home/malcolm/lookup/veh_LocationRoad.csv'  csv HEADER;

ALTER TABLE veh_LocationRoad OWNER to cyipt;



DROP TABLE IF EXISTS veh_Manoeuvre ;

CREATE TABLE veh_Manoeuvre (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_Manoeuvre FROM '/home/malcolm/lookup/veh_Manoeuvre.csv'  csv HEADER;

ALTER TABLE veh_Manoeuvre OWNER to cyipt;


DROP TABLE IF EXISTS veh_ObjectInCarriageway;

CREATE TABLE veh_ObjectInCarriageway (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_ObjectInCarriageway FROM '/home/malcolm/lookup/veh_ObjectInCarriageway.csv'  csv HEADER;

ALTER TABLE veh_ObjectInCarriageway OWNER to cyipt;



DROP TABLE IF EXISTS veh_ObjectOffCarriageway;

CREATE TABLE veh_ObjectOffCarriageway (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_ObjectOffCarriageway FROM '/home/malcolm/lookup/veh_ObjectOffCarriageway.csv'  csv HEADER;

ALTER TABLE veh_ObjectOffCarriageway OWNER to cyipt;


DROP TABLE IF EXISTS veh_PointofImpact ;

CREATE TABLE veh_PointofImpact (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_PointofImpact FROM '/home/malcolm/lookup/veh_PointofImpact.csv'  csv HEADER;

ALTER TABLE veh_PointofImpact OWNER to cyipt;



DROP TABLE IF EXISTS veh_Propulsion;

CREATE TABLE veh_Propulsion (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_Propulsion FROM '/home/malcolm/lookup/veh_Propulsion.csv'  csv HEADER;

ALTER TABLE veh_Propulsion OWNER to cyipt;



DROP TABLE IF EXISTS veh_RoofUndersideDamage;

CREATE TABLE veh_RoofUndersideDamage (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_RoofUndersideDamage FROM '/home/malcolm/lookup/veh_RoofUndersideDamage.csv'  csv HEADER;

ALTER TABLE veh_RoofUndersideDamage OWNER to cyipt;


DROP TABLE IF EXISTS veh_SexDriver;

CREATE TABLE veh_SexDriver (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_SexDriver FROM '/home/malcolm/lookup/veh_SexDriver.csv'  csv HEADER;

ALTER TABLE veh_SexDriver OWNER to cyipt;


DROP TABLE IF EXISTS veh_SkiddingOverturning ;

CREATE TABLE veh_SkiddingOverturning (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_SkiddingOverturning FROM '/home/malcolm/lookup/veh_SkiddingOverturning.csv'  csv HEADER;

ALTER TABLE veh_SkiddingOverturning OWNER to cyipt;


DROP TABLE IF EXISTS veh_VehTo ;

CREATE TABLE veh_VehTo (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_VehTo FROM '/home/malcolm/lookup/veh_VehTo.csv'  csv HEADER;

ALTER TABLE veh_To OWNER to cyipt;



DROP TABLE IF EXISTS veh_TowingArticulation ;

CREATE TABLE veh_TowingArticulation (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_TowingArticulation FROM '/home/malcolm/lookup/veh_TowingArticulation.csv'  csv HEADER;

ALTER TABLE veh_TowingArticulation OWNER to cyipt;


DROP TABLE IF EXISTS veh_VehAgeBand ;

CREATE TABLE veh_VehAgeBand (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_VehAgeBand FROM '/home/malcolm/lookup/veh_VehAgeBand.csv'  csv HEADER;

ALTER TABLE veh_VehAgeBand OWNER to cyipt;


DROP TABLE IF EXISTS veh_VehicleLetter;

CREATE TABLE veh_VehicleLetter (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_VehicleLetter FROM '/home/malcolm/lookup/veh_VehicleLetter.csv'  csv HEADER;

ALTER TABLE veh_VehicleLetter OWNER to cyipt;



DROP TABLE IF EXISTS veh_VehicleType ;

CREATE TABLE veh_VehicleType (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY veh_VehicleType FROM '/home/malcolm/lookup/veh_VehicleType.csv'  csv HEADER;

ALTER TABLE veh_VehicleType OWNER to cyipt;





DROP TABLE IF EXISTS acc_CrossingControl;

CREATE TABLE acc_CrossingControl (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY acc_CrossingControl FROM '/home/malcolm/lookup/acc_CrossingControl.csv'  csv HEADER;

ALTER TABLE acc_CrossingControl OWNER to cyipt;

DROP TABLE IF EXISTS acc_CrossingFacilities;

CREATE TABLE acc_CrossingFacilities (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY acc_CrossingFacilities FROM '/home/malcolm/lookup/acc_CrossingFacilities.csv'  csv HEADER;

ALTER TABLE acc_CrossingFacilities OWNER to cyipt;

DROP TABLE IF EXISTS acc_Hazards;

CREATE TABLE acc_Hazards (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY acc_Hazards FROM '/home/malcolm/lookup/acc_Hazards.csv'  csv HEADER;

ALTER TABLE acc_Hazards OWNER to cyipt;

DROP TABLE IF EXISTS acc_JunctionControl;

CREATE TABLE acc_JunctionControl (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY acc_JunctionControl FROM '/home/malcolm/lookup/acc_JunctionControl.csv'  csv HEADER;

ALTER TABLE acc_JunctionControl OWNER to cyipt;

DROP TABLE IF EXISTS acc_JunctionDetail;

CREATE TABLE acc_JunctionDetail (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY acc_JunctionDetail FROM '/home/malcolm/lookup/acc_JunctionDetail.csv'  csv HEADER;

ALTER TABLE acc_JunctionDetail OWNER to cyipt;

DROP TABLE IF EXISTS acc_Light;

CREATE TABLE acc_Light (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY acc_Light FROM '/home/malcolm/lookup/acc_Light.csv'  csv HEADER;

ALTER TABLE acc_Light OWNER to cyipt;

DROP TABLE IF EXISTS acc_RoadClass1;

CREATE TABLE acc_RoadClass1 (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY acc_RoadClass1 FROM '/home/malcolm/lookup/acc_RoadClass1.csv'  csv HEADER;

ALTER TABLE acc_RoadClass1 OWNER to cyipt;

DROP TABLE IF EXISTS acc_RoadClass2;

CREATE TABLE acc_RoadClass2 (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY acc_RoadClass2 FROM '/home/malcolm/lookup/acc_RoadClass2.csv'  csv HEADER;

ALTER TABLE acc_RoadClass2 OWNER to cyipt;

DROP TABLE IF EXISTS acc_RoadType;

CREATE TABLE acc_RoadType (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY acc_RoadType FROM '/home/malcolm/lookup/acc_RoadType.csv'  csv HEADER;

ALTER TABLE acc_RoadType OWNER to cyipt;

DROP TABLE IF EXISTS acc_Severity;

CREATE TABLE acc_Severity (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY acc_Severity FROM '/home/malcolm/lookup/acc_Severity.csv'  csv HEADER;

ALTER TABLE acc_Severity OWNER to cyipt;

DROP TABLE IF EXISTS acc_SpecialConditions;

CREATE TABLE acc_SpecialConditions (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY acc_SpecialConditions FROM '/home/malcolm/lookup/acc_SpecialConditions.csv'  csv HEADER;

ALTER TABLE acc_SpecialConditions OWNER to cyipt;

DROP TABLE IF EXISTS acc_Surface;

CREATE TABLE acc_Surface (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY acc_Surface FROM '/home/malcolm/lookup/acc_Surface.csv'  csv HEADER;

ALTER TABLE acc_Surface OWNER to cyipt;

DROP TABLE IF EXISTS acc_Weather;

CREATE TABLE acc_Weather (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY acc_Weather FROM '/home/malcolm/lookup/acc_Weather.csv'  csv HEADER;

ALTER TABLE acc_Weather OWNER to cyipt;



DROP TABLE IF EXISTS cas_BusPassenger;

CREATE TABLE cas_BusPassenger(
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY cas_BusPassenger FROM '/home/malcolm/lookup/cas_BusPassenger.csv'  csv HEADER;

ALTER TABLE cas_BusPassenger OWNER to cyipt;


DROP TABLE IF EXISTS cas_CarPassenger ;

CREATE TABLE cas_CarPassenger (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY cas_CarPassenger FROM '/home/malcolm/lookup/cas_CarPassenger.csv'  csv HEADER;

ALTER TABLE cas_CarPassenger OWNER to cyipt;


DROP TABLE IF EXISTS cas_CasSex ;

CREATE TABLE cas_CasSex (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY cas_CasSex FROM '/home/malcolm/lookup/cas_CasSex.csv'  csv HEADER;

ALTER TABLE cas_CasSex OWNER to cyipt;


DROP TABLE IF EXISTS cas_CasualtyClass ;

CREATE TABLE cas_CasualtyClass (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY cas_CasualtyClass FROM '/home/malcolm/lookup/cas_CasualtyClass.csv'  csv HEADER;

ALTER TABLE cas_CasualtyClass OWNER to cyipt;


DROP TABLE IF EXISTS cas_CasualtyIMD ;

CREATE TABLE cas_CasualtyIMD (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY cas_CasualtyIMD FROM '/home/malcolm/lookup/cas_CasualtyIMD.csv'  csv HEADER;

ALTER TABLE cas_CasualtyIMD OWNER to cyipt;


DROP TABLE IF EXISTS cas_CasualtyType ;

CREATE TABLE cas_CasualtyType (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY cas_CasualtyType FROM '/home/malcolm/lookup/cas_CasualtyType.csv'  csv HEADER;

ALTER TABLE cas_CasualtyType OWNER to cyipt;



DROP TABLE IF EXISTS cas_HomeArea ;

CREATE TABLE cas_HomeArea (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY cas_HomeArea FROM '/home/malcolm/lookup/cas_HomeArea.csv'  csv HEADER;

ALTER TABLE cas_HomeArea OWNER to cyipt;


DROP TABLE IF EXISTS cas_MaintenanceWorker ;

CREATE TABLE cas_MaintenanceWorker (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY cas_MaintenanceWorker FROM '/home/malcolm/lookup/cas_MaintenanceWorker.csv'  csv HEADER;

ALTER TABLE cas_MaintenanceWorker OWNER to cyipt;



DROP TABLE IF EXISTS cas_PedestrianDirection ;

CREATE TABLE cas_PedestrianDirection (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY cas_PedestrianDirection FROM '/home/malcolm/lookup/cas_PedestrianDirection.csv'  csv HEADER;

ALTER TABLE cas_PedestrianDirection OWNER to cyipt;


DROP TABLE IF EXISTS cas_PedestrianMovement;

CREATE TABLE cas_PedestrianMovement (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY cas_PedestrianMovement FROM '/home/malcolm/lookup/cas_PedestrianMovement.csv'  csv HEADER;

ALTER TABLE cas_PedestrianMovement OWNER to cyipt;


DROP TABLE IF EXISTS cas_SchoolPupil ;

CREATE TABLE cas_SchoolPupil (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY cas_SchoolPupil FROM '/home/malcolm/lookup/cas_SchoolPupil.csv'  csv HEADER;

ALTER TABLE cas_SchoolPupil OWNER to cyipt;

DROP TABLE IF EXISTS cas_SeatBelt ;

CREATE TABLE cas_SeatBelt (
		code SMALLINT PRIMARY KEY,
		label VARCHAR(255) NOT NULL
    );

COPY cas_SeatBelt FROM '/home/malcolm/lookup/cas_SeatBelt.csv'  csv HEADER;

ALTER TABLE cas_SeatBelt OWNER to cyipt;




