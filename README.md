
<!-- README.md is generated from README.Rmd. Please edit that file -->
Project management and overview documents for the Cycling Infrastructure Prioritisation Toolkit (CyIPT) Funded by the Department for Transport.

Comments/suggestions welcome in the [issue tracker](https://github.com/cyipt/cyipt/issues).

Definitions and variable names
------------------------------

Variable names are in lowercase and use\_underscores, and should be kept updated in [input-data/variables.csv](https://github.com/cyipt/cyipt/blob/master/input-data/variables.csv):

``` r
variables = readr::read_csv("input-data/variables.csv")
knitr::kable(variables)
```

| Variable name      | Definition                                                           |
|:-------------------|:---------------------------------------------------------------------|
| pcu                | Passenger Car Units per hour                                         |
| aadf               | Estimated annual average daily flows (disagregated by vehicle types) |
| aadt               | Estimated annual average traffic (vehicles)                          |
| all                | Total no. commuters                                                  |
| bicycle            | No. cyclists in Census 2011                                          |
| govtarget\_slc     | No. cyclists in Government Target                                    |
| dutch\_slc         | No. cyclists in Go Dutch                                             |
| msoa1              | Start or end zone 1, zone code                                       |
| msoa2              | Start or end zone 2, zone code                                       |
| car\_driver        | No. car drivers in Census 2011                                       |
| dist               | Straight line distance (km)                                          |
| dist\_fast         | Fast route distance (km)                                             |
| dist\_quiet        | Quiet route distance (km)                                            |
| cirquity           | Cirquity (fast distance / straight line distance)                    |
| distq\_f           | Quiet distance / fast distance                                       |
| av\_incline\_fast  | Average incline of fastest route                                     |
| av\_incline\_quiet | Average incline of quietest route                                    |
| time\_fast         | Time of fast route (s)                                               |
| time\_quiet        | Time of quiet route (s)                                              |

``` r
library(sf)
region = st_read("areas/bristol-poly.geojson")
#> Reading layer `OGRGeoJSON' from data source `/home/robin/cyipt/cyipt/areas/bristol-poly.geojson' using driver `GeoJSON'
#> converted into: MULTIPOLYGON
#> Simple feature collection with 1 feature and 21 fields
#> geometry type:  MULTIPOLYGON
#> dimension:      XY
#> bbox:           xmin: -2.773873 ymin: 51.39755 xmax: -2.510999 ymax: 51.54443
#> epsg (SRID):    4326
#> proj4string:    +proj=longlat +datum=WGS84 +no_defs
```
