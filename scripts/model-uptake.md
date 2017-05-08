Modelling Cycling Update
================
Robin Lovelace
8 May 2017

This document reports on methods and preliminary findings associated with the modelling of cycling uptake associated with infrastructure.

Input data
----------

The input data comes from 2 main sources, which eventually will work for any region, as specified by the region variable and selected from an appropriate data source:

``` r
region = "avon"
data_source = "https://github.com/npct/pct-data/raw/master/"
```

-   Outputs from the PCT project, which reports current cycling levels and estimated 'fastest routes' for cyclists:

``` r
l = readRDS("l.Rds")
rf = readRDS("rf.Rds")
rq = readRDS("rq.Rds")
```

-   Outputs from the data processing stage of the CyIPT project, which provides data on the current road network from the perspective of cycling.

``` r
l = readRDS("../../example-data/bristol/osm-lines-quietness-full.Rds")
```

These can be loaded as follows:
