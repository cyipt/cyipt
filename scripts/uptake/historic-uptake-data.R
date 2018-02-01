# Aim: create predictive model of uptake of cycling following exposure to infrastructure
devtools::install_github("robinlovelace/ukboundaries")
library(tmap)
tmap_mode("view")
library(ukboundaries)
library(tidyverse)
library(stplanr)
library(osmdata)
library(sf)
region_name = "Bristol"
