library(tidyverse)
library(sf)

ml1 = readRDS("N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyipt-securedata/uptakemodel/ml1.Rds")
min_cycle = 0
min_all = 20

# load data - see uptake_2001_2011_results.R for preprocessing steps ----
rf_sp <- readRDS("N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyoddata/rf.Rds")
rf_sf <- st_as_sf(rf_sp)
rf <- as.data.frame(rf_sf)
rf <- rf[,c("id","rf_avslope_perc","rf_time_min","e_dist_km")]

head(rf)
routes_orig <- readRDS("../cyipt-securedata/uptakemodel/route_infra_final2.Rds")
routes_orig$all01[is.na(routes_orig$all01)] <- 0
routes_orig$bicycle01[is.na(routes_orig$bicycle01)] <- 0
routes_orig$percycle01 <- routes_orig$bicycle01 / routes_orig$all01
routes_orig$percycle11 <- routes_orig$bicycle11 / routes_orig$all11
routes_orig$changecommuters <- (routes_orig$all11 - routes_orig$all01) / routes_orig$all01
routes_orig$Puptake <- routes_orig$percycle11 - routes_orig$percycle01
routes_orig[is.na(routes_orig)] <- 0

routes <- left_join(routes_orig, rf, by = c("id" = "id")) # add wpz and other vars to routes
routes = sample_frac(routes, 0.01) # for testing

# total length of changed infra:
# Description: all non-negative 'good' changes in infrastructure - routes_infra_length
routes_infra_change = routes %>%
  select(matches("C", ignore.case = F)) %>%
  select(-contains("N", ignore.case = F))
names(routes_infra_change)
summary(routes_infra_change)
routes_infra_change_pos = mutate_all(routes_infra_change, .funs = funs(ifelse(. < 0, 0, .)))
summary(routes_infra_change_pos)
routes_infraC = rowSums(routes_infra_change_pos)
summary(routes_infraC)
routes$routes_infra_length = routes_infraC

# find percentage on roads with different speeds:
# description: the % of routes on any road type with different speeds
# 20- mph
routes_pspeeds20 = routes %>%
  select(matches("20")) %>%
  select(contains("P"), -contains("01"))
summary(routes_pspeeds20)
routes_pspeed20 = rowSums(routes_pspeeds20)
summary(routes_pspeed20)
routes$routes_pspeed20 = routes_pspeed20
# 30 mph
routes_pspeeds30 = routes %>%
  select(matches("30")) %>%
  select(contains("P"), -contains("01"))
summary(routes_pspeeds30)
routes_pspeed30 = rowSums(routes_pspeeds30)
summary(routes_pspeed30)
routes$routes_pspeed30 = routes_pspeed30
# 40+ mph
routes_pspeeds40 = routes %>%
  select(matches("40")) %>%
  select(contains("P"), -contains("01"))
summary(routes_pspeeds40)
routes_pspeed40 = rowSums(routes_pspeeds40)
summary(routes_pspeed40)
routes$routes_pspeed40 = routes_pspeed40

# estimate uptake with model
p = predict(ml1, routes)
summary(p)

# add distance decay ----
distance = routes$length / 1000
logit_pcycle = -3.894 + (-0.5872 * distance) + (1.832 * sqrt(distance) ) + (0.007956 * distance^2)
pcycle_pct = boot::inv.logit(logit_pcycle)
mean(pcycle_pct)
weighted.mean(pcycle_pct, p)
p_pcycle = p * pcycle_pct
pcycle_normalised = pcycle_pct / (mean(p_pcycle) / mean(p))
pcycle_normalised_truncated = pcycle_normalised
pcycle_normalised_truncated[pcycle_normalised_truncated > 1] = 1
mean(p)
mean(pcycle_normalised)
mean(pcycle_normalised_truncated)
dd_linear = function(d, dd_start = 3, dd_end = 20) {
  dd_range = dd_end - dd_start
  d_minus_dd_start = d - dd_start
  dd = 1 - d_minus_dd_start / dd_range
  dd[dd > 1] = 1
  dd[dd < 0] = 0
  dd
}
pcycle_linear = dd_linear(distance, dd_start = 3, dd_end = 20)
plot(distance, pcycle_normalised, ylim = c(0, 3))
points(distance, pcycle_normalised_truncated, col = "blue")
points(distance, pcycle_linear, col = "green")
p_distance_supressed = p * pcycle_linear
mean(p_distance_supressed)
mean(p)

# testing ----
n = 50
routes.infra_test = select_if(routes, is.numeric) %>% summarise_all(mean)
routes.infra_test = routes.infra_test[rep(1, n), ]
routes.infra_test$routes_infra_length = (1:n) * 100
routes.infra_test <- routes.infra_test[,c("routes_infra_length","rf_avslope_perc","Fcycleway","routes_pspeed20","routes_pspeed30","routes_pspeed40")]
p = predict(ml1, routes.infra_test)
plot(routes.infra_test$routes_infra_length, p)
summary(ml1)
