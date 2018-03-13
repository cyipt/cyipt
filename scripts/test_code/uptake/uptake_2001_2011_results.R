library(xgboost)
library(tidyverse)
library(sf)
min_cycle = 0
min_all = 20

# load and preprocess data ----
rf_sp <- readRDS("N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyoddata/rf.Rds")
rf_sf <- st_as_sf(rf_sp)
rf_cents = st_centroid(rf_sf)
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

routes <- left_join(routes_orig, rf, by = c("id" = "id"))



# get region data
wpz_orig = readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
wpz = select(wpz_orig, ttwa11nm)
rf_cents = st_join(rf_cents, wpz)
head(rf_cents)

# total length of changed infra:
# Description: all non-negative 'good' changes in infrastructure - routes_infra_length
routes_infra_change = routes %>%
  select(matches("C", ignore.case = F)) %>%
  select(-contains("N", ignore.case = F))
summary(routes_infra_change)
routes_infra_change_pos = mutate_all(routes_infra_change, .funs = funs(ifelse(. < 0, 0, .)))
summary(routes_infra_change_pos)
routes_infraC = rowSums(routes_infra_change_pos)
summary(routes_infraC)
routes$routes_infra_length = routes_infraC
cor(routes$Puptake, routes$routes_infra_length) # negative correlation (unexpected)

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

# find percentage on change with different speeds:
# description: the % change in routes on any road type with different speed lims
# 20- mph
routes_fspeeds20 = routes %>%
  select(matches("20")) %>%
  select(contains("F"), -contains("01"))
summary(routes_fspeeds20)
routes_fspeed20 = rowSums(routes_fspeeds20)
summary(routes_fspeed20)
routes$routes_fspeed20 = routes_fspeed20
# 30 mph
routes_fspeeds30 = routes %>%
  select(matches("30")) %>%
  select(contains("F"), -contains("01"))
summary(routes_fspeeds30)
routes_fspeed30 = rowSums(routes_fspeeds30)
summary(routes_fspeed30)
routes$routes_fspeed30 = routes_fspeed30
# 40+ mph
routes_fspeeds40 = routes %>%
  select(matches("40")) %>%
  select(contains("F"), -contains("01"))
summary(routes_fspeeds40)
routes_fspeed40 = rowSums(routes_fspeeds40)
summary(routes_fspeed40)
routes$routes_fspeed40 = routes_fspeed40

# sanity test of line E02000274 - E02000524
rf_test = filter(rf_sf, id == "E02000274 E02000524")
mapview::mapview(rf_test) # passed

# check infra in routes with high levels of uptake ----
rf_sfj = inner_join(rf_sf, routes, by = "id")
rfu = rf_sfj %>% filter(Puptake > 0.1 & all11 >= 20 & all01 >= 20)
# mapview::mapview(rfu)
# routes with high levels of uptake have:
mean(rfu$routes_pspeed20) / mean(routes$routes_pspeed20) # more 20mph roads in high uptake routes (expected)
mean(rfu$routes_fspeed20) / mean(routes$routes_fspeed20) # less routes changed to 20 (unexpected)
mean(rfu$routes_pspeed30) / mean(routes$routes_pspeed30) # less with 30 (expected)
mean(rfu$routes_fspeed30) / mean(routes$routes_fspeed30) # less changed to 30 (expected)
mean(rfu$routes_pspeed40) / mean(routes$routes_pspeed40) # much lower % on 40mph roads (expected)
mean(rfu$routes_fspeed40) / mean(routes$routes_fspeed40) # less routes changed to 20 (expected)
mean(rfu$Pmain40_N11) / mean(routes$Pmain40_N11) # less routes on main roads areas with high uptake (expected)
mean(rfu$Fmain40_I) / mean(routes$Fmain40_I) # less fractional change in infra (unexpected)
mean(rfu$Pcycleway11) / mean(routes$Pcycleway11) # less routes on cycleways (unexpected)
mean(rfu$Fcycleway) / mean(routes$Fcycleway) # less new cycleways (unexpected)

# focus on speeds:
rfu_fspeeds = rfu %>%
  select(matches("20|30|40")) %>%
  select(contains("F"), -contains("01"))
names(rfu_fspeeds)
routes_pspeeds = routes %>%
  select(matches("20|30|40")) %>%
  select(contains("F"), -contains("01"))
names(routes_pspeeds)
colMeans(rfu_fspeeds %>% st_set_geometry(NULL)) / colMeans(routes_pspeeds)

routes.sub_vars = select(rfu, -Puptake, -id, -contains("bicycle"), -contains("percycle11"), -contains("all"),
               -contains("changecom"), -contains("P")) %>%
  st_set_geometry(NULL) %>%
  select(matches("F|length|av"))
train = routes.sub_vars %>%
  as.matrix()
colnames(train)
w = rfu$all11 + rfu$all01 / 2
mx1 = xgboost(data = train, label = rfu$Puptake, weight = w, nrounds = 10)
importance <- xgb.importance(model = mx1, feature_names = colnames(train))
xgb.plot.importance(importance , top_n = 10)

# run on full dataset ----
sel_mincycle = routes$bicycle01 >= min_cycle & routes$all01 >= min_all &
  routes$bicycle11 >= 5
summary(sel_mincycle)
routes.sub <- routes[sel_mincycle,]
routes.train <- sample_frac(routes.sub, 0.5)
routes.test <- routes.sub[!routes.sub$id %in% routes.train$id,]

# simple linear model
summary(routes.sub)
m1 = lm(Puptake ~ length + rf_avslope_perc + routes_pspeed20 + routes_pspeed30 + routes_pspeed40 + Fcycleway, data =  routes.sub)
m2 = lm(Puptake ~ length + rf_avslope_perc + routes_fspeed20 + routes_fspeed30 + routes_fspeed40 + Fcycleway, data =  routes.sub)
m3 = lm(Puptake ~ length + rf_avslope_perc + routes_infra_length : routes_pspeed20 +
          routes_infra_length : routes_pspeed30 + routes_infra_length : routes_pspeed40 + Fcycleway : Pcycleway11, data =  routes.sub)
m4 = lm(Puptake ~ length + rf_avslope_perc + routes_infra_length : routes_pspeed20 +
          routes_infra_length : routes_pspeed30 + routes_infra_length : routes_pspeed40 + Pcycleway11, data =  routes.sub)
# simple model of interaction between infra distance and uptake
m5 = lm(Puptake ~ length + rf_avslope_perc + routes_infra_length : routes_pspeed20
        + routes_infra_length : routes_pspeed30 +
        + routes_infra_length : routes_pspeed40, data =  routes.sub)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)

routes.sim =  routes.sub %>%
  mutate(routes_infra_length = routes_infra_length + 500,
         routes_pspeed20 = routes_pspeed20 + 0.3,
         routes_pspeed40 = routes_pspeed40 - 0.1
         )
puptake_sim = predict(m5, routes.sim)
mean(puptake_sim)
mean(routes.sub$Puptake) # ~ 1% increase in cycling...
# explore feature importance with xgboost
# routes.sub_vars = select(routes.sub, -Puptake, -id, -contains("bicycle"), -contains("percycle11"), -contains("all"),
#                -contains("changecom"), -contains("P"))
# % on busy
routes.sub_vars = select(routes.sub, contains("routes_"))
train = routes.sub_vars %>%
  as.matrix()
colnames(train)
w = routes.sub$all11 + routes.sub$all01 / 2
mx1 = xgboost(data = train, label = routes.sub$Puptake, weight = w, nrounds = 10)
mx2 = xgboost(data = train, label = routes.sub$Puptake, weight = w, nrounds = 10,
              params = list(booster = "gblinear")) # garbage results
importance <- xgb.importance(model = mx1, feature_names = colnames(train))
xgb.plot.importance(importance , top_n = 10)

message(paste0("Correlation with train data = ", round(cor(predict(object = mx1, train), routes.sub$Puptake)^2, 4)))

# experiments of cycling uptake with this model:
train_infra = routes.sub_vars %>%
  mutate(Pmain40_N = 0) %>%
  as.matrix()
uptake_infra = predict(object = mx1, train_infra)
message(paste0("This scenario results in an average of a ",
               round((mean(uptake_infra) - mean(routes.sub$Puptake)) * 100, 1),
               " percentage point increase in cycling"))

calc_up = function(x) {
  train_infra = select(routes.sub, -Puptake, -id, -contains("cycl"), -contains("all"), -contains("changecom")) %>%
    mutate(Pmain20_I01 = Pmain20_I01 + x) %>%
    as.matrix()
  uptake_infra = predict(object = mx1, train_infra)
  round((mean(uptake_infra) - mean(routes.sub$Puptake)) * 100, 1)
}
calc_up(-0.2)


# explore relationships with individual variables
rs = seq(0, 0.5, 0.1)
map(rs, ~ calc_up)


#remove unwanted varaibles
#mat <- as.matrix(routes.sub[,names(routes.sub)[!names(routes.sub) %in%
                                                # c("id",
                                                   #"all11","bicycle11","all01","bicycle01",
                                                 #  "Puptake","lengthSums","lengthratios")] ])
#mat <- mat[,c(colnames(mat)[grep("C",colnames(mat))],"length","rf_avslope_perc")]
#mat <- mat[,colnames(mat)[!colnames(mat) %in% c("Cmotorway","Ctrunk20_I","Cother40_I")] ]

#force variables

vars.tokeep <- c(names(routes)[grep("F",names(routes))],"length","rf_avslope_perc")


#vars.tokeep <- names(routes)[!names(routes.sub) %in% c("id","all11","bicycle11","all01","bicycle01","Puptake","lengthSums","lengthratios")]

#vars.tokeep <- c("Ccycleway","Cpath",
#                 "Cother20_N","Cother20_I","Cother30_N","Cother30_I","Cother40_N",
#                 "Cprimary20_N","Cprimary20_I","Cprimary30_N","Cprimary30_I","Cprimary40_N","Cprimary40_I",
#                 "Cresidential20_N", "Cresidential20_I", "Cresidential30_N", "Cresidential30_I", "Cresidential40_N","Cresidential40_I",
#                 "Csecondary20_N","Csecondary20_I","Csecondary30_N","Csecondary30_I","Csecondary40_N","Csecondary40_I",
#                 "Ctertiary20_N","Ctertiary20_I","Ctertiary30_N","Ctertiary30_I","Ctertiary40_N","Ctertiary40_I",
#                 "Ctrunk20_N","Ctrunk30_N","Ctrunk30_I","Ctrunk40_N","Ctrunk40_I",
#                "length","rf_avslope_perc" )

# Make the training and testing datasets
mat.train <- as.matrix(routes.train[,vars.tokeep])
mat.test <- as.matrix(routes.test[,vars.tokeep])


mat.train <- xgb.DMatrix(data = mat.train, label=routes.train$percycle11)
mat.test <- xgb.DMatrix(data = mat.test, label=routes.test$percycle11)

watchlist <- list(train=mat.train, test=mat.test)

model <- xgb.train(data = mat.train,
                 label = routes.train$percycle11,
                 watchlist=watchlist,
                 eval.metric = "error",
                 eval.metric = "logloss",
                 # objective = "reg:linear",
                 #nthread = 6,
                 #params = list(booster = "gblinear"),
                 nrounds = 500,
                 early_stopping_rounds = 10,
                 verbose = 1,
                 weight = routes.train$all01 + routes.train$all11)

#mat.all <- mat.all <- as.matrix(routes.sub[,colnames(mat)])
message(paste0("Correlation with train data = ", round(cor(predict(object = model, mat.train), routes.train$percycle11)^2,4) ))
message(paste0("Correlation with test data = ", round(cor(predict(object = model, mat.test), routes.test$percycle11)^2,4) ))

importance <- xgb.importance(model = model, feature_names = colnames(mat.train))
xgb.plot.importance(importance , top_n = 50)
#result <- data.frame(actual = routes.test$Puptake, predicted = predict(object = model, mat.test))
plot(sample_frac(data.frame(actual = routes.test$percycle11, predicted = predict(object = model, mat.test)), 0.1), col = "green")
points(sample_frac(data.frame(actual = routes.train$percycle11, predicted = predict(object = model, mat.train)), 0.1), col = "blue")
abline(a = 0, b = 1, col = "Red", lwd = 2)




print("End")


saveRDS(model,"../cyipt/input-data/m5.Rds")
