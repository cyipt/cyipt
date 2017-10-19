library(dplyr)
library(sf)
lfq = readRDS("../example-data/bristol/lfq.Rds")
plot(lfq$rf)
head(lfq$rf)

train_geo = lfq$rf %>%
  select(busyness, av_incline, length)
train = lfq$l %>%
  select(id, bicycle, all) %>%
  mutate(pcycle = bicycle / all) %>%
  select(id, pcycle, all) %>%
  cbind(train_geo) %>%
  st_set_geometry(value = NULL)

m1 = glm(pcycle ~ length + I(length^0.5), data = train, weights = all, family = "quasipoisson")
plot(train$length, m1$fitted.values)
summary(m1) # not much use
cor(m1$fitted.values * train$all, train$pcycle * train$all)^2

m2 = glm(pcycle ~ busyness + I(busyness), data = train, weights = all, family = "quasipoisson")
plot(train$busyness, m2$fitted.values)
cor(m2$fitted.values * train$all, train$pcycle * train$all)^2

m3 <- glm(pcycle ~ length + I(length^0.5) + av_incline + av_incline * length, data = train, weights = all, family = "quasipoisson")
plot(train$length, m3$fitted.values)
cor(m3$fitted.values * train$all, train$pcycle * train$all)^2

m4 <- glm(pcycle ~ busyness + I(busyness^0.5) + av_incline + av_incline * busyness, data = train, weights = all, family = "quasipoisson")
plot(train$busyness, m4$fitted.values)
cor(m4$fitted.values * train$all, train$pcycle * train$all)^2

m5 <- glm(pcycle ~ length + busyness + I(busyness^0.5) + av_incline + av_incline * busyness, data = train, weights = all, family = "quasipoisson")
plot(train$busyness, m5$fitted.values)
cor(m5$fitted.values * train$all, train$pcycle * train$all)^2

m6 <- glm(pcycle ~ length + busyness + I(length^0.5) + av_incline + av_incline * busyness, data = train, weights = all, family = "quasipoisson")
plot(train$busyness, m6$fitted.values)
cor(m6$fitted.values * train$all, train$pcycle * train$all)^2

library(xgboost)
xmat = as.matrix(select(train, length, busyness, av_incline))
m9 = xgboost(data = xmat, label = train$pcycle, nrounds = 5, weight = train$all)
cor(predict(object = m9, xmat)* train$all, train$pcycle * train$all)^2

xmat2 = as.matrix(select(train2, length, busyness, av_incline))
train2 = mutate(train, busyness = busyness / 2)
summary(predict(object = m9, xmat)* train2$all)

# find change


logit_pcycle = -3.894 + (-0.5872 * distance) + (1.832 * sqrt(distance) ) + (0.007956 * distance^2)


# road scenarios

knitr::knit(input = "model-uptake.Rmd")
typology = readr::read_csv("input-data/roadtypes4.csv")
rc = readRDS("../example-data/bristol/results/osm-schemes.Rds")
summary(ways$length)
select(ways, contains("cycle")) %>% summary()
ways_long = dplyr::filter(ways, length > 500)
ways_long$length_cycleway = ways_long$length
summary(ways_long$cycleway)
l_joined = stjoin_old(lfq$rf, ways_long["length_cycleway"], FUN = sum) # old way
summary(l_full$length_cycleway)
summary(l_joined$length_cycleway) # 5 fold increase in cycling
l_full$length_cycleway = l_joined$length_cycleway
uptake = predict(m8, as.matrix(l_full[-c(1, 2)]))
cor(uptake * l$all, l_full$bicycle)^2
mean(uptake)
mean(l_full$bicycle / l_full$all)

# communicate result:
# knitr::spin(hair = "scripts/select_infra/simulate-uptake.R")
