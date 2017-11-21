library(sf)
library(dplyr)

pct.all <- readRDS("../cyipt-securedata/pct-routes-all.Rds")
pct.all <- as.data.frame(pct.all)
pct.all$geometry <- NULL
gc()

pct.all$total <- pct.all$pct.census + pct.all$onfoot + pct.all$workathome + pct.all$underground + pct.all$train + pct.all$bus + pct.all$taxi + pct.all$motorcycle + pct.all$carorvan + pct.all$passenger + pct.all$other
pct.all$pcycle <- pct.all$pct.census / pct.all$total


train = pct.all %>%
  sample_n(round(nrow(pct.all)/2,0)) %>%
  select(ID, pct.census, total, length, busyness, pcycle,
         cum_hill,change_elev,dif_max_min,up_tot,down_tot,av_incline,calories,
         male_16_24, male_25_35, male_35_49, male_50_64, male_65_74, male_75p,
         female_16_24, female_25_34, female_35_49, female_50_64, female_65_74, female_75p
         )

m1 = glm(pcycle ~ length + I(length^0.5), data = train, weights = total, family = "quasipoisson")
plot(train$length, m1$fitted.values)
#summary(m1) # not much use
cor(m1$fitted.values * train$total, train$pcycle * train$total)^2
plot(m1$fitted.values  * train$total, train$pct.census)

m2 = glm(pcycle ~ busyness + I(busyness), data = train, weights = total, family = "quasipoisson")
plot(train$busyness, m2$fitted.values)
cor(m2$fitted.values * train$total, train$pcycle * train$total)^2

m3 <- glm(pcycle ~ length + I(length^0.5) + av_incline + av_incline * length, data = train, weights = total, family = "quasipoisson")
plot(train$length, m3$fitted.values)
cor(m3$fitted.values * train$total, train$pcycle * train$total)^2
plot(m3$fitted.values, train$pct.census)


m4 <- glm(pcycle ~ busyness + I(busyness^0.5) + av_incline + av_incline * busyness, data = train, weights = total, family = "quasipoisson")
plot(train$busyness, m4$fitted.values)
cor(m4$fitted.values * train$total, train$pcycle * train$total)^2

m5 <- glm(pcycle ~ length + busyness + I(busyness^0.5) + av_incline + av_incline * busyness, data = train, weights = total, family = "quasipoisson")
plot(train$busyness, m5$fitted.values)
cor(m5$fitted.values * train$total, train$pcycle * train$total)^2

m6 <- glm(pcycle ~ length + busyness + I(length^0.5) + av_incline + av_incline * busyness, data = train, weights = total, family = "quasipoisson")
plot(train$busyness, m6$fitted.values)
cor(m6$fitted.values * train$total, train$pcycle * train$total)^2


m7 <- glm(pcycle ~ length + I(length^0.5) + I(busyness^0.5) + av_incline + I(av_incline^0.5) , data = train, weights = total, family = "quasipoisson")
cor(m7$fitted.values * train$total, train$pcycle * train$total)^2
plot(m7$fitted.values, train$pcycle)
abline(a = 0, b = 1, col = "Red", lwd = 2)
m7$coefficients


foo =  exp(-2.2679333871 -0.0001207765 * train$length + 0.0242742120 * sqrt(train$length) -0.0075675068 * sqrt(train$busyness) -1.9347971306 * train$av_incline + -6.8377711996 * sqrt(train$av_incline))

plot(foo, train$pcycle)

as.formula(
  paste0("y ~ ", round(coefficients(m7)[1],2), " + ",
         paste(sprintf("%.2f * %s",
                       coefficients(m7)[-1],
                       names(coefficients(m7)[-1])),
               collapse=" + ")
  )
)

test <- data.frame(foo = round(foo * train$total,0), m7 = round(m7$fitted.values * train$total,0), census = train$pct.census)
test$diff <- test$foo - test$m7
predict(m7) == m7$fitted.values

library(xgboost)
xmat = as.matrix(select(train, length, busyness, av_incline))
m9 = xgboost(data = xmat, label = train$pcycle, nrounds = 5, weight = train$total)
cor(predict(object = m9, xmat)* train$total, train$pcycle * train$total)^2

train2 = mutate(train, busyness = busyness / 2)
xmat2 = as.matrix(select(train2, length, busyness, av_incline))

summary(predict(object = m9, xmat)* train2$total)

# try filtering out data that has very communting and or low cycling rates

train3 = pct.all %>%
  filter(total > 5) %>%
  filter(pct.census > 1) %>%
  #sample_n(round(nrow(pct.all)/20,0)) %>%
  select(ID, pct.census, total, length, busyness, av_incline, pcycle) %>%
  st_set_geometry(value = NULL)


m6.3 <- glm(pcycle ~ length + busyness + I(length^0.5) + av_incline + av_incline * busyness, data = train3, weights = total, family = "quasipoisson")
plot(train3$busyness, m6.3$fitted.values)
cor(m6.3$fitted.values * train3$total, train3$pcycle * train3$total)^2
plot(m6.3$fitted.values  * train3$total, train3$pct.census)



train4 = pct.all %>%
  #filter(total > 5) %>%
  filter(pct.census > 1) %>%
  #sample_n(round(nrow(pct.all)/20,0)) %>%
  select(ID, pct.census, total, length, busyness, av_incline, pcycle) %>%
  st_set_geometry(value = NULL)


m4.1 <- glm(pcycle ~ length + busyness + I(length^0.5) + av_incline + av_incline * busyness, data = train4, weights = total, family = "quasipoisson")
plot(train4$busyness, m4.1$fitted.values)
cor(m4.1$fitted.values * train4$total, train4$pcycle * train4$total)^2
plot(m4.1$fitted.values  * train4$total, train4$pct.census)



xmat4 = as.matrix(select(train4, length, busyness, av_incline))
m4.9 = xgboost(data = xmat4, label = train4$pcycle, nrounds = 20, weight = train4$total)
cor(predict(object = m4.9, xmat4)* train4$total, train4$pcycle * train4$total)^2
plot(predict(object = m4.9, xmat4)* train4$total, train4$pcycle * train4$total)

importance_m4.9 <- xgb.importance(model = m4.9, feature_names = c("length", "busyness", "av_incline"))
xgb.plot.importance(importance_m4.9)

train5 = pct.all %>%
  #filter(total > 5) %>%
  #filter(pct.census > 1) %>%
  sample_n(round(nrow(pct.all)/20,0)) %>%
  select(ID, pct.census, total, length, busyness, av_incline, publictransport, onfoot, motorvehicle, other) %>%
  st_set_geometry(value = NULL)

xmat5 = as.matrix(select(train5, length, busyness, av_incline, total, publictransport, onfoot, motorvehicle, other))
m5.9 = xgboost(data = xmat5, label = train5$pct.census, nrounds = 10)
cor(predict(object = m5.9, xmat5), train5$pct.census)^2
plot(train5$pct.census, predict(object = m5.9, xmat5), xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "Red", lwd = 2)

importance_m5.9 <- xgb.importance(model = m5.9, feature_names = c("length", "busyness", "av_incline","total", "publictransport", "onfoot", "motorvehicle", "other"))
xgb.plot.importance(importance_m5.9)


#Regional
regions <- readRDS("../cyipt-bigdata/boundaries/england_regions/england_regions.Rds")
regions <- regions[,c("rgn16nm")]


train6 = pct.all %>%
  #filter(total > 5) %>%
  #filter(pct.census > 5) %>%
  #sample_n(round(nrow(pct.all)/20,0)) %>%
  select(ID, pct.census, total, length, busyness, av_incline, publictransport, onfoot, motorvehicle, other, pcycle)

st_crs(regions) <- st_crs(train6) #hack don't do
train6 <- st_intersects(train6,regions[1,])


xmat6 = as.matrix(select(train6, length, busyness, av_incline))
m6.9 = xgboost(data = xmat6, label = train6$pcycle, nrounds = 10)
cor(predict(object = m6.9, xmat6) * train6$total, train6$pct.census)^2
plot(train6$pct.census, predict(object = m6.9, xmat6) * train6$total, xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "Red", lwd = 2)

importance_m6.9 <- xgb.importance(model = m6.9, feature_names = c("length", "busyness", "av_incline"))
xgb.plot.importance(importance_m6.9)


xmat7 = as.matrix(select(train,length,busyness,cum_hill,change_elev,dif_max_min,up_tot,down_tot,av_incline,calories,
                         male_16_24,male_25_35,male_35_49,male_50_64,male_65_74,male_75p,
                         female_16_24,female_25_34, female_35_49, female_50_64, female_65_74, female_75p))
m7.9 = xgboost(data = xmat7, label = train$pct.census, nrounds = 20)
cor(predict(object = m7.9, xmat7), train$pct.census)^2
plot(train$pct.census, predict(object = m7.9, xmat7), xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "Red", lwd = 2)

importance_m7.9 <- xgb.importance(model = m7.9, feature_names = c("length", "busyness", "cum_hill", "change_elev", "dif_max_min", "up_tot",
                                                                  "down_tot","av_incline", "calories", "male_16_24", "male_25_35", "male_35_49",
                                                                  "male_50_64", "male_65_74", "male_75p", "female_16_24", "female_25_34",
                                                                  "female_35_49", "female_50_64", "female_65_74", "female_75p"))
xgb.plot.importance(importance_m7.9)









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
cor(uptake * l$total, l_full$bicycle)^2
mean(uptake)
mean(l_full$bicycle / l_full$total)

# communicate result:
# knitr::spin(hair = "scripts/select_infra/simulate-uptake.R")
