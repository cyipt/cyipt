library(sf)
library(tidyverse)
library(sf)

l = readRDS("../cyipt-securedata/route_infra_final.Rds")
l = l[1:1e5,]



head(l)

n = names(l)
c_props = grep(pattern = "P", n)
col_sums = rowSums(l[c_props], na.rm = T)
summary(col_sums)
hist(col_sums, breaks = c(-0.001, 0.001, 0.2, 0.4, 0.8, 1, 80))
plot(col_sums)
summary(col_sums < 1.)

# summary of infrastructure added
sum(l$Ccycleway * l$length, na.rm = T) / 1000
sum(l$Cprimary40_I * l$length, na.rm = T) / 1000
sum(l$Cprimary30_I * l$length, na.rm = T) / 1000
sum(l$Csecondary40_I * l$length, na.rm = T) / 1000
sum(l$Csecondary30_I * l$length, na.rm = T) / 1000

l_less_30 = filter(l, Csecondary30_I < 0)
sum(l_less_30$Ccycleway * l_less_30$length, na.rm = T) / 1000
sum(l_less_30$Cpath)
sum(l_less_30$Cpath)
sum(l$Csecondary20_I * l$length, na.rm = T) / 1000

sum(l$Cother30_I * l$length, na.rm = T) / 1000
sum(l$Cother30_I * l$length, na.rm = T) / 1000
cor(l$Ccycleway, )
