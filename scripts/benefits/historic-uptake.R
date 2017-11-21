remove_cycle_infra = function(ways) {
  ways$cycleway.left = "no"
  ways$cycleway.right = "no"
  return(ways)
}

# test data
ways = readRDS("~/cyipt/cyipt-bigdata/osm-clean/BristolCityof/osm-lines.Rds")
cpaths = ways %>% filter(cycleway.left != "no") # 21km cycle paths...
summary(as.factor(ways$cycleway.left))
ways = remove_cycle_infra(ways)
summary # 200 cycle paths removed

# get uptake data from get-nomis data
# sustrans data - read-in from cyinfdat
sc2sd = readRDS("../cyinfdat/sc2sd")
i = readRDS("../cyinfdat/ri_04_11_dft")
qtm(l) +
  qtm(aggzone) +
  qtm(sc2sd, "green") +
  qtm(i) + # very little infrastructure there
  qtm(cpaths)

# imagine all infrastructure is new...
# lines most exposed to new infrastructure (within a 1km buffer around them)
l_buf = geo_buffer(l, dist = 1000) # looks good
st_crs(l_buf)
cpaths = st_transform(cpaths, 4326)
cpaths$length = as.numeric(st_length(cpaths))
l$cpath_length_buff = aggregate(cpaths["length"], l_buf, sum)$length
l$cpath_length_buff[is.na(l$cpath_length_buff)] = 0
summary(l)
plot(l$cpath_length_buff, l$uptake)
cor(l$cpath_length_buff, l$uptake, use = "complete.obs")
m1 = lm(formula = uptake ~ cpath_length_buff, data = l, weights = all11)

cor(predict(m1, l) * l$all, l$cpath_length_buff)^2

# subset those with high flows and high exposure
l_sub = l %>% filter(all11 > median(all11)) %>%
  filter(cpath_length_buff > median(.$cpath_length_buff))
r = line2route(l_sub)
