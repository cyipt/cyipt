remove_cycle_infra = function(ways) {
  ways$cycleway.left = "no"
  ways$cycleway.right = "no"
  return(ways)
}

# test data
ways = readRDS("~/cyipt/cyipt-bigdata/osm-clean/BristolCityof/osm-lines.Rds")
summary(as.factor(ways$cycleway.left))
ways = remove_cycle_infra(ways)
summary(as.factor(ways$cycleway.left))
