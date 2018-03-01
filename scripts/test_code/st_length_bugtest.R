library(sf)
#Make some random squares
# calualte the perimeter usin sf and basic maths
# compare the ratio whcih should be 1 in all cases

for(n in 1:100){
  #make some random dimesensions
  i = runif(1,0,10000000)
  j = runif(1,0,10000000)
  
  #make a square
  pol <- rbind(c(i,i), c(j,i), c(j,j), c(i,j), c(i,i))
  pol <- st_polygon(list(pol))
  
  #calcualte the perimeter in two ways
  perimeter.expected = 4 * sqrt((i-j)**2)
  perimeter.calc = st_length(pol)
  
  #alt method
  line <- st_cast(pol, "LINESTRING")
  perimeter.alt = st_length(line)
  
  #return the ratio
  message(paste0("ratio of values is ",perimeter.alt/perimeter.expected))
}




