#Evaluate Schemes

# Read in a scheme

schemes <- readRDS("../cyipt-bigdata/osm-prep/Leeds/schemes.Rds")

#For now add some random data
schemes$ncycle_before <- round(runif(nrow(schemes), 0.0, 100), 0)
schemes$ncycle_after <-  schemes$ncycle_before + round(runif(nrow(schemes), 0.0, 100), 0)
schemes$carkm <- - (schemes$ncycle_after - schemes$ncycle_before) * round(runif(nrow(schemes), 0.0, 5), 2) * 2 * 220


# Main Blocks

# Jouney Qualitiy
# From WebTAG A4.1.6
# Value of jounrey ambience benefit of cycling facilities 2015 prices and values

qual.data <- data.frame(scheme = c("Off-road segregated cycle track", "On-road segregated cycle lane", "On-road non-segregated cycle lane", "Wider lane", "Shared bus lane"), value = c(8.07,3.43,3.41,2.08,0.88))

schemes$jouney_qual_ben <- NA
for(a in 1:nrow(schemes)){
  #Select correct value and convert to £ / s
  if(schemes$type[a] %in% c("Segregated Cycle Track", "Stepped Cycle Tracks", "Cycle Lanes with light segregation")){
    val <- qual.data$value[qual.data$scheme == "On-road segregated cycle lane"] / 100 / 60
  }else if(schemes$type[a] == "Cycle Lanes"){
    val <- qual.data$value[qual.data$scheme == "On-road non-segregated cycle lane"] / 100 / 60
  }else if(schemes$type[a] == "Segregated Cycle Track on Path"){
    val <- qual.data$value[qual.data$scheme == "Off-road segregated cycle track"] / 100 / 60
  }else{
    val <- 0
  }

  #For now assume that all people cycle 50% length of the scheme
  #Ben = #cyclists X length cycled / 8 mph  X value of time cycled X 2 for return journey * 220 days per year * 0.5 (rule of half) + same again for existing without rule of half
  schemes$jouney_qual_ben[a] <- round((schemes$ncycle_after[a] - schemes$ncycle_before[a]) * schemes$length[a] * 0.5 / 3.576 * val * 2 * 220 * 0.5,2) + round((schemes$ncycle_before[a]) * schemes$length[a] * 0.5 / 3.576 * val * 2 * 220,2)
  rm(val)
}


# Physical Activity
#Based on http://www.cedar.iph.cam.ac.uk/blog/dft-tag-cedar-010917/

schemes$health_deathavoided <- NA
schemes$health_benefit <- NA

#Gender and age split of the observed main-mode cycle trips in England (reference: NTS 2012-14).
gender_split <- data.frame(Male = c(16,39,13,5,1), Female = c(4,16,5,1,0))
row.names(gender_split) <- c("0-19","20-49","50-64","65-80","80+")

# Observed speed of main mode cycling trips (miles/hour) in England (reference: NTS 2012-2014).
gender_speed <- data.frame(Male = c(6.12,9.12,8.91,7.48,7.99), Female = c(4.45,7.22,7.07,5.99,4.2))
row.names(gender_speed) <- c("0-19","20-49","50-64","65-80","80+")

#Background mortality rates by age and gender (reference: Global Burden of Disease Study 2015 results for England)
gender_mortality <- data.frame(Male = c(4.1949E-04,1.1833E-03,6.2669E-03,2.4591E-02,1.1471E-01), Female = c(3.1919E-04,7.1164E-04,4.1887E-03,1.6686E-02,9.9484E-02))
row.names(gender_mortality) <- c("0-19","20-49","50-64","65-80","80+")

#Discounted, average Years of Life Lost (YLL) loss per death
gender_YLL <- data.frame(Male = c(47.71,34.06,23.73,15.13,5.78), Female = c(48.00,33.55,23.73,14.34,5.78))
row.names(gender_YLL) <- c("0-19","20-49","50-64","65-80","80+")

#Discount rate
years <- 0:20
dis <- (1/1.015) ** years
discount <- data.frame(year = years, discount = dis )
rm(years,dis)
#For now assume that cycle along the lenght of the scheme

for(b in 1:nrow(schemes)){
  #Loop THough schemes and calcualte the deaths avoided

  #Estimate the age a gnerde mix of new cyclists
  #Note: does not use weight as PCT values better represent number of cyclists than number of trips
  #Otherwise the weight roughyl halves the number of cyclists
  gender_new <- gender_split / 100 * (schemes$ncycle_after[b] - schemes$ncycle_before[b])

  dist <- schemes$length[b] * 1.9 / 1609.34  # convert to miles * 1.9 (two way weighting factor)
  #convert to hours per week for each gender and age bracket
  gender_deathsavoided <- (dist * 4.22) / gender_speed # distance per week = distance * 4.22 (days per week)

  #convert to METS
  #Average Metabolically Equivalent Tasks (MET) for cycling
  #(reference: Compendium of Physical Activities, https://sites.google.com/site/compendiumofphysicalactivities/)
  gender_deathsavoided <-  gender_deathsavoided * 6.8

  #Convert to Relative Risks
  # Relative risks (RRs) for all-cause mortality for cycling (reference: Kelly et al. 2014) 0.9
  gender_deathsavoided <-  exp(gender_deathsavoided * log(0.9)/11.25)

  # Max benefits from cycling (benefit cap) 0.55
  gender_deathsavoided$Male[gender_deathsavoided$Male < 0.55] <- 0.55
  gender_deathsavoided$Female[gender_deathsavoided$Female < 0.55] <- 0.55

  #Convert to PAF due to cycling
  gender_deathsavoided <- 1 - gender_deathsavoided

  #Convert to Deaths Avoided
  gender_deathsavoided <- gender_deathsavoided * gender_mortality * gender_new
  #factor out 0-19 years
  gender_deathsavoided[1,] <- c(0,0)

  #Calc YYLs
  gender_yearslost <- gender_deathsavoided * gender_YLL

  health_benefits <- discount
  health_benefits$benefits <- health_benefits$discount * sum(gender_yearslost) * 60000 # Value of a startical life in 2012

  schemes$health_deathavoided[b] <- round(sum(gender_deathsavoided),2)
  schemes$health_benefit[b] <- round(sum(health_benefits$benefits),2)
  rm(gender_deathsavoided,health_benefits, gender_yearslost, dist, gender_new)

}

# Accidents

#Environmental Impacts


# GHG emissions





#air quality - WebTAG being updated soon


#noise

# indirect tax revenue

# travel time (decongestion)
# Cost of congestion from WebTAG Data book A5.4.4
#For yorkshire Mon-Fri Average 15.6 p / km

schemes$congestion_benefit <- round(schemes$carkm * 0.156 * -1, 2)


# Absenteeism

