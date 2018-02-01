# Price Paid data(

pp <- read.csv("C:/Users/earmmor/Downloads/Land Registry Price Paid/pp-complete.csv", header = F)
names(pp) <- c("id","price","date","postcode","propertytype","newbuild","estatetype","number","name","street", "locality", "town", "district", "county", "V15", "V16")

pp <- pp[,c("price","date","postcode","propertytype","newbuild","estatetype","number","name","street", "locality", "town", "district", "county")]


saveRDS(pp,"C:/Users/earmmor/Downloads/Land Registry Price Paid/pricepaid_Jan_2018.Rds")

pp.bromley <- pp[pp$district == "BROMLEY",]
#pp.bromley <- pp.bromley[pp.bromley$estatetype == "F",]
pp.bromley$date <- as.Date(as.character(pp.bromley$date))
pp.bromley <- pp.bromley[pp.bromley$date >= as.Date("2001-08-18"),]
pp.bromley$address <- paste0(pp.bromley$number,"-",pp.bromley$street,"-",pp.bromley$postcode)
pp.bromley <- pp.bromley[order(pp.bromley$date, decreasing = T),]
pp.bromley <- pp.bromley[!duplicated(pp.bromley$address),]
summary(duplicated(pp.bromley$address))


summary(pp.bromley$date)

#Read in Land Map

library(sf)
library(tmap)
tmap_mode("view")

poly <- st_read("C:/Users/earmmor/Downloads/Inspire Polygons/Bromley/Land_Registry_Cadastral_Parcels.gml")
poly$gml_id <- NULL
poly$LABEL <- NULL
poly$NATIONALCADASTRALREFERENCE <- NULL
poly$VALIDFROM <- as.Date(as.character(poly$VALIDFROM))
poly$BEGINLIFESPANVERSION <- as.Date(as.character(poly$BEGINLIFESPANVERSION))

# Differne (i.e has the house been sold recently)
poly$diff <- as.numeric(poly$BEGINLIFESPANVERSION - poly$VALIDFROM)
summary(poly$diff)

poly.sub <- poly[poly$diff != 0,]
qtm(poly.sub)



# comeare dates
#pp.sub <- pp.bromley[pp.bromley$date %in% poly$BEGINLIFESPANVERSION,]
pp.sub <- pp.bromley[pp.bromley$date == as.Date("2013-08-30"),]
pp.sub <- pp.bromley[pp.bromley$street == "THE CRESCENT",]
