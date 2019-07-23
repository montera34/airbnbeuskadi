# count points in polygons: https://gis.stackexchange.com/questions/110117/counts-the-number-of-points-in-a-polygon-in-r#110246
# read GeoJSON in R https://stackoverflow.com/questions/24183007/is-it-possible-to-read-geojson-or-topojson-file-in-r-to-draw-a-choropleth-map

#  Lodas libraries
library("raster")
library("sp")
# restart R .rs.restartR() if rgdal does not work
library(rgdal)

# check ogr drivers to see if GeoJSON is loaded:
# ogrDrivers()

## read files

# shapes
barrios <- readOGR("data/barrios-donostia_simplificado.geojson")
menores <- readOGR("data/output/limites/unidades-menores-donostia_cleaned-merged.geojson")
# class(menores) # checks that is patialPolygonsDataFrame
# plot(menores) # plots the shape map
# proj4string(menores) # check CRS

# points
# airbnb <- read.delim("data/original/180423_listings-airbnb-donostia_datahippo.csv",sep = ",")
# airbnb <- read.delim("data/output/vut-donostia/censo-viviendas-turisticas-donostia-180301.csv",sep = ",")
# airbnb <- read.delim("data/output/vut-donostia/censo-viviendas-turisticas-donostia-180301_WGS84.csv",sep = ",")
# airbnb <- read.delim("data/output/vut-donostia/censo-viviendas-turisticas-donostia-20180914-wgs84.csv",sep = ",")
# airbnb <- read.delim("data/output/180604_listings-airbnb-donostia_datahippo_with-last-review-20180912-reviewed.csv",sep = ",")
# airbnb <- read.delim("data/output/temp/180926_listings-airbnb_donostia_datahippo_with-last-review-20181127-reviewed_active-filtered.csv",sep = ",")
airbnb <- data_long

airbnb$latitude <- as.numeric(airbnb$latitude)
airbnb$longitude <- as.numeric(airbnb$longitude)

## Get long and lat from your data.frame. Make sure that the order is in lon/lat.
# source: https://stackoverflow.com/questions/29736577/how-to-convert-data-frame-to-spatial-coordinates#29736844
xy <- airbnb[,c("longitude","latitude")]
airbnbSp <- SpatialPointsDataFrame(coords = xy, data = airbnb, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# class(airbnbSp)
# proj4string(airbnbSp)

# serves to count points (people in Padrón) in polygon (barrios)
# retrieve  the  geometry  indices  of Barrios at  the  locations  of A (people points).  
# More  in particular, an integer vector of length length(A) is returned, with NA values for locations in 
# A not matching with locations in B (e.g.  those points outside a set of polygons).
# (https://cran.r-project.org/web/packages/sp/vignettes/over.pdf) 
countBarrios <- over(airbnbSp, barrios)
# table(countBarrios$BAR_DS_O)

# same calc for Unidades menores
countMenores <- over(airbnbSp, menores)
# table(countMenores$Unidad_men)

# Adds calculated barrios to spatial poligon
airbnbSp$barrio <- countBarrios$BAR_DS_O
airbnbSp$umenores <- countMenores$Unidad_men

# Where are those points without barrio
library(ggmap)
qmplot(longitude, latitude, data = airbnb[is.na(airbnbSp$barrio =="no location"),], maptype = "toner-lite", 
       color = I("red"),alpha = I(.2)) + labs(title= "Points without barrio" )
qmplot(longitude, latitude, data = airbnb, maptype = "toner-lite", 
       color = I("red"),alpha = I(.2)) + labs(title= "Points without barrio" )

airbnb <- as.data.frame(airbnbSp) # convert spatial data to regular data frame
# removes duplicated columns with lat and long
drops <- c("latitude.1","longitude.1") 
airbnb <- airbnb[ , !(names(airbnb) %in% drops)]

# Some points will be outside polygons and have Barrio variable fixed
airbnb[is.na(airbnbSp$barrio),]$name

# There are n points that have no Barrio assigned
length(airbnb[is.na(airbnbSp$barrio),]$name)

# library(dplyr)
# airbnb$barrio <- lapply(airbnb$barrio, as.character)
# airbnb$umenores <- lapply(airbnb$umenores, as.character)

# Assign "no location" to points
# airbnb[is.na(airbnbSp$barrio),]$barrio <- "no location"
# airbnb[is.na(airbnbSp$umenores),]$umenores <- "no location"
# airbnb[airbnb$umenores == "Sag�es" ,]$umenores <- "Sagües"

# saves file
# save(airbnb,file="data/output/180423_listings-airbnb-donostia_datahippo_barrio-umenor.Rda")
# write.csv(airbnb, file = "data/output/180423_listings-airbnb-donostia_datahippo_barrio-umenor.csv", row.names = FALSE)
# write.csv(airbnb, file = "data/output/vut-donostia/censo-viviendas-turisticas-donostia-180301_barrio-umenor.csv", row.names = FALSE)
# write.csv(airbnb, file = "data/output/vut-donostia/censo-viviendas-turisticas-donostia-20180914_barrio-umenor.csv", row.names = FALSE)
# write.csv(airbnb, file = "data/output/180604_listings-airbnb-donostia_datahippo_with-last-review-20180912-reviewed_barrio-umenor.csv", row.names = FALSE)
write.csv(airbnb, file = "tmp/listings-evolution.csv", row.names = FALSE)
