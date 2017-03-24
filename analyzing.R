#carga librerías
library("tidyverse")
#o solo carga ggplot2
#library("ggplot2")
#carla librería para reagrupar tablas de datos y hacer "melt"
#library("reshape2") #no sé la diferencia con "reshape"

#importa datos
listings = read.csv("data/listings.csv")

str(listings)
head(listings)
#añade datos
attach(listings)
listings$city 

citieslevels = levels(city)
cities = data.frame(citieslevels) #eran 330 y tras limpieza  270

propertytype = levels(property_type)
propertytype = data.frame(propertytype)

roomtype = levels(room_type)
roomtype = data.frame(roomtype)

levels(license)

length(listings[city=="Donostia",])
donostia = listings[city=="Donostia",] #eran 799 y tras limpieza  1215
bilbao = listings[city=="Bilbao",]
vitoria = listings[city=="Vitoria-Gasteiz",]

#exports only some columns to avoid problems with scapes in data
donostia_simple = donostia[,c(1,5,20,22,23,49,50,51,52,53,54,55,56,57,58,61,75,77,88)]

#exporta tabla
write.table(donostia, "data/donostia.tsv", sep="\t",row.names=FALSE)
#need to export to tsv to avoid prolems with comas inside data
write.table(donostia_simple, "data/donostia_simple.tsv", sep="\t",row.names=FALSE) 

#numero de habitaciones y tipo de oferta
ggplot(data = listings) + 
  geom_point(mapping = aes(x = room_type, y = bedrooms),  position = "jitter")

ggplot(istings) + 
  geom_point(mapping = aes(x = city, y = bedrooms),  position = "jitter")

ggplot(listings) + 
  geom_point(mapping = aes(x = room_type, y = bedrooms)) + 
  facet_wrap(~ city, nrow = 3)

