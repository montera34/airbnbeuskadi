#carga librerías
#library("tidyverse") #solo se usa muchos pasos más adelante
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

levels(city)
citieslevels = levels(city)
cities = data.frame(citieslevels) #eran 330 y tras limpieza  270

length(listings[city=="Donostia",])
donostia = listings[city=="Donostia",] #eran 799 y tras limpieza  1215
bilbao = listings[city=="Bilbao",]