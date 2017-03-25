#carga librerías
library("tidyverse")
#o solo carga ggplot2
#library("ggplot2")
#carla librería para reagrupar tablas de datos y hacer "melt"
#library("reshape2") #no sé la diferencia con "reshape"

#importa datos
listings = read.csv("data/listings.csv")

#view data basics
str(listings)
head(listings)
#añade variables para acceso fácil
attach(listings)

#municipios
citieslevels = levels(city)
cities = data.frame(citieslevels) #eran 330 y tras limpieza  270
nrow(cities) #número de municipios diferentes

propertytype = data.frame(levels(property_type))
roomtype = data.frame(levels(room_type))

#listadospor capitales
donostia = listings[city=="Donostia",] #eran 799 y tras limpieza  1215
bilbao = listings[city=="Bilbao",]
vitoria = listings[city=="Vitoria-Gasteiz",]

vitoriaclean = gsub(pattern = "\\$", replacement = "", vitoria)
gsub(pattern = "\\$", replacement = "", "sdfsdfdfs$sdfsdf$sdfsd ·$$")

#número de pisos en las capitales
ntotal = nrow(listings) #numero de pisos
ndonostia = nrow(donostia)
nbilbao = nrow(bilbao)
nvitoria = nrow(vitoria)

#pisos con licencia
ntotallicencia = nrow(listings[license!= "",])
ndonostialicencia = nrow(listings[license!= ""&city=="Donostia",])
nbilbaolicencia = nrow(listings[license!= ""&city=="Bilbao",])
nvitorialicencia = nrow(listings[license!= ""&city=="Vitoria-Gasteiz",])
#ndonostialicencia = nrow(donostia[donostia$license!= "",]) # dan mismo resultado

# % pisos con licencia
totalLicenciaPerCent = ntotallicencia / ntotal
donostiaLicenciaPerCent = ndonostialicencia / ndonostia
bilbaoLicenciaPerCent = nbilbaolicencia / nbilbao
vitoriaLicenciaPerCent = nvitorialicencia / nvitoria

#exports only some columns to avoid problems with scapes in data
donostia_simple = donostia[,c(1,5,20,22,23,49,50,51,52,53,54,55,56,57,58,61,75,77,88)]

#exporta data
write.table(donostia, "data/donostia.tsv", sep="\t",row.names=FALSE)
#need to export to tsv to avoid prolems with comas inside data
write.table(donostia_simple, "data/donostia_simple.tsv", sep="\t",row.names=FALSE) 


#------------------------Gráficos ---------------------------------
#numero de dormitorios y tipo de alojamiento
ggplot(data = listings) + 
  geom_point(mapping = aes(x = property_type, y = bedrooms),  position = "jitter") + coord_flip()

#numeroo de dormitorios  y tipo de oferta
ggplot(data = listings) + 
  geom_point(mapping = aes(x = room_type, y = bedrooms),  position = "jitter")  + coord_flip()

ggplot(listings) + 
  geom_point(mapping = aes(x = city, y = bedrooms),  position = "jitter")

ggplot(listings) + 
  geom_point(mapping = aes(x = room_type, y = bedrooms)) + 
  facet_wrap(~ city, nrow = 3)

