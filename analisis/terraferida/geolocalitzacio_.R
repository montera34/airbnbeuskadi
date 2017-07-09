# Bestreta de geolocalitzacions
# Arxiu de dades en formate .csv separat per coma i punt , delimitador text "

# Càrrega de llibreries
library("stargazer")

# Creació de l'element frame data
data <- read.csv("ng_Illes_Balears-Homeaway-20170411-2230.csv",sep=';')   

#creacio de fitxer .csv

i = "homeaway"

geoloc <- subset(data[,c('Latitude','Longitude','Url')])
write.csv(geoloc,paste('geoloc_',i,'.csv'), sep=';',row.names = FALSE)

