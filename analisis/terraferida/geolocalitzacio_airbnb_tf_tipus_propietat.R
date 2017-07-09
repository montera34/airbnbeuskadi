# Bestreta de geolocalitzacions per tipus de propietat
# Arxiu de dades en formate .csv separat per coma , delimitador text "

# Càrrega de llibreries
library("stargazer")

# Creació de l'element frame data
data <- read.csv("data_1.csv",sep=',')   

# element integer d'etiquetes de zipcode
labelsproperty_type <- levels(data$property_type)

# Loop en base als elements labelsproperty_type

for (i in labelsproperty_type) {

# Crea els frames de tots els ítems per a cada una dels tipus de propietat i creacio de fitxers .csv

property_type_geoloc <- subset(data[,c('latitude','longitude','picture_url','property_type')],property_type == i)
write.csv(property_type_geoloc,paste('property_type_geoloc_',i,'.csv'), row.names = FALSE)
}


