# Bestreta de dades per municipi
# Arxiu de dades en formate .csv separat per punt i coma , delimitador text "

# Càrrega de llibreries
library("stargazer")

# Creació de l'element frame data d'establiments turístics no et no ev
ata <- read.csv("establiments_no_et_ev_tramit_30052017.csv",sep=";")

data <- subset(ata[,c('NOM','MUNICIPI','LOCALITAT','GRUP','SUBGRUP','NOMBRE.PLACES','NOMBRE.UNITATS')],GRUP == 'HOTEL')

# Comptatge de registres no et ev en tramit (grup == HOTEL)
summary(data$MUNICIPI)

# element integer d'etiquetes de municipi
labelsMUNICIPI <- levels(data$MUNICIPI)

# Loop en base als elements labelsMUNICIPI

for (i in labelsMUNICIPI) {

# Creació de frames per suma de total de places per a cada MUNICIPI

hotels_registrats <- subset(data[,c('NOM','MUNICIPI','LOCALITAT','GRUP','SUBGRUP','NOMBRE.PLACES','NOMBRE.UNITATS')],MUNICIPI == i)
print(i)
places <- sum(hotels_registrats$NOMBRE.PLACES)
print(places)
# write.csv(hotels_registrats,paste('_',i,'.csv'), row.names = FALSE)
}


