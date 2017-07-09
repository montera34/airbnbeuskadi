# Bestreta de dades per localitat
# Arxiu de dades en formate .csv separat per punt i coma , delimitador text "

# Càrrega de llibreries
library("stargazer")

# Creació de l'element frame data d'establiments turístics no et no ev
data <- read.csv("et_vt_mallorca_tramit_24052017.csv",sep=";")

# Comptatge de registres no et ev tramit
summary(data$LOCALITAT)

# element integer d'etiquetes de zipcode
labelsLOCALITAT <- levels(data$LOCALITAT)

# Loop en base als elements labelsGRUP

for (i in labelsLOCALITAT) {

# Creació de frames per suma de total de places per a cada GRUP

establiments_no_et_ev_fin_30052017 <- subset(data[,c('NOM','MUNICIPI','LOCALITAT','GRUP','SUBGRUP','NOMBRE.PLACES','NOMBRE.UNITATS')],LOCALITAT == i)
print(i)
places <- sum(establiments_no_et_ev_fin_30052017$NOMBRE.PLACES)
print(places)
write.csv(establiments_no_et_ev_fin_30052017,paste('et_vt_mallorca_tramit_24052017',i,'.csv'), row.names = FALSE)
}


