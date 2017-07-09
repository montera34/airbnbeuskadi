# Bestreta de dades per municipi
# Arxiu de dades en formate .csv separat per punt i coma , delimitador text "

# Càrrega de llibreries
library("stargazer")

# Creació de l'element frame data d'establiments turístics no et no ev
data <- read.csv("establiments_no_et_ev_fin_30052017.csv",sep=";")

# Comptatge de registres no et ev registrats (tots els grups)
summary(data$MUNICIPI)

# element integer d'etiquetes de municipi
labelsMUNICIPI <- levels(data$MUNICIPI)

# Loop en base als elements labelsMUNICIPI

for (i in labelsMUNICIPI) {

# Creació de frames per suma de total de places per a cada MUNICIPI

establiments_no_et_ev_fin_30052017 <- subset(data[,c('NOM','MUNICIPI','LOCALITAT','GRUP','SUBGRUP','NOMBRE.PLACES','NOMBRE.UNITATS')],MUNICIPI == i|GRUP == 'HOTEL')
print(i)
places <- sum(establiments_no_et_ev_fin_30052017$NOMBRE.PLACES)
print(places)
# write.csv(et_vt_mallorca_fin_24052017,paste('et_vt_mallorca_fin_24052017',i,'.csv'), row.names = FALSE)
}


