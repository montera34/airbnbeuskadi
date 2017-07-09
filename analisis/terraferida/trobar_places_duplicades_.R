# Duplicats places per nom
# carregar fitxer amb tots els registres

data_tot <- read.csv("llistat_no_et_ev_tots_.csv",sep=";")

data <- subset(data_tot[,c('NOM','LOCALITAT','GRUP','NOMBRE.PLACES','NOMBRE.UNITATS','ESTAT')],GRUP == 'TURISME INTERIOR')

 
library("dplyr")

a1 <- data %>% group_by(NOM) %>% summarize(count=n())
a2 <- filter(a1,count >=2)

# element integer de NOM si estan repetits dues vegades

labelsNOM <- a2$NOM

# Loop en base als elements labelsNOM

for (i in labelsNOM) {

# Creació de frames per suma de total de places per a cada NOM

establiments_no_et_ev_fin_30052017 <- subset(data[,c('NOM','LOCALITAT','GRUP','NOMBRE.PLACES','NOMBRE.UNITATS','ESTAT')],NOM == i)
print(i)
print(establiments_no_et_ev_fin_30052017)

places <- sum(establiments_no_et_ev_fin_30052017$NOMBRE.PLACES)
print(places)
}
