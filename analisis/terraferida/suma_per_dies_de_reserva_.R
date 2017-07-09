# Nombre de hostes, ocupació per propietari o comercialitzador
# Arxiu de dades en formate .csv separat per coma , delimitador text "

# Càrrega de llibreries
library("stargazer")

# Creació de l'element frame data
data <- read.csv("calendar.csv",sep=',')   

# element integer d'etiquetes de data
label_date <- levels(data$date)

# Loop en base als elements label_date

for (i in label_date) 
{

# Creació del frame per a cada data

date_data <- subset(data[,c('listing_id','date','available','price')],date == i)

# Habitatges_disponibles_per_dia

suma_ocupats_lliures <- summary(date_data$available)

print(paste(i,',',suma_ocupats_lliures))

}


