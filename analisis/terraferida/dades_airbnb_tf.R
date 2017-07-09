# Resum de comptatges per a cada municipi
# Arxiu de dades en formate .csv separat per coma , delimitador text "

# Càrrega de llibreries
library("stargazer")

# Creació de l'element frame data
data <- read.csv("data_1.csv",sep=',')   

# element integer d'etiquetes de zipcode
labelscity <- levels(data$zipcode)


# Loop en base als elements labelscity

for (i in labelscity) {

# Crea els frames de tots els ítems per a cada una de les localitats

zipcoderoom_type <- subset(data[,c('zipcode','room_type')],zipcode == i)
zipcodeproperty_type <- subset(data[,c('zipcode','property_type')],zipcode == i)
zipcodeaccommodates <- subset(data[,c('zipcode','accommodates')],zipcode == i)
zipcodebathrooms <- subset(data[,c('zipcode','bathrooms')],zipcode == i)
zipcodebedrooms <- subset(data[,c('zipcode','bedrooms')],zipcode == i)
zipcodesquare_feet <- subset(data[,c('zipcode','square_feet')],zipcode == i)
zipcodehost_total_listings_count <- subset(data[,c('zipcode','host_total_listings_count')],zipcode == i)

# Comptatge de tots els ítems

comptaroomtype <- summary(zipcoderoom_type$room_type)
comptaproperty_type <- summary(zipcodeproperty_type$property_type)
sumaaccommodates <- sum(zipcodeaccommodates$accommodates)
sumabathrooms <- sum(zipcodebathrooms$bathrooms)
sumabedrooms <- sum(zipcodebedrooms$bedrooms)
sumasquare_feet <- sum(zipcodesquare_feet$squarefeet)
sumahost_total_listings_count <- sum(zipcodehost_total_listings_count$host_total_listings_count)

print(i)
stargazer(comptaroomtype, type="text", font.size="small", title="Tipus d'habitació", digits=1,flip=TRUE)
stargazer(comptaproperty_type, type="text", font.size="small", title="Tipus d'allotjament", digits=1,flip=TRUE)
stargazer(sumaaccommodates, type="text", font.size="small", title="Nombre d'hostes", digits=1, flip=TRUE)
stargazer(sumabathrooms, type="text", font.size="small", title="Nombre de banys", digits=1, flip=TRUE)
stargazer(sumabedrooms, type="text", font.size="small", title="Nombre de llits", digits=1, flip=TRUE)
stargazer(sumasquare_feet, type="text", font.size="small", title="Superfícies de l'allotjament", digits=1, flip=TRUE)
stargazer(sumahost_total_listings_count, type="text", font.size="small", title="Nombre d'hostes durant 2015", digits=1, flip=TRUE)
}


