# Resum de comptatges
# Arxiu de dades en formate .csv separat per coma , delimitador text "

# Càrrega de llibreries
library("stargazer")

# Creació de l'element frame data
data <- read.csv("../../data/listings_donostia_simple.csv",sep=',')   

# Comptatge de tots els ítems

comptaroomtype <- summary(data$room_type)
comptaproperty_type <- summary(data$property_type)
sumaaccommodates <- sum(data$accommodates,NA,na.rm=TRUE)
sumaguests_included <- sum(data$guests_included,NA,na.rm=TRUE)
sumabathrooms <- sum(data$bathrooms,NA,na.rm=TRUE)
sumabeds <- sum(data$beds,NA,na.rm=TRUE)
summarybed_type <- summary(data$bed_type)
sumasquare_feet <- sum(data$square_feet,NA,na.rm=TRUE)
sumahost_total_listings_count <- sum(data$host_total_listings_count,NA,na.rm=TRUE)
comptalicense <- summary(data$license)

stargazer(comptaroomtype, type="text", font.size="small", title="Tipus d'habitatge", digits=1,flip=TRUE)
stargazer(comptaproperty_type, type="text", font.size="small", title="Tipus d'allotjament", digits=1,flip=TRUE)
stargazer(sumaaccommodates, type="text", font.size="small", title="Nombre d'hostes", digits=1, flip=TRUE)
stargazer(sumaguests_included, type="text", font.size="small", title="Nombre d'hostes addicionals", digits=1, flip=TRUE)
stargazer(sumabathrooms, type="text", font.size="small", title="Nombre de banys", digits=1, flip=TRUE)
stargazer(sumabeds, type="text", font.size="small", title="Nombre de llits", digits=1, flip=TRUE)
stargazer(summarybed_type, type="text", font.size="small", title="Tipus de llits", digits=1, flip=TRUE)
stargazer(sumasquare_feet, type="text", font.size="small", title="Superfície de l'allotjament peus quadrats", digits=1, flip=TRUE)
stargazer(sumahost_total_listings_count, type="text", font.size="small", title="Nombre d'hostes durant 2016", digits=1, flip=TRUE)
stargazer(comptalicense, type="text", font.size="small", title="Llicències", digits=1, flip=TRUE)



