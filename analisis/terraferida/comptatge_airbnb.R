
# Airbnb

# Carrega de llibreries
library(stargazer)

# Obre l'arxiu .csv
data <- read.csv("ng_Illes_Balears-Airbnb-20170411-2236_eivissa.csv",sep=";")

# Nombre de registres (habitatges)
Number = dim(data)

# Recollir la distribució per tipus d'habitatge
Compta_Room_type = summary(data$Room.Type)

# Suma de la capacitat total
Suma_Capacity = sum(data$Capacity)

# Suma de facturació per cada habitatge
Suma_Invoiced = sum(data$Invoiced, na.rm=T)

# Suma de pernoctes
Suma_Overnights = sum(data$Overnights, na.rm=T)

# Resum dels preus
Compta_Price = summary(data$Price)

# Resum de les capacitats de cada habitatge
Compta_Capacity = summary(data$Capacity)

# Resum dels llogaters principals
Compta_Username = summary(data$User.Name)

stargazer(Number, type="text", font.size="small", title="Nombre de registres", digits=1,flip=TRUE)
stargazer(Compta_Room_type, type="text", font.size="small", title="Tipus d'habitatge", digits=1,flip=TRUE)
stargazer(Suma_Capacity, type="text", font.size="small", title="Places totals ofertades", digits=1,flip=TRUE)
stargazer(Suma_Invoiced, type="text", font.size="small", title="Euros facturats", digits=1, flip=TRUE)
stargazer(Suma_Overnights, type="text", font.size="small", title="Nombre de pernoctacions", digits=1, flip=TRUE)
print("Distribució de preus per nit")
print(Compta_Price)
print("Distribució de places per habitatge")
print(Compta_Capacity)
print("Principals propietaris/gestors/nombre d'habitatges en propietat/gestionats")
print(Compta_Username)

