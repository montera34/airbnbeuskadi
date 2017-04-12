#importa datos
listings <- read.delim("data/original/listings-csv_tras-openrefine2.tsv", sep = "\t")

#creates variable True/false for with/without license
listings$withlicense <- listings$license!=""

#Añade variable para precio como número y no como factor, diferente de "price"
listings$precio <- as.numeric(levels(listings$price))[listings$price]

#Exporta a csv y elimina las columnas que dan problemas por comillas no escapadas
listings_simple <- listings[,c(-6,-7,-8,-10,-11,-12,-13,-14,-15,-25,-35,-59)]

#--------------------------export data --------------------
# write.table(listings, "data/listings_euskadi.csv", sep=",", row.names=FALSE)
# write.table(listings_simple, "data/listings_euskadi_simple.csv", sep=",", row.names=FALSE)

write.table(listings, "data/listings_euskadi.tsv", sep="\t", row.names=FALSE)
write.table(listings_simple, "data/listings_euskadi_simple.tsv", sep="\t", row.names=FALSE)

#listados por capitales
donostia = listings[listings$city=="Donostia",]
bilbao = listings[listings$city=="Bilbao",]
vitoria = listings[listings$city=="Vitoria",]
#exports only some columns to avoid problems with scapes in data
donostia_simple = listings_simple[listings$city=="Donostia",]
bilbao_simple = listings_simple[listings$city=="Bilbao",]
vitoria_simple = listings_simple[listings$city=="Vitoria",]
alava_simple = listings_simple[listings$neighbourhood_group_cleansed=="Alava",]
guipuzcoa_simple = listings_simple[listings$neighbourhood_group_cleansed=="Guipuzcoa",]
vizcaya_simple = listings_simple[listings$neighbourhood_group_cleansed=="Vizcaya",]

#--------------------------export data --------------------
write.table(donostia, "data/listings_donostia.csv", sep=",",row.names=FALSE)
write.table(donostia_simple, "data/listings_donostia_simple.csv", sep=",",row.names=FALSE)

write.table(bilbao, "data/listings_bilbao.csv", sep=",",row.names=FALSE)
write.table(bilbao_simple, "data/listings_bilbao_simple.csv", sep=",",row.names=FALSE)

write.table(vitoria, "data/listings_vitoria.csv", sep=",",row.names=FALSE)
write.table(vitoria_simple, "data/listings_vitoria_simple.csv", sep=",",row.names=FALSE)