# Carrega de llibreries i autorització gpclib

require("gpclib")
require("rgdal")
require("maptools")
gpclibPermit()
require("maptools")
require("ggplot2")
require("plyr")
require("scales")

# Carrega arxiu de dades

places.data <- read.csv(file="prova.csv", sep=";")

# Construccio recintes muncipals a partir de les dades del IGN
# L'arxiu .shp i els derivats es troben al mateix directori de treball

fn='recintos_municipales_inspire_peninbal_etrs89.shp'
esp = readOGR(fn,layer='recintos_municipales_inspire_peninbal_etrs89')

# Construccio recintes municipals de Balears

balearstot <- subset(esp, esp$CODNUT2 == "ES53")

# Preparacio del mapa (camp municipi a les dades i NAMEUNIT han de coincidir). Selecció municipis.
balears <- subset(balearstot[,c('NAMEUNIT','CODNUT1','CODNUT2','CODNUT3')],NAMEUNIT== 'Alaró'|NAMEUNIT == 'Alcúdia'|NAMEUNIT=='Algaida'|NAMEUNIT=='Andratx'|NAMEUNIT=='Ariany'|NAMEUNIT=='Artà'| NAMEUNIT=='Banyalbufar'|NAMEUNIT=='Binissalem'|NAMEUNIT=='Búger'|NAMEUNIT=='Bunyola'|NAMEUNIT=='Calvià'|NAMEUNIT=='Campanet'|NAMEUNIT=='Campos'|NAMEUNIT=='Capdepera'|NAMEUNIT=='Consell'|NAMEUNIT=='Costitx'|NAMEUNIT=='Deià'|NAMEUNIT=='Escorca'|NAMEUNIT=='Esporles'|NAMEUNIT=='Estellencs'|NAMEUNIT=='Felanitx'|NAMEUNIT=='Fornalutx'|NAMEUNIT=='Inca'|NAMEUNIT=='Lloret de Vistalegre'|NAMEUNIT=='Lloseta'|NAMEUNIT=='Llubí'|NAMEUNIT=='Llucmajor'|NAMEUNIT=='Manacor'|NAMEUNIT=='Mancor de la Vall'|NAMEUNIT=='Maria de la Salut'|NAMEUNIT=='Marratxí'|NAMEUNIT=='Montuïri'|NAMEUNIT=='Muro'|NAMEUNIT=='Palma'|NAMEUNIT=='Petra'|NAMEUNIT=='Pollença'|NAMEUNIT=='Porreres'|NAMEUNIT=='Puigpunyent'|NAMEUNIT=='Santa Eugènia'|NAMEUNIT=='Santa Margalida'|NAMEUNIT=='Santa María del Camí'|NAMEUNIT=='Santanyí'|NAMEUNIT=='Sant Joan'|NAMEUNIT=='Sant Llorenç des Cardassar'|NAMEUNIT=='Sa Pobla'|NAMEUNIT=='Selva'|NAMEUNIT=='Sencelles'|NAMEUNIT=='ses Salines'|NAMEUNIT=='Sineu'|NAMEUNIT=='Sóller'|NAMEUNIT=='Son Servera'|NAMEUNIT=='Valldemossa'|NAMEUNIT=='Vilafranca de Bonany')

balears@data$id <- rownames(balears@data)
balears.df <- fortify(balears)
balears.df <- join(balears.df,balears@data,by="id")
balears.df <- merge(balears.df,places.data,by.x="NAMEUNIT",by.y="municipi",all.x=T,all.y=F)


# Afegir les capes

 ggp <- ggplot(data=balears.df,aes(x=long,y=lat,group=group))
 ggp <- ggp + geom_polygon(aes(fill=places_tramit)) 
 ggp <- ggp + geom_path(color="grey",size=0.1)
 ggp <- ggp + coord_equal()
 ggp <- ggp + scale_fill_distiller(palette = "Purples", breaks = pretty_breaks(n = 10), direction=1)
 ggp <- ggp + guides(fill = guide_legend(reverse=TRUE))
 ggp <- ggp + labs(title="Places hoteleres en tràmit\nMallorca, maig 2017",subtitle="Font: Conselleria d'Innovació, Recerca i Turisme Illes Balears", fill="")


# Dibuixar el mapa
# ggsave, genera un fitxer d'imatge
# print, genera un fitxer .pdf

ggsave(ggp, file = "map_hoteleres_tramit.png", width = 6, height = 4.5, type = "cairo-png")

# print(ggp)

# https://stackoverflow.com/questions/19791210/r-ggplot2-merge-with-shapefile-and-csv-data-to-fill-polygons
# http://www.kevjohnson.org/making-maps-in-r/
# Referències





