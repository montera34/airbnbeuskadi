# Análisis de Idealista en Donostia

# ---- Load libraries -----
library(tidyverse)
library(rgdal)
library(ggmap) #for theme nothing
library(reshape2)
library(scales)
library(formattable) #para tablas enriquecidas

# ------------ to export format table ------------
library("htmltools")
library("webshot")
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

# ------ Load data ------

# Load shapes
barrios <- readOGR("data/barrios-donostia_simplificado.geojson")
menores <- readOGR("data/output/limites/unidades-menores-donostia_cleaned-merged.geojson")
mar <- readOGR("data/original/shapes/mar-donostia.geojson")
rio <- readOGR("data/original/shapes/rio-donostia.geojson")
# edificios <- readOGR("data/original/shapes/edificios-donostia.geojson")

# Load data
# Idealista
idealista_barrio_mediano <- read.delim("data/output/idealista/donostia/idealista-barrio-precio-mediano-2012-2016.csv",sep = ",")
idealista_barrio_medio <- read.delim("data/output/idealista/donostia/idealista-barrio-precio-medio-2012-2016.csv",sep = ",")
idealista_barrio_medio_m2 <- read.delim("data/output/idealista/donostia/idealista-barrio-precio-medio-metro-cuadrado-2012-2016.csv",sep = ",")
idealista_menor_medio_m2 <- read.delim("data/output/idealista/donostia/idealista-unidad-menor-precio-medio-metro-cuadrado-2012-2016.csv",sep = ",")

# location of center point barios and unidad menores
load("data/output/limites/donostia/barrios.names.location.Rda")
load("data/output/limites/donostia/menores.names.location.Rda")

# correct location of Egia name
barrios.names.location[barrios.names.location$BAR_DS_O=="Egia",]$lat <- 43.31799

# ----------- Prepare data -------------

# Create matrix
# idealista_barrio_matrix <-dcast(idealista_barrio_mediano,barriocal ~ yearf)

# ---- Análisis gráficos de línea evolución 2012-2017 por barrio------ 

# Precios medianos mensualidad
ggplot() +
  geom_line(data=na.omit(idealista_barrio_mediano), aes(x = yearf, y = precio.mediano, group=barriocal), alpha=0.3,color="#888888") +
  # geom_line(data=na.omit(mean_total), aes(x = yearf, y = precio.medio, group=1), alpha=1,color="#000000",size=2) +
  # geom_point(data=na.omit(mean_total), aes(x = yearf, y = precio.medio), alpha=1,color="#000000",size=2) +
  coord_cartesian(ylim = c(0, 1500)) + theme(legend.position="none") +
  labs(title = "Idealista: precios medianos por barrios y media total. Pisos de 1-4 habitaciones. Max 3.000€")+
  ylab("Precio medio (€)") + xlab("Años") + labs(colour = "Nº habitaciones") +
  theme(axis.text.y = element_text(size=12),
        # axis.title.y=element_blank(),
        axis.ticks.y =element_blank(),
        axis.ticks.x =element_blank(),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14,face="bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        # legend.position = "bottom",
        legend.text = element_text(size=15) ) +
  geom_text(data = na.omit(idealista_barrio_mediano), aes(label=format(round(precio.mediano, 1), nsmall = 1, decimal.mark=','), x = yearf, y = precio.mediano), hjust=0.5, vjust=2)

# Precios medios mensualidad
ggplot() +
  geom_line(data=na.omit(idealista_barrio_medio), aes(x = yearf, y = precio.medio, group=barriocal), alpha=0.3,color="#888888") +
  # geom_line(data=na.omit(mean_total), aes(x = yearf, y = precio.medio, group=1), alpha=1,color="#000000",size=2) +
  # geom_point(data=na.omit(mean_total), aes(x = yearf, y = precio.medio), alpha=1,color="#000000",size=2) +
  coord_cartesian(ylim = c(0, 1500)) + theme(legend.position="none") +
  labs(title = "Idealista: precios medio por barrios. Pisos de 1-4 habitaciones. Max 3.000€")+
  ylab("Precio medio (€)") + xlab("Años") + labs(colour = "Nº habitaciones") +
  theme(axis.text.y = element_text(size=12),
        # axis.title.y=element_blank(),
        axis.ticks.y =element_blank(),
        axis.ticks.x =element_blank(),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14,face="bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        # legend.position = "bottom",
        legend.text = element_text(size=15) ) 

# Precios medios metro cuadrado
ggplot() +
  geom_line(data=na.omit(idealista_barrio_medio_m2[!idealista_barrio_medio_m2$precio.metro.cuadrado.medio>20,]), aes(x = yearf, y = precio.metro.cuadrado.medio, group=barriocal), alpha=0.3,color="#888888") +
  # geom_line(data=na.omit(mean_total), aes(x = yearf, y = precio.medio, group=1), alpha=1,color="#000000",size=2) +
  # geom_point(data=na.omit(mean_total), aes(x = yearf, y = precio.medio), alpha=1,color="#000000",size=2) +
  coord_cartesian(ylim = c(0, 15)) + theme(legend.position="none") +
  labs(title = "Idealista: precios medio por m2 barrios. Pisos de 1-4 habitaciones. Max 3.000€")+
  ylab("Precio medio (€)") + xlab("Años") + labs(colour = "Nº habitaciones") +
  theme(axis.text.y = element_text(size=12),
        # axis.title.y=element_blank(),
        axis.ticks.y =element_blank(),
        axis.ticks.x =element_blank(),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14,face="bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        # legend.position = "bottom",
        legend.text = element_text(size=15) ) 


# ------ Evolucion precio m2 ------

# Convierte a matriz
idealista_barrio_medio_m2_matrix <-dcast(idealista_barrio_medio_m2,barriocal ~ yearf)
idealista_menor_medio_m2_matrix <-dcast(idealista_menor_medio_m2,menores ~ yearf)

# Calcula valores evolución 
# Diferencia
idealista_barrio_medio_m2_matrix$dif13_17 <- idealista_barrio_medio_m2_matrix[,7] - idealista_barrio_medio_m2_matrix[,3]
# Porcentaje de cambio 2013-2017
idealista_barrio_medio_m2_matrix$evol13_17 <- round(100*(idealista_barrio_medio_m2_matrix[,7] - idealista_barrio_medio_m2_matrix[,3])/idealista_barrio_medio_m2_matrix[,3],digits = 1)
# Porcentaje de cambio 2016-2017
idealista_barrio_medio_m2_matrix$evol16_17 <- round(100*(idealista_barrio_medio_m2_matrix[,7] - idealista_barrio_medio_m2_matrix[,6])/idealista_barrio_medio_m2_matrix[,6],digits = 1)
as.data.frame.matrix(idealista_barrio_medio_m2_matrix)

ft <- formattable( idealista_barrio_medio_m2_matrix[,-c(2,8:10)],
             list( area(col = 1:6) ~ color_tile("transparent", "#2b8cbe")))
export_formattable(ft,"images/idealista/tablas-idealista-precios-m2-barrios-donostia-2013-17.png")

ft <- formattable( idealista_barrio_medio_m2_matrix[,-c(2:7)],
             list( area(col = 1:4) ~ color_tile("transparent", "#2b8cbe")))
export_formattable(ft,"images/idealista/tablas-idealista-evolucion-precios-m2-barrios-donostia-2013-17.png")

# Diferencia
idealista_menor_medio_m2_matrix$dif13_17 <- idealista_menor_medio_m2_matrix[,7] - idealista_menor_medio_m2_matrix[,3]
# Porcentaje de cambio 2013-2017
idealista_menor_medio_m2_matrix$evol13_17 <- round(100*(idealista_menor_medio_m2_matrix[,7] - idealista_menor_medio_m2_matrix[,3])/idealista_menor_medio_m2_matrix[,3],digits = 1)
# Porcentaje de cambio 2016-2017
idealista_menor_medio_m2_matrix$evol16_17 <- round(100*(idealista_menor_medio_m2_matrix[,7] - idealista_menor_medio_m2_matrix[,6])/idealista_menor_medio_m2_matrix[,6],digits = 1)

# ----- Prepara mapas ------

# Precio metro cuadrado por barrio
# Une los contornos "barrios" con los datos acumulados en formato matriz
test <- merge(barrios, idealista_barrio_medio_m2_matrix, by.x="BAR_DS_O", by.y="barriocal")
# añade nº de barrio a la variable id
test@data$id <- rownames(test@data)
# convierte en data.frame al SpaialPolygonsDataFrame
sf.points <- fortify(test, region="id")
# añade los datos al data.frame
barrio_m2 <-left_join(sf.points, test@data, by="id")

# Une los contornos "unidades menores" con los datos acumulados en formato matriz
test <- merge(menores, idealista_menor_medio_m2_matrix, by.x="Unidad_men", by.y="menores")
# añade nº de barrio a la variable id
test@data$id <- rownames(test@data)
# convierte en data.frame al SpaialPolygonsDataFrame
# testb = rgeos::gBuffer(test, width=0)
sf.points <- fortify(test, region="id")
# añade los datos al data.frame
menor_m2 <-left_join(sf.points, test@data, by="id")

# ------ Mapas evolución -----

# evolución precio metro cuadrado por barrio
# Location of center of polygon
barrios.names.location <- left_join(barrios.names.location, barrio_m2, by="BAR_DS_O") #cnames stores name location of barrio
barrios.names.location <- barrios.names.location[!duplicated(barrios.names.location$BAR_DS_O),] # get only unique barrios

png(filename="images/idealista/mapa-idealista-evolucion-precio-m2-barrios-donostia-2013-17.png",width = 900,height = 600)
ggplot() +
  geom_polygon(data = barrio_m2, 
               aes(x = long, y = lat, group = group, fill = evol13_17), 
               colour="white",  
               size = 0.5) +
  scale_fill_distiller(name="Evolución precios (%)", palette = "RdBu",
                       breaks = pretty_breaks(n = 4),limits = c(-44, 44))+ #direction = 1, 
  scale_colour_gradient() +
  # coord_map() +
  coord_quickmap(xlim=c(-2.08, -1.92), ylim=c(43.2775,43.335))  +
  theme_nothing(legend = TRUE) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 13) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Evolución precio mensual medio por m² por barrio en Donostia. 2013-2017",
       subtitle = "Ofertas debajo de 3.000€ de 1 a 4 habitaciones.",
       x = NULL,
       y = NULL,
       caption = "Datos: Idealista. Gráfico: lab.montera34.com/airbnb") +
  geom_text(data=barrios.names.location, 
            aes(long.x, lat.x,
                label = paste(BAR_DS_O,paste(evol13_17,"%",sep=""),sep = "\n")),
            # color = porcentajeviv), #substring(BAR_DS_O,1,2)," ",
            size=3,color = "black") #fontface="bold"
dev.off()

# evolución precio metro cuadrado por unidad menor
menores.names.location <- merge(menores.names.location, menor_m2, by.x="menores", by.y="Unidad_men") #menores.names.location stores name location of menores
menores.names.location <- menores.names.location[!duplicated(menores.names.location$menores),] # get only unique unidades menores

png(filename="images/idealista/mapa-idealista-evolucion-precio-m2-menores-donostia-2013-17.png",width = 900,height = 600)
ggplot() +
  geom_polygon(data = menor_m2, 
               aes(x = long, y = lat, group = group,fill = evol13_17),
               colour="white",  
               size = 0.2) +
  scale_fill_distiller(name="Evolución precios (%)", palette = "RdBu",
                       breaks = pretty_breaks(n = 4),limits = c(-121, 121))+ #direction = 1,
  scale_colour_gradient() +
  # coord_map() +
  coord_quickmap(xlim=c(-2.08, -1.92), ylim=c(43.2775,43.335))  +
  theme_nothing(legend = TRUE) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Evolución precio mensual medio por m² por unidad menor en Donostia. 2013-2017",
       subtitle = "Ofertas debajo de 3.000€ de 1 a 4 habitaciones.",
       x = NULL,
       y = NULL,
       caption = "Datos: Idealista. Gráfico: lab.montera34.com/airbnb") +
  geom_text(data=menores.names.location, 
            aes(long.x, lat.x,
                label = paste(evol13_17,sep="")),
            size=2.5,color = "black")
dev.off()

# ----- Mapas precio ----
# renames variable to be able to use it
names(menor_m2)[names(menor_m2) == '2016'] <- 'x2016'
names(menor_m2)[names(menor_m2) == '2017'] <- 'x2017'

# Mapa precio por m2 en unidad menor
png(filename="images/idealista/mapa-idealista-precio-m2-menores-donostia-2017.png",width = 900,height = 600)
ggplot() +
  geom_polygon(data = menor_m2, 
               aes(x = long, y = lat, group = group,fill = x2017), #TODO: no funciona
               colour="white",  
               size = 0.2) +
  scale_fill_distiller(name="Precios €/m² mes", palette = "Blues",
                       breaks = pretty_breaks(n = 4),direction = 1)+ #,
  scale_colour_gradient() +
  # coord_map() +
  coord_quickmap(xlim=c(-2.08, -1.92), ylim=c(43.2775,43.335))  +
  theme_nothing(legend = TRUE) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Precio mensual medio por m² por unidad menor en Donostia. 2017",
        subtitle = "Ofertas debajo de 3.000€ de 1 a 4 habitaciones.",
       x = NULL,
       y = NULL,
       caption = "Datos: Idealista. Gráfico: lab.montera34.com/airbnb") 
  # geom_text(data=menores.names.location, 
  #           aes(long.x, lat.x,
  #               label = paste(evol13_17,sep = "\n")),
  #           size=3,color = "black")
dev.off()
