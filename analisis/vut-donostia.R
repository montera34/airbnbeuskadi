# VUT oficiales Donostia
# Este archivo genera mapas basados en los datos oficiales de viviendas y habitaciones turísticas
# según la web del Ayuntamiento de Donostia (descargado en data/output/vut-donostia/censo-viviendas-turisticas-donostia-180301_WGS84.csv).
# Hay datos disponibles para abril y septiembre 2018, fecha de las descarga. Selecciona uno u otro.
# Los gráficos se guardan en el directorio images/vut-donostia/

# ------ Load libraries------
library(tidyverse)
# read shapes
library(rgdal)
library(ggmap) #for theme nothing
library(gsubfn) #for selecting estado

# ------ Load data and shapes ------
barrios <- readOGR("data/barrios-donostia_simplificado.geojson")
menores <- readOGR("data/output/limites/unidades-menores-donostia_cleaned-merged.geojson")
mar <- readOGR("data/original/shapes/mar-donostia.geojson")
rio <- readOGR("data/original/shapes/rio-donostia.geojson")
# edificios <- readOGR("data/original/shapes/edificios-donostia.geojson")

# Load zonas VUT
zona_saturada <- readOGR("data/original/vut-donostia/VUT_saturada-donostia-WGS84.geojson")
zona_b <- readOGR("data/original/vut-donostia/VUT_zonaB-donostia-WGS84.geojson")
zona_c <- readOGR("data/original/vut-donostia/VUT_zonaC-donostia-WGS84.geojson")

# Load viviendas por barrio
viviendas_barrios <-  read.delim("data/viviendas-barrios-donostia.csv",sep = ",")

# Load VUT data ---------

# Load VUT 
# Select one file
# Abril 2018 ----
vut <- read.delim("data/output/vut-donostia/censo-viviendas-turisticas-donostia-180301_barrio-umenor.csv",sep = ",", encoding = "utf-8")
date <- "Abril 2018"
date_abr <- "20180301"

# Septiembre 2018 ----
vut <- read.delim("data/output/vut-donostia/censo-viviendas-turisticas-donostia-20180914_barrio-umenor.csv",sep = ",", encoding = "utf-8")
date <- "Septiembre 2018"
date_abr <- "20180914"
# Fix data> missing barrio and unicada menor for VUT (september 2018)
vut[27,]$barrio <- "Centro"
vut[27,]$umenores <- "Area Romantica"

# create taxonomy for estado -------
levels(vut$estado)
vut$estadox <- strapplyc( as.character(vut$estado), ".*_(.*)", simplify = TRUE)
vut$estadox <- factor(vut$estadox,levels = c("Favorable","Tramitacion"))
# reorder levels
vut$estadox <- factor(vut$estadox,levels(vut$estadox)[c(2,1)])

# Changes estadox name by estado
# April
colnames(vut) <- c("estado_orig","tipo","direccion","longitude","latitude", "barrio","umenores","estado")
# September
colnames(vut) <- c("estado_orig","tipo","direccion","x","y","longitude","latitude", "barrio","umenores","estado")

# clean wrong character in accent
levels(vut$tipo) <- c("Habitaciones de uso turístico","Viviendas de uso turístico")

# add noise to location of points ------
vut$latr <- jitter(vut$latitude, factor=1, amount = 0.001)
vut$lonr <- jitter(vut$longitude, factor=1, amount = 0.001)
# Corrige nombre de unidad menor
levels(vut$umenores)[levels(vut$umenores)=="Sag�es"] <- "Sagües"
# check numbers for VUT
table(vut$estado_orig)
table(vut$estado)
table(vut$tipo)
table(vut$tipo,vut$estado)

# -------------colores ------------
cbPalette <- c("#017c0a","#f4830f")

# ----- Barras por barrios-------
vut0 <- vut %>%
  group_by(barrio) %>% 
  summarise(count=n()) %>% 
  arrange(count)

cbPalette0 <- c("#FF8888","#FFAAAA","#EEEEEE","#CCCCCC","#EEEEEE","#CCCCCC","#EEEEEE","#CCCCCC",
                "#EEEEEE","#CCCCCC","#EEEEEE","#CCCCCC","#EEEEEE","#CCCCCC","#EEEEEE","#CCCCCC")

png(filename=paste("images/vut-donostia/vut-barras-barrio-",date_abr,".png",sep = ""),width = 900,height = 400)
ggplot( data = vut0, aes(x = 1, y = count, 
                         fill=factor(barrio, levels=c("Centro","Gros","Antiguo","Amara Berri",
                                                       "Egia","Ibaeta","Aiete","Intxaurrondo","Ibaiondo","Ategorrieta-Ulia",
                                                       "Miramon-Zorroaga","Miracruz-Bidebieta","Loiola",
                                                       "Igeldo","Altza","Añorga","Martutene")))) +
  scale_fill_manual(values=cbPalette0) +
  geom_bar(stat="identity") +
  # scale_y_continuous(limits = c(0,625), expand = c(0, 0)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(),
    legend.position="right", axis.text = element_blank()
    
  ) +
  guides(fill=guide_legend(title="Barrio")) +
  labs(title = "Viviendas de uso turístico por barrio",
       subtitle = paste("Donostia. ",date,".",sep=""),
       x = NULL,
       y = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb") +
  annotate("text", x = 1, y = 800, label = "Centro y Gros (75,6%)", size = 6) +
  annotate("text", x = 1, y = 200, label = "Resto de barrios", size = 5) +
  coord_flip()
dev.off()

# ----- Barras por barrios y estado-------
vut1 <- vut %>%
  group_by(barrio,estado) %>% 
  # group_by(barrio,umenores,tipo,estado) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

cbPalette1 <- c("#f4830f","#017c0a")

png(filename=paste("images/vut-donostia/vut-barras-estado-tramitacion-barrio-",date_abr,".png",sep = ""),width = 900,height = 600)
ggplot( data = vut1, aes(x = reorder(barrio, suma), y = count, group = estado)) +
  scale_fill_manual(values=cbPalette1) +
  geom_col(aes(fill=estado)) +
  scale_y_continuous(limits = c(0,625), expand = c(0, 0)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top"
  ) +
  guides(fill=guide_legend(title="Estado")) +
  labs(title = "Viviendas de uso turístico según estado de tramitación por barrio",
       subtitle = paste("Donostia. ",date,".",sep=""),
       x = NULL,
       y = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = count,
                x = reorder(barrio, suma), y = count), 
            data=vut1[vut1$count > 15,], 
            position = position_stack(vjust = 0.5),size=4,color="#FFFFFF") +
  geom_text(aes(label = suma,
                x = reorder(barrio, suma), y = suma+10), 
            data=vut1[vut1$count > 0,],  
            position = "dodge",
            size=3,color="#AAAAAA") +
  coord_flip()
dev.off()

vut1$percent <- round( vut1$count / nrow(vut) * 100, digits=1 )
vut1$percent_estado <- round( vut1$count / vut1$suma * 100, digits=1 )

# Viviendas de uso turístico según habitación/apartamento por barrio ---------------
vut2 <- vut %>% 
  group_by(barrio,tipo) %>%
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

png(filename=paste("images/vut-donostia/hab-viv-barras-barrio-vut-",date_abr,".png",sep = ""),width = 900,height = 600)
ggplot(vut2,aes(x = reorder(barrio, suma), y = count, group = tipo)) +
  geom_col(aes(fill=tipo)) +
  scale_y_continuous(limits = c(0,625), expand = c(0, 0)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Viviendas de uso turístico según habitación/apartamento por barrio",
       subtitle = paste("Donostia. ",date,".",sep=""),
       x = NULL,
       y = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = count,
                x = reorder(barrio, suma), y = count), 
            data=vut2[vut2$count > 13,], 
            position = position_stack(vjust = 0.5),size=4,color="#FFFFFF") +
  geom_text(aes(label = suma,
                x = reorder(barrio, suma), y = suma+10), 
            data=vut2[vut2$count > 0,],  
            position = "dodge",
            size=3,color="#AAAAAA") +
  coord_flip()
dev.off()

vut2$percent <- round( vut2$count / nrow(vut) * 100, digits=1 )
vut2$percent_tipo <- round( vut2$count / vut2$suma * 100, digits=1 )

# ----- barras por unidad menor-------

vut3 <- vut %>%
  group_by(umenores,tipo,estado) %>%
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

png(filename=paste("images/vut-donostia/viviendas-umenor-barras-vut-",date_abr,".png",sep = ""),width = 900,height = 1800)
ggplot(vut3[vut3$tipo=="Viviendas de uso turístico",],aes(x = reorder(umenores,suma), y = count, group =estado)) +
  geom_col(aes(fill=estado)) +
  scale_fill_manual(values=cbPalette1) +
  scale_y_continuous(limits = c(0,425), expand = c(0, 0)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Viviendas de uso turístico según estado de tramitación por unidad menor",
       subtitle =  paste("Donostia. ",date,".",sep=""),
       x = NULL,
       y = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = count,
                x = reorder(umenores, suma), y = count), 
            data=vut3[vut3$count > 15,], 
            position = position_stack(vjust = 0.5),size=4,color="#FFFFFF") +
  geom_text(aes(label = suma,
                x = reorder(umenores, suma), y = suma+5), 
            position = "dodge",
            size=3,color="#AAAAAA") +
  coord_flip()
dev.off()

vut3$percent <- round(vut3$count / vut3$suma * 100, digits = 2) 

vut3b <- vut %>% 
  group_by(umenores,tipo) %>%
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

vut3b$percent <- round(vut3b$count / vut3b$suma * 100, digits = 2) 

# ------------tablas por tipos de habitación-------
vut4 <- vut %>% 
  group_by(tipo,estado) %>% 
  summarise(count=n()) %>%
  mutate(suma=sum(count))

png(filename=paste("images/vut-donostia/hab-viv-barras-vut-donostia-",date_abr,".png",sep = ""),width = 900,height = 600)
ggplot(vut4 ,aes(x = tipo, y = count,group =estado)) +
  geom_col(aes(fill=estado)) +
  scale_y_continuous(limits = c(0,1200), expand = c(0, 55)) +
  scale_fill_manual(values=cbPalette1) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) +
  labs(title = "Habitaciones y viviendas (VUT) en Donostia. Estado tramitación",
       subtitle =  paste("Donostia. ",date,".",sep=""),
       y = "número de anuncios",
       x = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = count,
                x = tipo, y = count), 
            data=vut4, 
            position = position_stack(vjust = 0.5),size=4,color="#FFFFFF") +
  geom_text(aes(label = suma,
                x = tipo, y = suma+37), 
            data=vut4,  
            position = "dodge",
            size=3,color="#AAAAAA") +
  coord_flip()
dev.off()



# --------cálculo de ratio VUT por nº viviendas-------
barrios_vut <- vut %>%
  group_by(barrio) %>% 
  # group_by(barrio,umenores,tipo,estado) %>% 
  summarise(vut=n()) %>% 
  # mutate(suma=sum(count)) %>%
  arrange(-vut)

barrios_vut <- merge(barrios_vut,viviendas_barrios,by.x="barrio",by.y="barrios")
barrios_vut$ratio_vut <- round(barrios_vut$vut / barrios_vut$Total.Viviendas.familiares *100,digits=2)
# barrios_vut$ratio_airbnb <- round(barrios_vut$count_vals / barrios_vut$Total.Viviendas.familiares *100,digits=2)

write.csv(barrios_vut[,-c(3)], file = paste("data/output/vut-donostia/por-barrios-censo-viviendas-turisticas-donostia",date_abr,".csv",sep=""), row.names = FALSE)

png(filename=paste("images/vut-donostia/ratio-vut-barras-donostia-barrio-",date_abr,".png",sep = ""),width = 900,height = 600)
ggplot(barrios_vut,aes(x = reorder(barrio, ratio_vut), y = ratio_vut)) +
  geom_col() +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Ratio de viviendas de uso turístico (VUT) por cada 100 viviendas por barrio",
       subtitle =  paste("Donostia. ",date,".",sep=""),
       x = NULL,
       y = "VUT por cada 100 viviendas familiares",
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = ratio_vut),
            data = barrios_vut[barrios_vut$ratio_vut > 0.25,],
            position = position_stack(vjust = 0.5),size=4,color="#FFFFFF") +
  coord_flip()
dev.off()

# --------------- Mapas --------------------

cbPalette <- c("#017c0a","#f4830f")

png(filename=paste("images/vut-donostia/map-vut-tramitacion-donostia-",date_abr,"_zoom.png",sep = ""),width = 900,height = 600)
ggplot() + 
  # mar y río
  geom_polygon(data=mar,aes(x=long, y=lat,group=group),fill="#f0f5fc",alpha=0.5) +
  geom_polygon(data=rio,aes(x=long, y=lat,group=group),fill="#f0f5fc",alpha=0.5) +
  # zonas de VUT
  geom_polygon(data=zona_saturada,aes(x=long, y=lat,group=group),fill="#2b8cbe") +
  geom_polygon(data=zona_b,aes(x=long, y=lat,group=group),fill="#a6bddb") +
  geom_polygon(data=zona_c,aes(x=long, y=lat,group=group),fill="#eeeeee") +
  # edificios
  # geom_polygon(data=edificios,aes(x=long, y=lat,group=group),fill="#bbbbbb") +
  # Puntos VUT
  scale_colour_manual(values=cbPalette) +
  geom_point(data=vut,aes(x=longitude, y=latitude,color=estado,shape=tipo),
             alpha=0.9,size = 2)+
  # geom_point(data=vut,aes(x=lonr, y=latr,colour=estado,shape=tipo),
             # alpha=0.9,size = 2)+
  # barrios o unidades menores
  geom_path(data=barrios,aes(x=long, y=lat,group=group), colour="black",size = 0.1)+
  # geom_path(data=barrios[barrios@data$BAR_DS_O == "Centro" | barrios@data$BAR_DS_O == "Gros",],
  #           aes(x=long, y=lat,group=group),
  #           colour="black",size = 0.7)+
  # Theme options: 
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text = element_blank()) +
  # coord_quickmap(xlim=c(-2.08, -1.92), ylim=c(43.2775,43.335)) + #todo donostia
  coord_quickmap(xlim=c(-2.02, -1.96), ylim=c(43.30,43.331)) + #zoom 
  guides(colour = guide_legend(override.aes = list(alpha = 0.9))) +
  labs(title = "Habitaciones y viviendas de uso turístico según estado de tramitación",
     subtitle = "Un total de 1.289 elementos. 1.163 viviendas (526 en tramitación) y 126 habitaciones (77 en tramitación).",
     # subtitle =  paste("Donostia. ",date,".",sep=""),
     x = NULL,
     y = NULL,
     caption = paste("Donostia. ",date,".", " Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb",sep=""))
dev.off()


ggplot() + 
  scale_fill_manual(values=cbPalette) +
  geom_point(data=vut[vut$tipo=="Habitaciones de uso turístico",],aes(x=lonr, y=latr,colour=estado,shape=tipo),
             alpha=0.5,size = 3)+
  geom_path(data=menores,aes(x=long, y=lat,group=group), colour="black",size = 0.1)+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  # theme_nothing(legend = TRUE) +
  # theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # coord_quickmap(xlim=c(-2.08, -1.92), ylim=c(43.2775,43.335)) + #todo donostia
  coord_quickmap(xlim=c(-2.02, -1.96), ylim=c(43.30,43.331)) + #zoom 
  # coord_fixed() +
  # xlim(-1.968,-1.94) +
  # ylim(43.305,43.325) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.9))) +
  labs(title = "Habitaciones de uso turístico según estado de tramitación",
       subtitle = "125 habitaciones (73 en tramitación)",
       x = NULL,
       y = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb")

ggplot() + 
  scale_fill_manual(values=cbPalette) +
  geom_point(data=vut[vut$tipo=="Viviendas de uso turístico",],aes(x=lonr, y=latr,colour=estado,shape=tipo),
             alpha=0.5,size = 3)+
  geom_path(data=menores,aes(x=long, y=lat,group=group), colour="black",size = 0.1)+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  # theme_nothing(legend = TRUE) +
  # theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # coord_quickmap(xlim=c(-2.08, -1.92), ylim=c(43.2775,43.335)) + #todo donostia
  coord_quickmap(xlim=c(-2.02, -1.96), ylim=c(43.30,43.331)) + #zoom 
  # coord_fixed() +
  # xlim(-1.968,-1.94) +
  # ylim(43.305,43.325) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.9))) +
  labs(title = "Viviendas de uso turístico según estado de tramitación",
       subtitle = "1117 viviendas (764 en tramitación)",
       x = NULL,
       y = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb")

# Now with real location (not obfusqed)
ggplot() + 
  scale_fill_manual(values=cbPalette) +
  geom_point(data=vut[vut$tipo=="Viviendas de uso turístico",],aes(x=longitude, y=latitude,colour=estado,shape=tipo),
             alpha=0.5,size = 3)+
  geom_path(data=menores,aes(x=long, y=lat,group=group), colour="black",size = 0.1)+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  # theme_nothing(legend = TRUE) +
  # theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # coord_quickmap(xlim=c(-2.08, -1.92), ylim=c(43.2775,43.335)) + #todo donostia
  coord_quickmap(xlim=c(-2.02, -1.96), ylim=c(43.30,43.331)) + #zoom 
  # coord_fixed() +
  # xlim(-1.968,-1.94) +
  # ylim(43.305,43.325) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.9))) +
  labs(title = "Viviendas de uso turístico según estado de tramitación",
       subtitle = "1117 viviendas (764 en tramitación)",
       x = NULL,
       y = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb")

