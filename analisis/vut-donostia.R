# VUT oficiales Donostia
# Este archivo genera mapas basados en los datos oficiales de viviendas y habitaciones turísticas
# según la web del Ayuntamiento de Donostia (descargado en data/output/vut-donostia/censo-viviendas-turisticas-donostia-180301_WGS84.csv).

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

# Load data

# Load VUT
# Select one file
# vut <- read.delim("data/output/vut-donostia/censo-viviendas-turisticas-donostia-180301_barrio-umenor.csv",sep = ",", encoding = "utf-8")
vut <- read.delim("data/output/vut-donostia/censo-viviendas-turisticas-donostia-20180914_barrio-umenor.csv",sep = ",", encoding = "utf-8")
# Select date
date <- "Septiembre 2018"
date_abr <- "20180914"

# Fix data> missing barrio and unicada menor for VUT (september 2018)
vut[27,]$barrio <- "Centro"
vut[27,]$umenores <- "Area Romantica"

# create taxonomy for estado
levels(vut$estado)
vut$estadox <- strapplyc( as.character(vut$estado), ".*_(.*)", simplify = TRUE)
# clean wrong character in accent
levels(vut$tipo) <- c("Habitaciones de uso turístico","Viviendas de uso turístico")

# add noise to location of points
vut$latr <- jitter(vut$latitude, factor=1, amount = 0.001)
vut$lonr <- jitter(vut$longitude, factor=1, amount = 0.001)
# Corrige nombre de unidad menor
levels(vut$umenores)[levels(vut$umenores)=="Sag�es"] <- "Sagües"
# check numbers for VUT
table(vut$estado)
table(vut$estadox)
table(vut$tipo)
table(vut$tipo,vut$estadox)

# Load viviendas por barrio
viviendas_barrios <-  read.delim("data/viviendas-barrios-donostia.csv",sep = ",")

# -------------colores ------------
cbPalette <- c("#017c0a","#f4830f")

# ----- barras por barrios-------
vut1 <- vut %>%
  group_by(barrio,estadox) %>% 
  # group_by(barrio,umenores,tipo,estadox) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

cbPalette1 <- c("#f4830f","#017c0a")

vut1$estadox <- factor(vut1$estadox,levels = c("Favorable","Tramitacion"))

png(filename=paste("images/vut-donostia/vut-barras-estado-tramitacion-barrio-",date_abr,".png",sep = ""),width = 900,height = 600)
ggplot(vut1,aes(x = reorder(barrio, suma), y = count, fill=factor(vut1$estadox, levels=c("Tramitacion","Favorable"))    )) +
  scale_fill_manual(values=cbPalette1) +
  geom_bar(stat="identity")+
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
  coord_flip()
dev.off()

vut1$percent <- round( vut1$count / nrow(vut) * 100, digits=1 )
vut1$percent_estado <- round( vut1$count / vut1$suma * 100, digits=1 )

vut2 <- vut %>% 
  group_by(barrio,tipo) %>%
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

png(filename=paste("images/vut-donostia/hab-viv-barras-barrio-vut-",date_abr,".png",sep = ""),width = 900,height = 600)
ggplot(vut2,aes(x = reorder(barrio, suma), y = count, fill=tipo)) +
  geom_bar(stat="identity")+
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
  coord_flip()
dev.off()

vut2$percent <- round( vut2$count / nrow(vut) * 100, digits=1 )
vut2$percent_tipo <- round( vut2$count / vut2$suma * 100, digits=1 )

# ----- barras por unidad menor-------

vut3 <- vut %>% 
  group_by(umenores,tipo,estadox) %>%
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

png(filename=paste("images/vut-donostia/viviendas-umenor-barras-vut-",date_abr,".png",sep = ""),width = 900,height = 1800)
ggplot(vut3[vut3$tipo=="Viviendas de uso turístico",],aes(x = reorder(umenores,suma), y = count,fill=estadox)) +
  geom_bar(stat="identity")+
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Viviendas de uso turístico según estado de tramitación por unidad menor",
       subtitle =  paste("Donostia. ",date,".",sep=""),
       x = NULL,
       y = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb") +
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
  group_by(tipo,estadox) %>% 
  summarise(count=n()) %>%
  mutate(suma=sum(count))

png(filename=paste("images/vut-donostia/hab-viv-barras-vut-donostia-",date_abr,".png",sep = ""),width = 900,height = 600)
ggplot(vut4 ,aes(x = tipo, y = count,fill=estadox)) +
  geom_bar(stat="identity")+
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) +
  labs(title = "Habitaciones y viviendas (VUT) en Donostia.Estado tramitación",
       subtitle =  paste("Donostia. ",date,".",sep=""),
       y = "número de viviendas",
       x = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()

# --------cálculo de ratio VUT por nº viviendas-------
barrios_vut <- vut %>%
  group_by(barrio) %>% 
  # group_by(barrio,umenores,tipo,estadox) %>% 
  summarise(vut=n()) %>% 
  # mutate(suma=sum(count)) %>%
  arrange(-vut)

barrios_vut <- merge(barrios_vut,viviendas_barrios,by.x="barrio",by.y="barrios")
barrios_vut$ratio_vut <- round(barrios_vut$vut / barrios_vut$Total.Viviendas.familiares *100,digits=2)
# barrios_vut$ratio_airbnb <- round(barrios_vut$count_vals / barrios_vut$Total.Viviendas.familiares *100,digits=2)

write.csv(barrios_vut[,-c(3)], file = "data/output/vut-donostia/por-barrios-censo-viviendas-turisticas-donostia-180914.csv", row.names = FALSE)

png(filename=paste("images/vut-donostia/ratio-vut-barras-donostia-barrio-",date_abr,".png",sep = ""),width = 900,height = 600)
ggplot(barrios_vut,aes(x = reorder(barrio, ratio_vut), y = ratio_vut)) +
  geom_bar(stat="identity")+
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
  coord_flip()
dev.off()

# --------------- Mapas --------------------

cbPalette <- c("#017c0a","#f4830f")

png(filename=paste("images/vut-donostia/map-vut-tramitacion-donostia-",date_abr,".png",sep = ""),width = 1200,height = 900)
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
  geom_point(data=vut,aes(x=longitude, y=latitude,color=estadox,shape=tipo),
             alpha=0.9,size = 2)+
  # geom_point(data=vut,aes(x=lonr, y=latr,colour=estadox,shape=tipo),
             # alpha=0.9,size = 2)+
  # barrios o unidades menores
  geom_path(data=barrios,aes(x=long, y=lat,group=group), colour="black",size = 0.1)+
  geom_path(data=barrios[barrios@data$BAR_DS_O == "Centro" | barrios@data$BAR_DS_O == "Gros",],
            aes(x=long, y=lat,group=group),
            colour="black",size = 0.7)+
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
     # subtitle = "Un total de xxx elementos. xxx viviendas (xxx en tramitación) y xxx habitaciones (xx en tramitación).",
     subtitle =  paste("Donostia. ",date,".",sep=""),
     x = NULL,
     y = NULL,
     caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb")
  dev.off()


ggplot() + 
  scale_fill_manual(values=cbPalette) +
  geom_point(data=vut[vut$tipo=="Habitaciones de uso turístico",],aes(x=lonr, y=latr,colour=estadox,shape=tipo),
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
  geom_point(data=vut[vut$tipo=="Viviendas de uso turístico",],aes(x=lonr, y=latr,colour=estadox,shape=tipo),
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
  geom_point(data=vut[vut$tipo=="Viviendas de uso turístico",],aes(x=longitude, y=latitude,colour=estadox,shape=tipo),
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
