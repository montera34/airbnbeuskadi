# VUT oficiales Donostia
# Este archivo genera mapas basados en los datos oficiales de viviendas y habitaciones turísticas
# según la web del Ayuntamiento de Donostia (descargado en data/output/vut-donostia/censo-viviendas-turisticas-donostia-180301_WGS84.csv).

# Load libraries
library(tidyverse)
# read shapes
library(rgdal)
library(ggmap) #for theme nothing
library(gsubfn) #for selecting estado

# Load barrios
barrios <- readOGR("data/barrios-donostia.geojson")
menores <- readOGR("data/original/shapes/unidades-menores-donostia.geojson")

# Load VUT
vut <- read.delim("data/output/vut-donostia/censo-viviendas-turisticas-donostia-180301_barrio-umenor.csv",sep = ",", encoding = "utf-8")

# Corrige nombre de unidad menor
levels(vut$umenores)[levels(vut$umenores)=="Sag�es"] <- "Sagües"

# create taxonomy for estado
levels(vut$estado)
vut$estadox <- strapplyc( as.character(vut$estado), ".*_(.*)", simplify = TRUE)

# add noise to location of points
vut$latr <- jitter(vut$latitude, factor=1, amount = 0.001)
vut$lonr <- jitter(vut$longitude, factor=1, amount = 0.001)

# ----- barras por barrios-------
vut2 <- vut %>%
  group_by(barrio,estadox) %>% 
  # group_by(barrio,umenores,tipo,estadox) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

png(filename="images/vut-barras-estado-tramitacion-barrio-2018.png",width = 900,height = 600)
ggplot(vut2,aes(x = reorder(barrio, suma), y = count, fill=estadox)) +
  geom_bar(stat="identity")+
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Viviendas de uso turístico según estado de tramitación por barrio",
       subtitle = "Donostia. Marzo 2018.",
       x = NULL,
       y = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()

vut2 <- vut %>% 
  group_by(barrio,tipo) %>%
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

png(filename="images/hab-viv-barras-barrio-vut-2018.png",width = 900,height = 600)
ggplot(vut2,aes(x = reorder(barrio, suma), y = count, fill=tipo)) +
  geom_bar(stat="identity")+
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Viviendas de uso turístico según habitación/apartamento por barrio",
       subtitle = "Donostia. Marzo 2018.",
       x = NULL,
       y = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()

# ----- barras por unidad menor-------

vut2 <- vut %>% 
  group_by(umenores,tipo,estadox) %>%
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

png(filename="images/viviendas-umenor-barras-vut-2018.png",width = 900,height = 1800)
ggplot(vut2[vut2$tipo=="Viviendas de uso turístico",],aes(x = reorder(umenores,suma), y = count,fill=estadox)) +
  geom_bar(stat="identity")+
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Viviendas de uso turístico según estado de tramitación por unidad menor",
       subtitle = "Donostia. Marzo 2018.",
       x = NULL,
       y = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()  

# ------------tablas por tipos de habitación-------
vut3 <- vut %>% 
  group_by(tipo,estadox) %>% 
  summarise(count=n()) %>%
  mutate(suma=sum(count))

png(filename="images/hab-viv-barras-vut-donostia-2018.png",width = 900,height = 600)
ggplot(vut3 ,aes(x = tipo, y = count,fill=estadox)) +
  geom_bar(stat="identity")+
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) +
  labs(title = "Habitaciones y viviendas (VUT) en Donostia.Estado tramitación",
       subtitle = "Marzo 2017",
       y = "número de viviendas",
       x = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()

# --------------- Mapas --------------------
ggplot() + 
  scale_fill_manual(values=cbPalette) +
  geom_point(data=vut,aes(x=lonr, y=latr,colour=estadox,shape=tipo),
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
  labs(title = "Habitaciones y viviendas de uso turístico según estado de tramitación",
     subtitle = "Un total de 1242 elementos. 1117 viviendas (764 en tramitación) y 125 habitaciones (73 en tramitación).",
     x = NULL,
     y = NULL,
     caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb")

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
