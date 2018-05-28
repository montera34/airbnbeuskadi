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
vut <- read.delim("data/output/vut-donostia/censo-viviendas-turisticas-donostia-180301_barrio-umenor.csv",sep = ",")

# create taxonomy for estado
levels(vut$estado)
vut$estadox <- strapplyc( as.character(vut$estado), ".*_(.*)", simplify = TRUE)

# randomice lotation of points
vut$latr <- jitter(vut$latitude, factor=1, amount = 0.001)
vut$lonr <- jitter(vut$longitude, factor=1, amount = 0.001)

# ------------tablas por barrios-------
vut2 <- vut %>% 
  group_by(barrio,tipo,estadox) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

ggplot(vut2,aes(x = barrio, y = count,fill=estadox)) +
  geom_bar(stat="identity")+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) +
  labs(title = "Habitaciones y viviendas de uso turístico según estado de tramitación por barrio",
       subtitle = "",
       x = NULL,
       y = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb") +
  coord_flip()

ggplot(vut2[vut2$tipo=="Viviendas de uso turístico",],aes(x = barrio, y = count,fill=estadox)) +
  geom_bar(stat="identity")+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) +
  labs(title = "Viviendasde uso turístico según estado de tramitación por barrio",
       subtitle = "",
       x = NULL,
       y = NULL,
       caption = "Datos: Ayuntamiento de Donostia. Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
           
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
