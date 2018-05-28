# Habitaciones vs Viviendas completas en Airbnb Donostia

# Load libraries
library(tidyverse)
# read shapes
library(rgdal)
library(ggmap) #for theme nothing

# Load barrios
barrios <- readOGR("data/barrios-donostia.geojson")
menores <- readOGR("data/original/shapes/unidades-menores-donostia.geojson")
mar <- readOGR("data/original/shapes/mar-donostia.geojson")

# Load points: Airbnb listings
load("data/output/180423_listings-airbnb-donostia_datahippo_barrio-umenor.Rda")

airbnb <- transform(airbnb,barrio=unlist(barrio))
airbnb <- transform(airbnb,umenores=unlist(umenores))


table(airbnb$room_type)

# ------------tablas por barrios-------
airbnb2 <- airbnb %>% 
  group_by(barrio,umenores,room_type) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

ggplot(airbnb2,aes(x = barrio, y = count,fill=room_type)) +
  geom_bar(stat="identity")+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) +
  labs(title = "Habitaciones y viviendas de Airbnb por barrios en Donostia.",
       subtitle = "Abril  2018",
       x = "número de anuncios",
       y = NULL,
       caption = "Datos: datahippo.org. Gráfico: lab.montera34.com/airbnb") +
  coord_flip()

# -----------Mapa---------------
ggplot() + 
  scale_fill_manual(values=cbPalette) +
  geom_point(data=airbnb,aes(x=longitude, y=latitude,colour=room_type),
             alpha=0.5,size = 1)+
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
       subtitle = "Un total de 1869 anuncios, 1.390 apartamentos comleto, 471 habitaciones y 8 hab. compartidas.",
       x = NULL,
       y = NULL,
       caption = "Datos: datahippo.org. Gráfico: lab.montera34.com/airbnb")
