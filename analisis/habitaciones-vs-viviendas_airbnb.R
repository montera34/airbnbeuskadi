# Habitaciones vs Viviendas completas en Airbnb Donostia


# ---- Load libraries -----
library(tidyverse)
# read shapes
library(rgdal)
library(ggmap) #for theme nothing
library(reshape)


# ------ Load files ----------
# Load shapes
barrios <- readOGR("data/barrios-donostia.geojson")
menores <- readOGR("data/original/shapes/unidades-menores-donostia.geojson")
mar <- readOGR("data/original/shapes/mar-donostia.geojson")

# Load points: 
# Airbnb listings 2017
airbnb2017 <- read.delim("data/listings_donostia_simple.csv",sep = ",")
# Translate room type
levels(airbnb2017$room_type) <- c("Vivienda completa","Habitación privada","Habitación compartida")

# Airbnb listings 2017 merged
airbnb2017merged <- read.delim("data/output/170400_listings-airbnb-donostia_insideairbnb-datahippo_barrio-umenor.csv",sep = ",")
# Translate room type
levels(airbnb2017merged$room_type) <- c("Vivienda completa","Habitación privada","Habitación compartida")

# Airbnb listings 2018
load("data/output/180423_listings-airbnb-donostia_datahippo_barrio-umenor.Rda")
airbnb2018 <- airbnb
# Translate room type
levels(airbnb2018$room_type) <- c("Vivienda completa","Habitación privada","Habitación compartida")

# ----- Prepares data -------

# Fixes barrios and unidades menores
airbnb2018 <- transform(airbnb2018,barrio=unlist(barrio))
airbnb2018 <- transform(airbnb2018,umenores=unlist(umenores))

table(airbnb2017$room_type)
table(airbnb2017merged$room_type)
table(airbnb2018$room_type)

# ----- Comparative room types -----------
# Compare two data sets by number of room types
# compare_room <- merge(data.frame(table(airbnb2018$room_type)),data.frame(table(airbnb2017merged$room_type)),by="Var1")
# compare_room <- merge(compare_room,data.frame(table(airbnb2017$room_type)),by="Var1")
compare_room <- merge(data.frame(table(airbnb2017merged$room_type)),data.frame(table(airbnb2018$room_type)),by="Var1")
colnames(compare_room) <- c("tipo_habitacion","2017","2018")

# reshape data to long format to prepare to plot bar chart year comparison
m <- reshape(compare_room, direction = "long", varying = list(names(compare_room)[2:3]), v.names = "Value", 
             idvar = c("tipo_habitacion"), timevar = "Year", times = 2017:2018)

ggplot(m,aes(x = tipo_habitacion, y = Value)) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_wrap(~Year)

m$Year <- as.factor(m$Year)
png(filename="images/hab-viv-barras-airbnb-donostia-2017-2018.png",width = 600,height = 400)
  ggplot(m,aes(x = tipo_habitacion, y = Value)) +
    geom_bar(aes(fill = Year), position = "dodge", stat="identity")+
    coord_flip() +
    theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
    theme(
      panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
    ) +
    labs(title = "Habitaciones y viviendas de Airbnb en Donostia.",
         subtitle = "Años 2017 y 2018",
         y = "nº de anuncios",
         x = NULL,
         caption = "Datos: Insideairbnb (marzo 2017) y Datahippo (abril 2017, abril 2018). Gráfico: lab.montera34.com/airbnb") +
    coord_flip()
dev.off()

# ------------barras por tipos de habitación-------
airbnb1_2017 <- airbnb2017 %>% 
  group_by(room_type) %>% 
  summarise(count=n()) 

png(filename="images/hab-viv-barras-airbnb-donostia-2017.png",width = 600,height = 400)
ggplot(airbnb1_2017,aes(x = room_type, y = count)) +
  geom_bar(stat="identity")+
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) +
  labs(title = "Habitaciones y viviendas de Airbnb en Donostia.",
       subtitle = "Marzo 2017",
       y = "número de anuncios",
       x = NULL,
       caption = "Datos: Insideairbnb. Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()

airbnb1 <- airbnb2018 %>% 
  group_by(room_type) %>% 
  summarise(count=n())

png(filename="images/hab-viv-barras-airbnb-donostia-2018.png",width = 900,height = 600)
ggplot(airbnb1,aes(x = room_type, y = count)) +
  geom_bar(stat="identity")+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) +
  labs(title = "Habitaciones y viviendas de Airbnb en Donostia.",
       subtitle = "Abril  2018",
       x = "número de anuncios",
       y = NULL,
       caption = "Datos: datahippo.org. Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()

# ------------barras por barrios-------
airbnb3 <- airbnb2017merged  %>% 
  group_by(barrio,room_type) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)
# TODO: hay varios anuncios clasificados como barrio "NA"
png(filename="images/hab-viv-barras-airbnb-barrios-donostia-2017.png",width = 900,height = 700)
ggplot(airbnb3,aes(x = reorder(barrio,suma), y = count,fill=room_type)) +
  geom_bar(stat="identity")+
  ylim(c(0,800))+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Habitaciones y viviendas de Airbnb por barrios en Donostia.",
       subtitle = "Abril 2017",
       y = "número de anuncios",
       x = NULL,
       caption = "Datos: Insideairbnb y datahippo.org. Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()

airbnb2 <- airbnb2018 %>% 
  group_by(barrio,room_type) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

png(filename="images/hab-viv-barras-airbnb-barrios-donostia-2018.png",width = 900,height = 700)
ggplot(airbnb2,aes(x = reorder(barrio,suma), y = count,fill=room_type)) +
  geom_bar(stat="identity")+
  ylim(c(0,800))+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Habitaciones y viviendas de Airbnb por barrios en Donostia.",
       subtitle = "Abril 2018",
       y = "número de anuncios",
       x = NULL,
       caption = "Datos: datahippo.org. Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()

# ------------barras por unidades menores-------
airbnb3 <- airbnb2017merged  %>% 
  group_by(umenores,room_type) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)
# TODO: hay varios anuncios clasificados como barrio "NA"
png(filename="images/hab-viv-barras-airbnb-umenores-donostia-2017.png",width = 900,height = 1400)
ggplot(airbnb3,aes(x = reorder(umenores,suma), y = count,fill=room_type)) +
  geom_bar(stat="identity")+
  ylim(c(0,600))+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Habitaciones y viviendas de Airbnb por unidad menor en Donostia.",
       subtitle = "Abril 2017",
       y = "número de anuncios",
       x = NULL,
       caption = "Datos: Insideairbnb y datahippo.org. Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()

airbnb2 <- airbnb2018 %>% 
  group_by(umenores,room_type) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

png(filename="images/hab-viv-barras-airbnb-umenores-donostia-2018.png",width = 900,height = 1400)
ggplot(airbnb2,aes(x = reorder(umenores,suma), y = count,fill=room_type)) +
  geom_bar(stat="identity")+
  ylim(c(0,600))+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Habitaciones y viviendas de Airbnb por unidad menor en Donostia.",
       subtitle = "Abril 2018",
       y = "número de anuncios",
       x = NULL,
       caption = "Datos: datahippo.org. Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()

airbnb2$percent <-round(100* airbnb2$count / airbnb2$suma,digits = 1)

airbnb2 <- airbnb2018 %>% 
  group_by(umenores,room_type) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

table() %>%
  arrange(desc(habitantes)) %>%
  knitr::kable("html") %>%
  kable_styling("striped", full_width = F, font_size = 10)

# -----------Mapa---------------
png(filename="images/hab-viv-mapa-airbnb-menores-donostia-2018.png",width = 800,height = 600)
ggplot() + 
  scale_fill_manual(values=cbPalette) +
  geom_point(data=airbnb2018,aes(x=longitude, y=latitude,colour=room_type),
             alpha=0.5,size = 1.5)+
  geom_path(data=menores,aes(x=long, y=lat,group=group), colour="black",size = 0.1)+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  # theme_nothing(legend = TRUE) +
  # theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position="top",
        axis.text = element_blank()) +
  # coord_quickmap(xlim=c(-2.08, -1.92), ylim=c(43.2775,43.335)) + #todo donostia
  coord_quickmap(xlim=c(-2.02, -1.96), ylim=c(43.30,43.331)) + #zoom 
  # coord_fixed() +
  # xlim(-1.968,-1.94) +
  # ylim(43.305,43.325) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.9))) +
  labs(title = "Anuncios de Airbnb según estado de tipo (abril 2018)",
       subtitle = "Un total de 1869 anuncios: 1.390 apartamentos, 471 habitaciones y 8 habitaciones compartidas.",
       x = NULL,
       y = NULL,
       caption = "Datos: datahippo.org. Gráfico: lab.montera34.com/airbnb")
dev.off()
