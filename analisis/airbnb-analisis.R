# Análisis de Airbnb en Donostia
# -Habitaciones vs Viviendas completas
# -numero anuncios por barrio
# -ratio anuncios por barrio por vivienda
# -numero plazas
# -ratio plazas por habitante

# ---- Load libraries -----
library(tidyverse)
# read shapes
library(rgdal)
library(ggmap) #for theme nothing
library(reshape)
library(gridExtra)

# ------ Load files ----------
# Load shapes
barrios <- readOGR("data/barrios-donostia_simplificado.geojson")
menores <- readOGR("data/output/limites/unidades-menores-donostia_cleaned-merged.geojson")
mar <- readOGR("data/original/shapes/mar-donostia.geojson")

viviendas_barrios <- read.delim("data/viviendas-barrios-donostia.csv",sep = ",")
habitantes <- read.delim("data/original/donostia/habitantes-por-barrio-2017.csv",sep = ",")

# Load Airbnb points: 
# Airbnb listings 2017
airbnb2017 <- read.delim("data/listings_donostia_simple.csv",sep = ",")
# Translate room type
levels(airbnb2017$room_type) <- c("Vivienda completa","Habitación privada","Habitación compartida")

# Airbnb listings 2017 merged
airbnb2017merged <- read.delim("data/output/170400_listings-airbnb-donostia_insideairbnb-datahippo_barrio-umenor.csv",sep = ",")
# Translate room type
levels(airbnb2017merged$room_type) <- c("Vivienda completa","Habitación privada","Habitación compartida")

# Airbnb listings 2018
# load("data/output/180423_listings-airbnb-donostia_datahippo_barrio-umenor.Rda")
airbnb2018 <- read.delim("data/output/180423_listings-airbnb-donostia_datahippo_barrio-umenor.csv",sep = ",")
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

# -----------barras por tipos de habitación-------
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

# -----------barras por barrios-------
airbnb2 <- airbnb2017merged  %>% 
  group_by(barrio,room_type) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

# TODO: hay varios anuncios clasificados como barrio "NA"
png(filename="images/hab-viv-barras-airbnb-barrios-donostia-2017.png",width = 900,height = 700)
ggplot(airbnb2,aes(x = reorder(barrio,suma), y = count,fill=room_type)) +
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

airbnb3 <- airbnb2018 %>% 
  group_by(barrio,room_type) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  arrange(-count)

png(filename="images/hab-viv-barras-airbnb-barrios-donostia-2018.png",width = 900,height = 700)
ggplot(airbnb3,aes(x = reorder(barrio,suma), y = count,fill=room_type)) +
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

airbnb2$name_complete <- paste(airbnb2$barrio,airbnb2$room_type,sep = "_")
airbnb3$name_complete <- paste(airbnb3$barrio,airbnb3$room_type,sep = "_")

airbnb_20172018 <- merge(airbnb2,airbnb3, by="name_complete" )

airbnb_20172018$evolution <- round(airbnb_20172018$count.y / airbnb_20172018$count.x * 100,digits=2)
airbnb_20172018$dif <- airbnb_20172018$count.y - airbnb_20172018$count.x 

# slope graph based on https://acaird.github.io/computers/r/2013/11/27/slopegraphs-ggplot
# create list of labels
l2017<-paste(airbnb_20172018$count.x,airbnb_20172018$barrio.x,sep="-")
l2018<-paste(airbnb_20172018$count.y,airbnb_20172018$barrio.x,sep="-")
ldif <-paste( airbnb_20172018$barrio, airbnb_20172018$dif,sep=" ")

# slope graph evolution 2017-2018 per barrio
ggplot(airbnb_20172018) +
  geom_segment(aes(x=0,xend=12,y=count.x,yend=count.y),size=.5) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(),
    axis.ticks=element_blank(),
    axis.text=element_blank(),
    legend.position="top"
  ) +
  labs(title = "Evolución por barrios de habitaciones y viviendas de Airbnb en Donostia.",
     subtitle = "Evolución 2017-2018",
     y = "número de anuncios",
     x = NULL,
     caption = "Datos: datahippo.org. Gráfico: lab.montera34.com/airbnb") +
  # geom_text(label=l2017, y=airbnb_20172018$count.x, x=rep.int(0,nrow(airbnb_20172018)), hjust=1.2,size=3.5) +
  # geom_text(label=l2018, y=airbnb_20172018$count.y, x=rep.int(12,nrow(airbnb_20172018)), hjust=-0.2,size=3.5) +
  geom_text(label=ldif, y=airbnb_20172018$count.y-((airbnb_20172018$count.y-airbnb_20172018$count.x)/2), 
            x=rep.int(6,nrow(airbnb_20172018)), hjust=1,vjust=-1,size=3.5) +
  geom_text(label="2017", x=0, y=(1.02*(max(airbnb_20172018$count.x,airbnb_20172018$count.y))),hjust= 0,size=3) +
  geom_text(label="2018", x=12, y=(1.02*(max(airbnb_20172018$count.x,airbnb_20172018$count.y))),hjust= 1,size=3)

# -----------barras por unidades menores-------
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

# ----- Prepara datos por barrios -------
airbnb_barrio <- data.frame(table(airbnb2017merged$barrio))
names(airbnb_barrio) <- c("barrio","airbnb_2017")
airbnb_barrio2 <- data.frame(table(airbnb2018$barrio))
names(airbnb_barrio2) <- c("barrio","airbnb_2018")

# Merge data in single data frame
por_barrios <- merge(viviendas_barrios,airbnb_barrio,by.x="barrios",by.y="barrio")
por_barrios <- merge(por_barrios,airbnb_barrio2,by.x="barrios",by.y="barrio")

# ----- Ratio Airbnb por barrios: barras -------
por_barrios$ratio2017 <- round(por_barrios$airbnb_2017 / por_barrios$Total.Viviendas.familiares *100,digits = 2)
por_barrios$ratio2018 <- round(por_barrios$airbnb_2018 / por_barrios$Total.Viviendas.familiares *100,digits = 2)

# provides position for reordering
por_barrios$pos_airbnb_2018 <- 1
por_barrios[ order(-por_barrios[,6]), ]$pos_airbnb_2018 <- 1:17
por_barrios$pos_ratio2018 <- 1
por_barrios[ order(-por_barrios[,8]), ]$pos_ratio2018 <- 1:17

# ----- Cantidad Airbnb por barrios: barras -------
# reshape into long format
p <- reshape(por_barrios[,-c(2:4,7,8)], direction = "long", varying = list(names(por_barrios[,-c(2:4,7)])[2:3]), v.names = "Value", 
             idvar = c("barrios"), timevar = "Year", times = c(2017:2018))
# creates factor for year
p$Year <- as.factor(p$Year)
# Plots in bars
png(filename="images/airbnb/n-anuncios-airbnb-barrios-donostia-2017-2018_order-ratio2018.png",width = 450,height = 700)
ggplot(p,aes(x = reorder(barrios, Value), y = Value)) + #order by Value or by ratio2018
  geom_bar(aes(fill = Year), position = "dodge", stat="identity")+
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) +
  labs(title = "Presencia de Airbnb en barrios: 2017 y 2018",
       subtitle = "Cantidad de anuncios de Airbnb en Donostia",
       y = "nº anuncios Airbnb",
       x = NULL,
       caption = "Datos: Insideairbnb (marzo 2017) y Datahippo (abril 2017, abril 2018). Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()

# reshape into long format
n <- reshape(por_barrios[,-c(2:6)], direction = "long", varying = list(names(por_barrios[,-c(2:6)])[2:3]), v.names = "Value", 
             idvar = c("barrios"), timevar = "Year", times = c(2017:2018))

# creates factor for year
n$Year <- as.factor(n$Year)
# Plots in bars
png(filename="images/airbnb/ratio-airbnb-barrios-donostia-2017-2018.png",width = 450,height = 700)
ggplot(n,aes(x = reorder(barrios, -pos_ratio2018), y = Value)) +
  geom_bar(aes(fill = Year), position = "dodge", stat="identity")+
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) + 
  labs(title = "Presencia de Airbnb en barrios: 2017 y 2018",
       subtitle = "Ratio de anuncios de Airbnb por cada 100 viviendas en Donostia",
       y = "ratio anuncios Airbnb / 100 viviendas",
       x = NULL,
       caption = "Datos: Insideairbnb (marzo 2017) y Datahippo (abril 2017, abril 2018). Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()

# -------- Gráfico mariposa anuncios --------
# inspired/copied from https://github.com/meneos/R_Dataviz/blob/master/RENTABILIDAD%20INMUEBLES%20MADRID/rentabilidad_distritos.R
# library(gridExtra)
plot1 <- ggplot(n,aes(x = reorder(barrios, -pos_ratio2018), y = Value)) +
  geom_bar(aes(fill = Year), position = "dodge", stat="identity")+
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.title.x = element_text(margin = margin(20,0,0,0)),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.2),
        plot.caption = element_text(margin = margin(20,0,0,0)), 
        axis.text.y = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="bottom") +
labs(title = "",
     subtitle = "Anuncios de Airbnb por cada 100 viviendas",
     y = "ratio anuncios Airbnb / 100 viviendas",
     x = NULL,
     caption = "Datos: Insideairbnb (marzo 2017) y Datahippo (abril 2017, abril 2018). Gráfico: lab.montera34.com/airbnb") +
  coord_flip()

plot2 <- ggplot(p,aes(x = reorder(barrios, -pos_ratio2018), y = Value)) + #order by Value or by ratio2018
  geom_bar(aes(fill = Year), position = "dodge", stat="identity")+
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    axis.title.x = element_text(margin = margin(20,0,0,0)),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(hjust = 0.2),
    plot.caption = element_text(margin = margin(20,0,0,0)), 
    # axis.text.y = element_text(hjust = 0.5),
    axis.text.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="bottom",
    plot.margin = unit(c(0.3,0,0.4,4), "cm")
  ) +
  scale_y_reverse() + #invert axis
  labs(title = "Presencia de Airbnb en barrios de Donostia por nº de anuncios",
       subtitle = "Número de anuncios de Airbnb",
       y = "nº anuncios Airbnb",
       x = NULL,
       caption = "") +
  coord_flip()

png(filename="images/airbnb/barras-mariposa-n-y-ratio-airbnb-barrios-donostia-2017-2018.png",width = 900,height = 800)
grid.arrange(plot2,plot1,ncol=2)
dev.off()

# Extra calculo diferencia y evolucion
por_barrios$dif <- por_barrios$ratio2018 - por_barrios$ratio2017
por_barrios$evol <- round((por_barrios$ratio2018 - por_barrios$ratio2017)/por_barrios$ratio2017 *100,digits = 2)

# ----- Calculo cantidad de plazas de Airbnb por barrios -----
plazas_2017 <- airbnb2017merged %>% 
  group_by(barrio) %>% 
  summarise(plazas2017 = sum(accommodates))
plazas_2018 <- airbnb2018 %>% 
  group_by(barrio) %>% 
  summarise(plazas2018 = sum(capacity))

plazas <- merge(plazas_2017,plazas_2018,by="barrio")
plazas <- merge(plazas,habitantes,by="barrio")

plazas$dif <- plazas$plazas2018 - plazas$plazas2017
plazas$evol <- round((plazas$plazas2018 - plazas$plazas2017)/plazas$plazas2017 *100,digits=2)

plazas$ratio2017 <- round(plazas$plazas2017 / plazas$habitantes*100,digits=2)
plazas$ratio2018 <- round(plazas$plazas2018 / plazas$habitantes*100,digits=2)

# provides position for reordering
plazas$pos_plazas2018 <- 1
plazas[ order(-plazas[,3]), ]$pos_plazas2018 <- 1:17
plazas$pos_ratio2018 <- 1
plazas[ order(-plazas[,8]), ]$pos_ratio2018 <- 1:17

# ----- Barras numero plazas por barrio ----------
q <- reshape(plazas[,c(1:3,9:10)], direction = "long", varying = list(names(plazas[,c(1:3)])[2:3]), v.names = "Value", 
             idvar = c("barrio"), timevar = "Year", times = c(2017:2018))

# creates factor for year
q$Year <- as.factor(q$Year)

png(filename="images/airbnb/plazas-airbnb-barrios-donostia-2017-2018.png",width = 450,height = 700)
ggplot(q,aes(x = reorder(barrio, -pos_plazas2018), y = Value)) +
  geom_bar(aes(fill = Year), position = "dodge", stat="identity")+
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) + 
  labs(title = "Presencia de Airbnb en barrios: 2017 y 2018",
       subtitle = "Plazas anunciadas en Airbnb en Donostia por barrio",
       y = "nº de plazas ofertadas",
       x = NULL,
       caption = "Datos: Insideairbnb (marzo 2017) y Datahippo (abril 2017, abril 2018). Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()

# ----- Barras ratio plazas Airbnb por barrio ----------
r <- reshape(plazas[,c(1,7:8,10)], direction = "long", varying = list(names(plazas[,c(1,7:8)])[2:3]), v.names = c("Value"), 
             idvar = c("barrio"), timevar = "Year", times = c(2017:2018))

# creates factor for year
r$Year <- as.factor(r$Year)

png(filename="images/airbnb/ratio-plazas-airbnb-barrios-donostia-2017-2018.png",width = 450,height = 700)
ggplot(r,aes(x = reorder(barrio, -pos_ratio2018), y = Value)) +
  geom_bar(aes(fill = Year), position = "dodge", stat="identity")+
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) + 
  labs(title = "Presencia de Airbnb en barrios: 2017 y 2018",
       subtitle = "Ratio de de plazas anunciadas en Airbnb por cada 100 habitantes en Donostia por barrio",
       y = "plazas anunciadas por cada 100 habitantes",
       x = NULL,
       caption = "Datos: Insideairbnb (marzo 2017) y Datahippo (abril 2017, abril 2018). Gráfico: lab.montera34.com/airbnb") +
  coord_flip()
dev.off()

# -------- Gráfico mariposa plazas--------
# inspired/copied from https://github.com/meneos/R_Dataviz/blob/master/RENTABILIDAD%20INMUEBLES%20MADRID/rentabilidad_distritos.R
# library(gridExtra)
plot1 <- ggplot(r,aes(x = reorder(barrio,  -pos_ratio2018), y = Value)) +
  geom_bar(aes(fill = Year), position = "dodge", stat="identity")+
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.title.x = element_text(margin = margin(20,0,0,0)),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.2),
        plot.caption = element_text(margin = margin(20,0,0,0)), 
        axis.text.y = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="bottom") +
  labs(title = "",
       subtitle = "Plazas de Airbnb por cada 100 habitantes",
       y = "plazas / 100 habitantes",
       x = NULL,
       caption = "Datos: Insideairbnb (marzo 2017) y Datahippo (abril 2017, abril 2018). Gráfico: lab.montera34.com/airbnb") +
  coord_flip()

plot2 <- ggplot(q,aes(x = reorder(barrio, -pos_ratio2018), y = Value)) + #order by Value or by ratio2018
  geom_bar(aes(fill = Year), position = "dodge", stat="identity")+
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    axis.title.x = element_text(margin = margin(20,0,0,0)),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(hjust = 0.2),
    plot.caption = element_text(margin = margin(20,0,0,0)), 
    # axis.text.y = element_text(hjust = 0.5),
    axis.text.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="bottom",
    plot.margin = unit(c(0.3,0,0.4,4), "cm")
  ) +
  scale_y_reverse() + #invert axis
  labs(title = "Presencia de Airbnb en barrios de Donostia por nº de plazas",
       subtitle = "Número de plazas de Airbnb",
       y = "nº plazas Airbnb",
       x = NULL,
       caption = "") +
  coord_flip()

png(filename="images/airbnb/barras-mariposa-n-y-ratio-plazas-airbnb-barrios-donostia-2017-2018.png",width = 900,height = 800)
grid.arrange(plot2,plot1,ncol=2)
dev.off()

# -------- Comparativa Airbnb, VUT e Idealista ------ 

vut <- read.delim(file="data/output/vut-donostia/por-barrios-censo-viviendas-turisticas-donostia-180301.csv", sep=",")
idealista_barrios <- read.delim(file="data/output/idealista/por-barrios-idealista-renta-precio-m2-donostia-2012-2017.csv", sep = ",")

barrios_compara <- por_barrios

names(barrios_compara) <- c("barrios","total","Total.Viviendas.familiares","Total.establecimientos.colectivos",
                        "airbnb_2017_count","airbnb_2018_count","airbnb_ratio_2017","airbnb_ratio_2018","pos_airbnb_2018",
                        "pos_ratio_aribnb_2018","airbnb_dif_17_18","airbnb_evol_17_18")

barrios_compara <- merge(barrios_compara[,-(2:4)],vut,by.x="barrios",by.y="barrio")

plazas_temp <- plazas
names(plazas_temp) <- c("barrio","airbnb_plazas_2017","airbnb_plazas_2018","habitantes","airbnb_dif_17_18","airbnb_evol_17_18",
                   "airbnb_ratio_plazas_2017","airbnb_ratio_plazas_2018","pos_airbnb_plazas_2018","pos_airbnb_ratio_plazas_2018")
barrios_compara <- merge(barrios_compara,plazas_temp,by.x="barrios",by.y="barrio")

names(idealista_barrios) <- c("barrio","idealista_m2_2012","idealista_m2_2013","idealista_m2_2014","idealista_m2_2015","idealista_m2_2016",
                              "idealista_m2_2017","idealista_m2_dif_13_17","idealista_evol_13_17","idealista_evol_16_17")
barrios_compara <- merge(barrios_compara,idealista_barrios,by.x="barrios",by.y="barrio")
