# evolución
# Este script analiza la evolución de listins de una localización con datos de InsideAirbnb
# Usa los archivos listings.csv.gz y no del listings-summary de Insideairbnb porque en el summary no vien si tiene licencia o no.
# Si no necesitas los gráficos con la licencia puedes usar el summary (archivos menos pesados)
# Los archivos han sido obenidos con el script scraping/get-insideairbnb-data.R

# Para el caso de Euskadi, los datos de Airbnb no tienen definido el distrito o barrio, sino provincia y municipio.
# Por eso se ha duplicado este archivo: 
# - aquí para analizar en Euskadi
# - en evolucion-donostia.R para analizar Donostia
# Para analizar por barrios y unidades menores en Donostia se ha detectado en cual está cada punto con el script points-in-polygons.R

# 0. Load settings ------
local_name <- "Donostia - San Sebastián"
local_basic <- "donostia"
period <- "03/2017 - 06/2019"
caption <- "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb"

# 1. Load libraries ----
library(gsubfn)
library(tidyverse)
# extends color paletter
library(RColorBrewer)
library("reshape2")
library(ggthemes) #install ggthemes
library(ggrepel) # for geom_text_repel to prevent overlapping

# Puntos 2. y 3. en evolucion.R

data_long <- data_long_donostia

# 4. Plots ------------------------------------------------------------------------------------------------------------------------

# 4.1 General numbers
# A listings count per scraping date ----------------------------------------------------------------------------
# Lines
dates.count <- data_long %>% group_by(fechab) %>% summarise(anuncios=n())
# creates fake df
df <- data.frame(x1 = 1, x2 = 1, y1 = 3, y2 = 5)
# plot
png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes-linea.png", sep = ""),width = 1000,height = 400)
dates.count  %>%
  ggplot(aes(fechab,anuncios)) + 
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # verano
  annotate(geom = "text", x = as.Date("2018-08-06"), y = 750, label = "Verano", 
           family = "Roboto Condensed", hjust = 0.5, size=5, color="#888888") +
  geom_line(size=1.5) +
  # geom_line(aes(fechab,anuncios))
  geom_point(size=2.5,color="#BB3300") +
  # add number in last date
  geom_text(data=filter(dates.count,fechab > max(fechab-1)),
            aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  # annotate("text",x=as.Date("2018-05-28"),y=4000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE),limits = c(0, max(dates.count$anuncios))) +
  # geom_text(aes(label=anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de anuncios de Airbnb en cada descarga de datos de InsideAirbnb",
       subtitle = paste(local_name," ",period,sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption) +
  # nota
  annotate(geom = "text", x = as.Date("2017-06-1"), y = 11500, label = "Cada punto es un scraping de InsideAirbnb", 
           family = "Roboto Condensed", hjust = 1,size=6) +
  # annotate(geom = "segment", x = as.Date("2017-01-1"), xend = as.Date("2017-04-1"), y = 12000, yend = 17200,
  # color="#999999") +
  geom_curve(aes(x = as.Date("2017-06-01"), y = 11500, xend = as.Date("2018-02-01"), yend = 18200), 
             color="#999999", data =df,  curvature = 0.2)
dev.off()

# Barras
png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes.png", sep = ""),width = 1000,height = 300)
data_long %>%
  ggplot(aes(fechab)) + 
  geom_bar() +
  # annotate("text",x=as.Date("2018-05-15"),y=6000,label="acuerdo",color="#000000",size=4,family = "Roboto Condensed") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # geom_text(aes(label=anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
    # axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "fecha",
       caption = caption)
dev.off()

# B.0 With license -------------------------------------
# Initial test looking for repeated license numbers #TODO
# prueba <- data_long[data_long$fechab=="2019-03-08",]
# prueba$license <- factor(prueba$license)
# pruebalicense <- as.data.frame(table(prueba$license)) #TODO
# plot(table(prueba$license))
# save.image()
# write.csv(pruebalicense, file = "tmp/pruebalicense.csv", row.names = FALSE)
# table(pruebalicense$Freq)

# B.1 With license---------------------------------

# paletter green red
# semaforo <- c("#458b00","orange","#c80100")
semaforo <- c("#458b00","#c80100")

haslicense <- data_long %>% group_by(fechab,has.license) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes-linea-license.png", sep = ""),width = 800,height = 350)
ggplot(NULL) + 
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  geom_text_repel(data=filter(haslicense, fechab > maxdate-1),
                  aes(fechab,anuncios,
                      label=paste(format(anuncios, nsmall=1, big.mark="."), " ", has.license , sep = "")),
                  # nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y"
  ) +
  geom_line(data=haslicense,
            aes(fechab,anuncios,color=has.license),size=1.5) +
  geom_point(data=haslicense,size=2.5, aes(fechab,anuncios,color=has.license)) +
  scale_color_manual(values=semaforo) +
  # annotate("text",x=as.Date("2018-05-28"),y=4000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_y_continuous(limits=c(0, max(haslicense$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  # geom_text(data=filter(dates.count,fechab > as.Date("2019-03-01")),
  #           aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios publicados según licencia",
       subtitle = paste("Airbnb. ", local_name," ",period,sep=""),
       y = "número de anuncios",
       x = "",
       caption = "caption",
       color = ""
       ) 
dev.off()

# B.2 With license and room type---------------------------------
haslicense.roomtype <- data_long %>% group_by(fechab,has.license,room_type.s) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes-linea-license-alojamiento.png", sep = ""),width = 1000,height = 400)
ggplot(NULL) + 
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  geom_point(data=haslicense.roomtype,size=2.5, aes(fechab,anuncios,color=has.license)) +
  geom_line(data=haslicense.roomtype,
            aes(fechab,anuncios,color=has.license),size=1.5) +
  scale_color_manual(values=semaforo) +
  annotate("text",x=as.Date("2018-05-28"),y=7000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  scale_y_continuous(limits=c(0, max(haslicense.roomtype$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  # geom_text(data=filter(dates.count,fechab > as.Date("2019-03-01")),
  #           aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  geom_text_repel(data=filter(haslicense.roomtype, fechab > maxdate-1),
                  aes(fechab+10,anuncios,label=paste(format(anuncios, nsmall=1, big.mark=".") , sep = "")), 
                  # nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y"
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios publicados según licencia por tipo de alojamiento",
       subtitle = paste("Airbnb. ", local_name," ",period,sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption,
       color = "" ) +
  facet_wrap(~room_type.s)
dev.off()

# B.3 With license and host type---------------------------------
haslicense.hosttype <- data_long %>% group_by(fechab,has.license,host.type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes-linea-license-host.png", sep = ""),width = 1000,height = 400)
ggplot(NULL) + 
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  geom_point(data=haslicense.hosttype,size=2.5, aes(fechab,anuncios,color=has.license)) +
  geom_line(data=haslicense.hosttype,
            aes(fechab,anuncios,color=has.license),size=1.5) +
  scale_color_manual(values=semaforo) +
  # annotate("text",x=as.Date("2018-05-28"),y=9000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_y_continuous(limits=c(0, max(haslicense.hosttype$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  # geom_text(data=filter(dates.count,fechab > as.Date("2019-03-01")),
  #           aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  geom_text_repel(data=filter(haslicense.hosttype, fechab > maxdate-1),
                  aes(fechab+10,anuncios,label=paste(format(anuncios, nsmall=1, big.mark=".") , sep = "")), 
                  nudge_x = 5, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y"
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios publicados según licencia por tipo de host",
       subtitle = paste("Airbnb. ", local_name," ",period,sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption,
       color = "Tiene" ) +
  facet_wrap(~host.type)
dev.off()

# B.4 With license and host type and room type ---------------------------------

# A
haslicense.hosttype.roomtype <- data_long %>% group_by(fechab,has.license,host.type,room_type.s) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes-linea-license-host-room.png", sep = ""),width = 1000,height = 600)
ggplot(NULL) + 
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  geom_point(data=haslicense.hosttype.roomtype,size=2.5, aes(fechab,anuncios,color=has.license)) +
  geom_line(data=haslicense.hosttype.roomtype,
            aes(fechab,anuncios,color=has.license),size=1.5) +
  scale_color_manual(values=semaforo) +
  annotate("text",x=as.Date("2018-05-28"),y=7000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_y_continuous(limits=c(0, max(haslicense.hosttype.roomtype$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text_repel(data=filter(haslicense.hosttype.roomtype, fechab > maxdate-1),
                  aes(fechab+10,anuncios,label=paste(format(anuncios, nsmall=1, big.mark=".") , sep = "")), 
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y"
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios publicados con y sin licencia por tipo de alojamiento y host",
       subtitle = paste("Airbnb. ", local_name," ",period,sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption,
       color = "Tiene" ) +
  facet_wrap(room_type.s~host.type)
dev.off()

# B xxxxx
png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes-linea-host-room-license.png", sep = ""),width = 1000,height = 600)
ggplot(NULL) + 
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  geom_text_repel(data=filter(haslicense.hosttype.roomtype, fechab > maxdate-1),
                  aes(fechab+10,anuncios,label=paste(format(anuncios, nsmall=1, big.mark=".") , sep = "")),
                                  nudge_x = 2, # adjust the starting y position of the text label
                                  size=4,
                                  hjust=0,
                                  family = "Roboto Condensed",
                                  direction="y",
                                  segment.size = 0.2,
                                  segment.color="#333333",
                                  xlim  = c(as.Date(max(haslicense.hosttype.roomtype$fechab)),as.Date("2019-12-4"))
  ) +
  geom_line(data=haslicense.hosttype.roomtype,
            aes(fechab,anuncios,color=host.type),size=1.5) +
  # scale_color_manual(values=c("#eab73e","#a7b3cc")) +
  scale_color_brewer(palette = "Dark2", type = "discrete") +
  annotate("text",x=as.Date("2018-05-28"),y=7000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_y_continuous(limits=c(0, max(haslicense.hosttype.roomtype$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_x_date( limits=c( min(haslicense.hosttype.roomtype$fechab), max(haslicense.hosttype.roomtype$fechab + 300)) ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios publicados con y sin licencia por tipo de alojamiento y host",
       subtitle = paste("Airbnb. ", local_name," ",period,sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption,
       color = "Host gestiona" ) +
  facet_wrap(room_type.s~has.license)
dev.off()

# C
png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes-linea-room-host-license.png", sep = ""),width = 1000,height = 600)
ggplot(NULL) + 
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  geom_text_repel(data=filter(haslicense.hosttype.roomtype, fechab > maxdate-1),
                  aes(fechab+10,anuncios,label=paste(format(anuncios, nsmall=1, big.mark=".") , sep = "")), 
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y"
  ) +
  geom_line(data=haslicense.hosttype.roomtype,
            aes(fechab,anuncios,color=room_type.s),size=1.5) +
  # scale_color_manual(values=c("#eab73e","#a7b3cc")) +
  annotate("text",x=as.Date("2018-05-28"),y=7000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_y_continuous(limits=c(0, max(haslicense.hosttype.roomtype$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios publicados con y sin licencia por tipo de alojamiento y host",
       subtitle = paste("Airbnb. ", local_name," ",period,sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption,
       color = "" ) +
  facet_wrap(host.type~has.license)
dev.off()

# C. Number of reviews 0 or More ---------------------------------
# dates.count.active <- data_long %>% group_by(fechab,reviews.type) %>% summarise(anuncios=n())
# 
# png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-reviews-por-mes-linea-compara.png", sep = ""),width = 1000,height = 400)
# ggplot(NULL) + 
#   annotate("rect", alpha = .07,
#            xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
#            xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
#            ymin = 0,
#            ymax = Inf) +
#   geom_line(data=dates.count.active %>% filter(reviews.type=="1 o más reviews"),
#             aes(fechab,anuncios,color=reviews.type),size=1.5) +
#   geom_point(data=dates.count.active %>% filter(reviews.type=="1 o más reviews"),
#              aes(fechab,anuncios,color=reviews.type),size=2.5) +
#   geom_line(data=dates.count,
#             aes(fechab,anuncios),size=1.5) +
#   # annotate("text",x=as.Date("2018-05-28"),y=4000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
#   # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
#   scale_y_continuous(limits=c(0, max(dates.count$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
#   geom_text(data=filter(dates.count,fechab > as.Date("2019-03-01")),
#             aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
#   geom_text(data=filter(dates.count.active,fechab > as.Date("2019-03-01") & reviews.type=="1 o más reviews"),
#             aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "top"
#   ) +
#   labs(title = "Número de anuncios publicados vs con alguna review",
#        subtitle = paste("Airbnb. ", local_name," ",period,sep=""),
#        y = "número de anuncios",
#        x = "",
#        caption = caption,
#        color = "Tiene" ) 
# dev.off()

# D. Number of reviews and availability ---------------------------------
dates.count.reviews.availablity <- data_long %>% group_by(fechab,reviews.type,availability.type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-reviews-por-mes-linea-multi.png", sep = ""),width = 1000,height = 400)
ggplot(NULL) + 
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  geom_line(data=filter(dates.count.reviews.availablity, !reviews.type ==""),
            aes(fechab,anuncios,color=availability.type),size=1.5) +
  scale_color_brewer(palette="PuBu") +
  scale_y_continuous(limits=c(0, max(dates.count.reviews.availablity$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  # annotate("text",x=as.Date("2018-05-28"),y=4000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb según número de reviews y disponibilidad",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption,
       color = "Según días disponbles" ) +
  facet_wrap(~reviews.type)
dev.off()

# E. separado por tipo de alojamiento -----
# counts listings per scraping date and room type 
dates.count.room_type <- data_long %>% group_by(fechab,room_type.s) %>% summarise(anuncios=n())

maxdate <- max(dates.count.room_type$fechab)

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes-room-type.png", sep = ""),width = 1000,height = 400)
dates.count.room_type %>%
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # verano
  annotate(geom = "text", x = as.Date("2017-08-06"), y = 500, label = "Verano", 
           family = "Roboto Condensed", hjust = 0.5, size=5, color="#888888") +
  # annotate("text",x=as.Date("2018-05-28"),y=4000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  geom_text_repel(data=filter(dates.count.room_type, fechab > maxdate-1),
                  aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), 
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y"
                  ) +
  geom_point(aes(fechab,anuncios,group=room_type.s,color=room_type.s),size=1.5) +
  geom_line(aes(fechab,anuncios,group=room_type.s,color=room_type.s),size=1.5) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_y_continuous(limits = c(0, max(dates.count.room_type$anuncios)), labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb por tipo de alojamiento",
       subtitle = paste( local_name, " ", period, "", sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption,
       color = "Tipo de alojamiento" )
dev.off()

# F.1 separado por tipo de host ----
dates.count.host.type <- data_long %>% group_by(fechab,host.type) %>% summarise(anuncios=n())

# timeline
png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes-host-type.png", sep = ""),width = 1000,height = 400)
dates.count.host.type %>%
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # verano
  annotate(geom = "text", x = as.Date("2017-08-06"), y = 500, label = "Verano", 
           family = "Roboto Condensed", hjust = 0.5, size=5, color="#888888") +
  # annotate("text",x=as.Date("2018-05-28"),y=4000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  geom_line(aes(fechab,anuncios,group=host.type,color=host.type),size=1.5) +
  geom_point(aes(fechab,anuncios,group=host.type,color=host.type),size=1.5) +
  geom_text_repel(data=filter(dates.count.host.type, fechab > maxdate-1),
                  aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), 
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y"
  ) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_color_brewer(palette = "Dark2", type = "discrete") +
  scale_y_continuous(limits = c(0, max(dates.count.host.type$anuncios)), labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb por tipo de host",
       subtitle = paste( local_name, " ", period, "", sep=""),
       y = "número de anuncios",
       x = "",
       color = "Host gestiona:",
       caption = caption)
dev.off()

# F.2 separado por tipo de host com más clasificaciones ----
dates.count.host.type.m <- data_long %>% group_by(fechab,host.type.m) %>% summarise(anuncios=n())

# timeline
png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes-host-type-1-2-3-more.png", sep = ""),width = 1000,height = 400)
dates.count.host.type.m %>%
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # verano
  annotate(geom = "text", x = as.Date("2017-08-06"), y = 1200, label = "Verano", 
           family = "Roboto Condensed", hjust = 0.5, size=5, color="#888888") +
  # annotate("text",x=as.Date("2018-05-28"),y=4000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  geom_line(aes(fechab,anuncios,group=host.type.m,color=host.type.m),size=1.5) +
  geom_point(aes(fechab,anuncios,group=host.type.m,color=host.type.m),size=1.5) +
  xlim(min(dates.count.host.type.m $fechab),as.Date("2020-01-4")) +
  geom_text_repel(data=filter(dates.count.host.type.m, fechab > maxdate-1),
                  aes(fechab+10,anuncios,label=paste(host.type.m, " ",format(anuncios, nsmall=1, big.mark=".") , sep = "")), 
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y"
  ) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_color_brewer(palette = "Dark2", type = "discrete") +
  scale_y_continuous(limits = c(0, max(dates.count.host.type.m$anuncios)), labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por tipo de host",
       subtitle = paste( local_name, " ", period, ". Según el número de anuncios que gestiona cada host.",sep=""),
       y = "número de anuncios",
       x = "fecha",
       caption = caption,
       color="Host gestiona")
dev.off()

# G.1 separado por tipo de host y alojamiento ----
dates.count.host.room.type <- data_long %>% group_by(fechab,room_type.s,host.type) %>% summarise(anuncios=n())

# timeline
png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes-host-room-type.png", sep = ""),width = 1000,height = 400)
dates.count.host.room.type %>%
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # verano
  # annotate(geom = "text", x = as.Date("2015-08-06"), y = 500, label = "Verano", 
  #          family = "Roboto Condensed", hjust = 0.5, size=3, color="#888888") +
  # annotate("text",x=as.Date("2018-05-28"),y=1000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  geom_line(aes(fechab,anuncios,group=host.type,color=host.type),size=1.5) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_y_continuous(limits = c(0, max(dates.count.host.room.type$anuncios)), labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_color_brewer(palette = "Dark2", type = "discrete") +
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb por tipo de host",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "fecha",
       caption = caption,
       color="Host gestiona") +
  facet_wrap(~room_type.s)
dev.off()

# steps
png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes-room-host-type.png", sep = ""),width = 1000,height = 300)
dates.count.host.room.type %>%
  ggplot () +
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # annotate("text",x=as.Date("2018-05-15"),y=1000,label="acuerdo",color="#000000",size=4) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_step(aes(fechab,anuncios,group=room_type.s,color=room_type.s),size=1.5) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb por tipo de host",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "fecha",
       caption = caption) +
  facet_wrap(~host.type)
dev.off()



# G.2a separado por tipo de host multiple y alojamiento simple ----
dates.count.host.m.room.type <- data_long %>% group_by(fechab,room_type.s,host.type.m) %>% summarise(anuncios=n())

# timeline
png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes-host-m-room-type.png", sep = ""),width = 1000,height = 400)
dates.count.host.m.room.type %>%
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # annotate("text",x=as.Date("2018-05-28"),y=4000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  geom_line(aes(fechab,anuncios,group=host.type.m,color=host.type.m),size=1.5) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_y_continuous(limits = c(0, max(dates.count.host.m.room.type$anuncios)), labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_color_brewer(palette = "Dark2", type = "discrete") +
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb por tipo de host",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "fecha",
       caption = caption,
       color="Host gestiona") +
  facet_wrap(~room_type.s)
dev.off()

# G.2b separado por tipo de host multiple y alojamiento -----------------------------------
dates.count.host.room.type.m <- data_long %>% group_by(fechab,room_type,host.type.m) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-mes-room-host-m-type.png", sep = ""),width = 1000,height = 600)
dates.count.host.room.type.m %>% filter(!room_type=="Habitación compartida") %>%
  ggplot () +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_line(aes(fechab,anuncios,group=room_type,color=room_type),size=1.5) +
  annotate("text",x=as.Date("2018-05-15"),y=100,label="acuerdo",color="#000000",size=4, hjust = 1 ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb por tipo de host",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "fecha",
       caption = caption,
       color="El host gestiona") +
  facet_wrap(~host.type.m)
dev.off()
  
# 4.2 Por barrios ---------------------------------------------------------------------------------------------------------

# inserts the data of barrio and unidad menor in place wher they should be according to other insiderairbnb data from other cities
data_long$neighbourhood_cleansed <- data_long$umenores
data_long$neighbourhood_group_cleansed<- data_long$barrio

# creates extended color palette https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
# use it once you have data_long data.frame
colourCount <- length(unique(data_long$neighbourhood_cleansed))
colourCountdistrict <- length(unique(data_long$neighbourhood_group_cleansed))
getPalette <- colorRampPalette(brewer.pal(9, "Set2"))

table(data_long$neighbourhood_cleansed)
table(data_long$neighbourhood_group_cleansed)

# A. counts listings por barrio --------------------------------------------------------------------
dates.count.barrio <- data_long %>% group_by(fechab,neighbourhood_cleansed) %>% summarise(anuncios=n())
# dates.count.barrio2 <- data_long2 %>% filter (exists ==1) %>% group_by(fechab,neighbourhood) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-umenor.png", sep = ""),width = 1000,height = 700)
# plot_a <- dates.count.barrio %>% 
dates.count.barrio %>% 
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_cleansed,color=neighbourhood_cleansed),size=0.8) +
  # scale_color_manual(values = getPalette(colourCountdistrict)) +
  annotate("text",x=as.Date("2018-05-28"),y=1950,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio,fechab==as.Date("2019-06-30"),anuncios>30), 
                  aes(
                    fechab,anuncios,label=paste(anuncios,neighbourhood_cleansed),
                    color=neighbourhood_cleansed
                    ),
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4.5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#333333"
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.barrio$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.barrio$fechab)),as.Date("2019-10-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por unidad menor en",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "",
       color = "Distrito",
       caption = caption)
dev.off()

# export data in the line chart to use in external visualization
# eeee <- plot_a$data
# spread <- eeee %>% select(-.group) %>% spread(neighbourhood,anuncios)
# write.csv(spread, file = "tmp/anucios-barrio-evol.csv", row.names = FALSE)
# write.csv(eeee, file = "tmp/anucios-barrio-evol-long.csv", row.names = FALSE)


# A.2 counts listings por barrio --------------------------------------------------------------------
dates.count.barrio <- data_long %>% group_by(fechab,barrio) %>% summarise(anuncios=n())
# dates.count.barrio2 <- data_long2 %>% filter (exists ==1) %>% group_by(fechab,neighbourhood) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-barrio.png", sep = ""),width = 900,height = 600)
# plot_a <- dates.count.barrio %>% 
dates.count.barrio %>% filter(!is.na(barrio)) %>%
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_line(aes(fechab,anuncios,group=barrio,color=barrio),size=0.8) +
  # scale_color_manual(values = getPalette(colourCountdistrict)) +
  # annotate("text",x=as.Date("2018-05-28"),y=1950,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio,fechab==as.Date("2019-06-30"),anuncios>20, !is.na(barrio)), 
                  aes(
                    fechab,anuncios,label=paste(anuncios,barrio),
                    color=barrio
                  ),
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4.5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#333333"
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.barrio[!is.na(dates.count.barrio$barrio),]$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.barrio$fechab)),as.Date("2019-12-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por barrio",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "",
       color = "Distrito",
       caption = caption)
dev.off()

# B. counts listings por barrio y license--------------------------------------------------------------neighbourhood_cleansed
dates.count.barrio.license <- data_long %>% group_by(fechab,neighbourhood_group_cleansed,has.license) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-barrio-license.png", sep = ""),width = 1000,height = 600)
dates.count.barrio.license %>% 
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_group_cleansed,color=neighbourhood_group_cleansed),size=1.2) +
  scale_color_manual(values = getPalette(colourCountdistrict)) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio.license,fechab==as.Date("2019-06-30"),anuncios>30), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood_group_cleansed)),
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#333333",
                  xlim  = c(as.Date(max(dates.count.barrio.license$fechab)),as.Date("2020-01-1"))
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.barrio.license$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_x_date(
    date_breaks = "1 year",
    limits = c(as.Date(min(dates.count.barrio.license$fechab)),as.Date("2019-12-31")),
    date_labels = "%Y"
  ) +
  # xlim(as.Date(min(dates.count.barrio.room$fechab)),as.Date("2021-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 20) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    # panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por barrio y si tiene licencia",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption) +
  facet_wrap(~has.license)
dev.off()

# C. counts listings por barrio y license y room type--------------------------------------------------------------
dates.count.barrio.license.room <- data_long %>% group_by(fechab,neighbourhood_cleansed,room_type.s,has.license,neighbourhood_group_cleansed) %>% 
  summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-umenor-license-room.png", sep = ""),width = 1300,height = 800)
dates.count.barrio.license.room %>% filter(!(neighbourhood_cleansed=="Gros" & neighbourhood_group_cleansed=="Centro")) %>%
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  annotate("text",x=as.Date("2018-05-25"),y=950,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_cleansed,color=neighbourhood_group_cleansed),size=0.9) +
  scale_color_manual(values = getPalette(colourCountdistrict)) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio.license.room,fechab==as.Date("2019-06-30"),anuncios>15), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood_cleansed)),
                  nudge_x = 190, # adjust the starting y position of the text label
                  size=4,
                  hjust=1,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#777777",
                  xlim  = c(as.Date(max(dates.count.barrio.license.room$fechab)),as.Date("2020-01-4")),
                  min.segment.length = 0.3
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.barrio.license.room$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.barrio.license.room$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#555555"),
    # panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb por unidad menor, si tiene licencia y tipo de alojamiento",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption,
       color= "barrio"
       ) +
  facet_wrap(room_type.s~has.license)
dev.off()

# C.2 counts listings por barrio y license y room type--------------------------------------------------------------
dates.count.distrito.license.room <- data_long %>% 
  group_by(fechab,neighbourhood_group_cleansed,room_type.s,has.license) %>% 
  summarise(anuncios=n())

# C.2.A -------------------------------
png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-barrio-license-room.png", sep = ""),width = 1300,height = 800)
dates.count.distrito.license.room %>% 
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  annotate("text",x=as.Date("2018-05-25"),y=950,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_group_cleansed,color=neighbourhood_group_cleansed),size=1.2) +
  scale_color_manual(values = getPalette(colourCountdistrict)) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.distrito.license.room,fechab==as.Date("2019-06-30"),anuncios>15), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood_group_cleansed)),
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#333333",
                  xlim  = c(as.Date(max(dates.count.distrito.license.room$fechab)),as.Date("2020-01-4"))
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.distrito.license.room$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.distrito.license.room$fechab)),as.Date("2020-01-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#555555"),
    # panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por barrio, si tiene licencia y tipo de alojamiento",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption) +
  facet_wrap(room_type.s~has.license)
dev.off()

 # C.2.B -------------------------------
dates.count.distrito.license.room.varios <- data_long %>% 
  filter(host.type == "varios anuncios" ) %>%
  group_by(fechab,neighbourhood_group_cleansed,room_type.s,has.license) %>% 
  summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-barrio-license-room-variosanuncios.png", sep = ""),width = 1100,height = 800)
dates.count.distrito.license.room.varios %>% 
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  annotate("text",x=as.Date("2018-05-25"),y=2100,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_group_cleansed,color=neighbourhood_group_cleansed),size=1.2) +
  scale_color_manual(values = getPalette(colourCountdistrict)) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.distrito.license.room.varios,fechab==as.Date("2019-06-30"),anuncios>15), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood_group_cleansed)),
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#333333",
                  xlim  = c(as.Date(max(dates.count.distrito.license.room.varios$fechab)),as.Date("2020-01-4"))
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.distrito.license.room.varios$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.distrito.license.room.varios$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#555555"),
    # panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por barrio, si tiene licencia y tipo de alojamiento. Hosts con varios anuncios",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption) +
  facet_wrap(room_type.s~has.license)
dev.off()


# C.2.C ---------------------------------------
dates.count.distrito.license.room.uni <- data_long %>% 
  filter(host.type == "1 anuncio" ) %>%
  group_by(fechab,neighbourhood_group_cleansed,room_type.s,has.license) %>% 
  summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-barrio-license-room-1anuncio.png", sep = ""),width = 1100,height = 800)
dates.count.distrito.license.room.uni %>% 
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  annotate("text",x=as.Date("2018-05-25"),y=950,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_group_cleansed,color=neighbourhood_group_cleansed),size=1.2) +
  scale_color_manual(values = getPalette(colourCountdistrict)) +
  
  # barrios labels
  geom_text_repel(data=filter(dates.count.distrito.license.room.uni,fechab==as.Date("2019-06-30"),anuncios>15), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood_group_cleansed)),
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=1,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#333333",
                  xlim  = c(as.Date(max(dates.count.distrito.license.room.uni$fechab)),as.Date("2020-01-4"))
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.distrito.license.room.varios$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.distrito.license.room.uni$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#555555"),
    # panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por barrio, si tiene licencia y tipo de alojamiento. Hosts con 1 anuncio",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption) +
  facet_wrap(room_type.s~has.license)
dev.off()

# counts listings por barrio y room type--------------------------------------------------------------
dates.count.barrio.room <- data_long %>% group_by(fechab,neighbourhood_cleansed,room_type.s) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-umenor-room.png", sep = ""),width = 1300,height = 700)
dates.count.barrio.room %>% 
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_cleansed,color=neighbourhood_cleansed),size=0.9) +
  scale_color_manual(values = getPalette(colourCount)) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio.room,fechab==as.Date("2019-06-30"),anuncios>14), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood_cleansed)),
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#333333",
                  xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2021-06-4"))
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.barrio.room$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_x_date(
    date_breaks = "1 year",
    limits = c(as.Date(min(dates.count.barrio.room$fechab)),as.Date("2021-01-4")),
    date_labels = "%Y"
  ) +
  # xlim(as.Date(min(dates.count.barrio.room$fechab)),as.Date("2021-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    # panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por unidad menor y tipo de alojamiento",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption) +
  facet_wrap(~room_type.s)
dev.off()

# counts listings por barrio y room type y reviews--------------------------------------------------------------
dates.count.barrio.room.reviews <- data_long %>% group_by(fechab,neighbourhood_cleansed,room_type.s,reviews.type) %>% 
  summarise(anuncios=n()) %>% filter(!reviews.type=="")

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-umenor-room-reviews.png", sep = ""),width = 1100,height = 800)
dates.count.barrio.room.reviews %>% 
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_cleansed,color=neighbourhood_cleansed),size=0.6) +
  scale_color_manual(values = getPalette(colourCount)) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio.room.reviews,fechab==as.Date("2019-06-30"),anuncios>15), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood_cleansed)),
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#333333",
                  xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2021-06-4"))
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.barrio.room.reviews$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.barrio.room.reviews$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#555555"),
    # panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Tipo alojamiento vs nº reviews por unidad menor",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption) +
  facet_wrap(reviews.type~room_type.s)
dev.off()

# counts listings por barrio y host type--------------------------------------------------------------
dates.count.barrio.host <- data_long %>% group_by(fechab,neighbourhood_cleansed,host.type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-unidad-menor-host.png", sep = ""),width = 1000,height = 600)
dates.count.barrio.host %>%
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
           xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
           ymin = 0,
           ymax = Inf) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  annotate("text",x=as.Date("2018-05-25"),y=1200,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_cleansed,color=neighbourhood_cleansed),size=0.9) +
  scale_color_manual(values = getPalette(colourCount)) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio.host,fechab==as.Date("2019-06-30"),anuncios>14), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood_cleansed)),
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#666666",
                  xlim  = c(as.Date(max(dates.count.barrio.host$fechab)),as.Date("2020-06-4"))
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.barrio.host$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.barrio.host$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#555555"),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por unidad menor y tipo de host",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption) +
  facet_wrap(~host.type)
dev.off()

# counts listings por barrio y host type y reviews--------------------------------------------------------------
# dates.count.barrio.host.reviews <- data_long %>% group_by(fechab,neighbourhood_cleansed,host.type,reviews.type) %>% 
#   summarise(anuncios=n()) %>% filter(!reviews.type=="")
# 
# png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-umenor-host-reviews.png", sep = ""),width = 1000,height = 600)
# dates.count.barrio.host.reviews %>% 
#   ggplot () +
#   # se marcan meses de verano
#   annotate("rect", alpha = .07,
#            xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
#            xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
#            ymin = 0,
#            ymax = Inf) +
#   # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1.5) +
#   geom_line(aes(fechab,anuncios,group=neighbourhood_cleansed,color=neighbourhood_cleansed),size=0.6) +
#   # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
#   # barrios labels
#   geom_text_repel(data=filter(dates.count.barrio.host.reviews,fechab==as.Date("2019-06-30"),anuncios>23), 
#                   aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood_cleansed)),
#                   size=4,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.colour="grey") +
#   scale_y_continuous(limits=c(0, max(dates.count.barrio.host.reviews$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
#   xlim(as.Date(min(dates.count.barrio.host$fechab)),as.Date("2020-06-4")) +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de anuncios de Airbnb por unidad menor, tipo de host y nº reviews",
#        subtitle = paste( local_name, " ", period, sep=""),
#        y = "número de anuncios",
#        x = "",
#        caption = caption) +
#   facet_wrap(reviews.type~host.type)
# dev.off()

# 4.3 Por distritos ---------------------------------------------------------------------------------------------------------
# A counts listings por distrito --------------------------------------------------------------------
dates.count.distrito <- data_long %>% group_by(fechab,neighbourhood_group_cleansed) %>% summarise(anuncios=n())
# ssss
png(filename=paste("images/airbnb/evolucion/anuncios-",local_basic,"-por-distrito.png", sep = ""),width = 1000,height = 600)
dates.count.distrito %>% 
  ggplot () +
  # se marcan meses de verano
    annotate("rect", alpha = .07,
             xmin = c(as.Date("2017-06-21"),as.Date("2018-06-21"),as.Date("2019-06-21")),
             xmax = c(as.Date("2017-09-21"),as.Date("2018-09-21"),as.Date("2019-09-30")),
             ymin = 0,
             ymax = Inf) +
  annotate("text",x=as.Date("2018-05-28"),y=5000,label="acuerdo",color="#000000",size=5,hjust=1,family = "Roboto Condensed") +
  geom_point(aes(fechab,anuncios,group=neighbourhood_group_cleansed,color=neighbourhood_group_cleansed),size=1.5) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_group_cleansed,color=neighbourhood_group_cleansed),size=1.5) +
  scale_color_manual(values = getPalette(colourCountdistrict)) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # distritos labels
  geom_text_repel(data=filter(dates.count.distrito, fechab > maxdate-1),
                  aes(fechab+10,anuncios,label=paste(neighbourhood_group_cleansed,format(anuncios, nsmall=1, big.mark=".") , sep = " ")), 
                  nudge_x = 25, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.colour = "#dddddd"
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.distrito$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.distrito$fechab)),as.Date("2020-02-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por barrio",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption)
dev.off()

# B. counts listings por distrito y room type--------------------------------------------------------------
dates.count.distrito.room <- data_long %>% group_by(fechab,neighbourhood_group_cleansed,room_type.s) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-distrito-room.png", sep = ""),width = 1200,height = 600)
dates.count.distrito.room %>%
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .1,
           xmin = c(as.Date("2018-06-21")),
           xmax = c(as.Date("2018-09-21")),
           ymin = 0,
           ymax = Inf) +
  annotate("text",x=as.Date("2018-05-25"),y=5000,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  geom_point(aes(fechab,anuncios,group=neighbourhood_group_cleansed,color=neighbourhood_group_cleansed),size=1.5) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_group_cleansed,color=neighbourhood_group_cleansed),size=1.5) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_color_manual(values = getPalette(colourCountdistrict)) +
  # distritos labels
  geom_text_repel(data=filter(dates.count.distrito.room, fechab > maxdate-1),
                  aes(fechab+10,anuncios,label=paste(neighbourhood_group_cleansed,format(anuncios, nsmall=1, big.mark=".") , sep = " ")), 
                  nudge_x = 25, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.colour = "#dddddd"
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.distrito.room$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.distrito.room$fechab)),as.Date("2020-12-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por distrito y tipo de alojamiento",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "fecha",
       caption = caption) +
  facet_wrap(~room_type.s)
dev.off()

# C. counts listings por distrito y host type--------------------------------------------------------------
dates.count.distrito.host <- data_long %>% group_by(fechab,neighbourhood_group_cleansed,host.type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-distrito-host.png", sep = ""),width = 1200,height = 600)
dates.count.distrito.host %>% 
  ggplot () +
  # se marcan meses de verano
  annotate("rect", alpha = .1,
           xmin = c(as.Date("2018-06-21")),
           xmax = c(as.Date("2018-09-21")),
           ymin = 0,
           ymax = Inf) +
  annotate("text",x=as.Date("2018-05-25"),y=3200,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  geom_point(aes(fechab,anuncios,group=neighbourhood_group_cleansed,color=neighbourhood_group_cleansed),size=1.5) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_group_cleansed,color=neighbourhood_group_cleansed),size=1.5) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_color_manual(values = getPalette(colourCountdistrict)) +
  # distritos labels
  geom_text_repel(data=filter(dates.count.distrito.host, fechab > maxdate-1),
                  aes(fechab+10,anuncios,label=paste(neighbourhood_group_cleansed,format(anuncios, nsmall=1, big.mark=".") , sep = " ")), 
                  nudge_x = 25, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.colour = "#cccccc"
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.distrito.host$anuncios)) ,labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.distrito.host$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por distrito y tipo de host en Barcelona",
       subtitle = paste( local_name, " ", period, sep=""),
       y = "número de anuncios",
       x = "",
       caption = caption) +
  facet_wrap(~host.type)
dev.off()

# data_long[data_long$exists == 1 & data_long$fechab > "2018-04-01",] %>%
# ggplot(aes(x = as.factor(fecha), y = as.factor(id))) +
#   # geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",alpha=0.3,size=0.1)
#   geom_point(size=0.1,alpha=0.8)

# 4.4 View every listing ------------------------------------------------------------------------
# cada anuncio es una línea. eje y nº reviews-----------------
png(filename=paste("images/airbnb/numero-review-anuncio-centro-201906.png", sep = ""),width = 1300,height = 700)
data_long %>% 
  filter(neighbourhood_group_cleansed == "Centro" ) %>%
  ggplot () +
  # annotate("text",x=as.Date("2018-05-25"),y=1300,label="acuerdo",color="#000000",
  #          size=5,family = "Roboto Condensed",hjust=1) +
  geom_line(aes(fechab,number_of_reviews, group = id),size=0.06,alpha=0.5) +
  geom_point(aes(fechab,number_of_reviews),size=0.03,alpha=0.2) +
  # scale_color_manual(values = getPalette(colourCount)) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_x_date(
    date_breaks = "1 year",
    # limits = c(as.Date(min(dates.count.barrio.room$fechab)),as.Date("2021-01-4")),
    date_labels = "%Y"
  ) +
  # ylim(0,5) +
  # xlim(as.Date(min(dates.count.barrio.room$fechab)),as.Date("2021-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    # panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de reviews por anuncio. Donostia",
       subtitle = "Centro. 2017- junio 2019",
       y = "número de reviews",
       x = "",
       caption = caption) +
  facet_wrap(room_type.s~host.type)
dev.off()

table(data_long$neighbourhood)