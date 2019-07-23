# Script para calcular distribución de anfitriones, alojamientos y plazas 
# según los gestionen hosts con 1, 2, 3-4 o más de 5 alojamientos

# 1. Load libraries ----
library(tidyverse)

# Selecciona variables de configuración
# región/lugar, fechas a analizar...
local_activo_name <- "Donostia - San Sebastián" #cambia 'Barcelona' por el municipio que quieras analizar
caption_1 <- "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb"
fechas <- "2017-junio 2019"
fecha_estudio <- "190630"

# ------ Get dates when data were scraped --------
# Loads dates with listings data
dates <- c("2019-06-30","2019-05-29","2019-04-29", "2019-03-30","2019-02-19","2019-01-30",
           "2018-12-22","2018-11-26", "2018-10-20","2018-09-27","2018-08-28","2018-07-31","2018-04-21",
           "2017-03-24"
           )

dates <- rev(dates)

# creates data.frames to influde data
anfitriones <- data.frame(matrix(0, ncol = 6, nrow = length(dates)))
names(anfitriones) <- c("date","mas_de_14","mas_de_5","n3.4","n2","n1")

alojamientos <- data.frame(matrix(0, ncol = 6, nrow = length(dates)))
names(alojamientos) <- c("date","mas_de_14","mas_de_5","n3.4","n2","n1")

plazas <- data.frame(matrix(0, ncol = 6, nrow = length(dates)))
names(plazas) <- c("date","mas_de_14","mas_de_5","n3.4","n2","n1")

data_long_donostia$accommodates <- as.numeric(data_long_donostia$accommodates)

for (i in 1:length(dates)) {
  # read data
  # local_activo <- read.delim( paste0("data/original/euskadi/insideairbnb/",dates[i],"/data/listings.csv.gz"),sep = ",") %>% 
  #   select(host_id, host_name, accommodates) 
  #uses the data?log?donostia where all the Donostia listings are
  local_activo <- data_long_donostia %>% 
    filter(fechab == dates[i] ) %>%
    select(host_id, host_name, accommodates)
  
  # calcula nº de plazas y alojamientos por host
  n_alojamientos <- local_activo %>% group_by(host_id, host_name) %>% summarize(alojamientos = n(),plazas = sum(accommodates) ) %>% arrange(desc(alojamientos))
  
  # calcula nº de 
  # naccommodates <- group_by(local_activo, host_id, host_name) %>% summarise( alojamientos = n(), plazas = sum(accommodates) ) %>%  arrange(desc(plazas))
  
  # inserts date in every row
  anfitriones$date[i] <- dates[i]
  alojamientos$date[i] <- dates[i]
  plazas$date[i] <- dates[i]
  
  # Calcula cuantos anfitriones tienen n alojamientos
  anfitriones$n1[i] <- nrow(n_alojamientos[n_alojamientos$alojamientos == 1,])
  anfitriones$n2[i] <- nrow(n_alojamientos[n_alojamientos$alojamientos == 2,])
  anfitriones$n3.4[i] <- nrow(n_alojamientos[(n_alojamientos$alojamientos == 3 |n_alojamientos$alojamientos == 4) ,])
  anfitriones$mas_de_5[i] <- nrow(n_alojamientos[n_alojamientos$alojamientos >4 & n_alojamientos$alojamientos < 15,])
  anfitriones$mas_de_14[i] <- nrow(n_alojamientos[n_alojamientos$alojamientos >14,])
  
  # Calcula cuantos alojamientos en total tienen los anfitriones con n alojamientos
  alojamientos$n1[i] <- sum(n_alojamientos[n_alojamientos$alojamientos == 1,"alojamientos"])
  alojamientos$n2[i] <- sum(n_alojamientos[n_alojamientos$alojamientos == 2,"alojamientos"])
  alojamientos$n3.4[i] <- sum(n_alojamientos[(n_alojamientos$alojamientos == 3 |n_alojamientos$alojamientos == 4),"alojamientos"])
  alojamientos$mas_de_5[i] <- sum(n_alojamientos[n_alojamientos$alojamientos > 4 & n_alojamientos$alojamientos < 15,"alojamientos"])
  alojamientos$mas_de_14[i] <- sum(n_alojamientos[n_alojamientos$alojamientos >14,"alojamientos"])
  
  # Calcula cuantas plazas en total tienen los anfitriones con n alojamientos
  plazas$n1[i] <- sum(n_alojamientos[n_alojamientos$host_id %in% n_alojamientos[n_alojamientos$alojamientos == 1,]$host_id,]$plazas)
  plazas$n2[i] <- sum(n_alojamientos[n_alojamientos$host_id %in% n_alojamientos[n_alojamientos$alojamientos == 2,]$host_id,]$plazas)
  plazas$n3.4[i] <- sum(n_alojamientos[n_alojamientos$host_id %in% n_alojamientos[(n_alojamientos$alojamientos == 3 | n_alojamientos$alojamientos == 4),]$host_id,]$plazas)
  plazas$mas_de_5[i] <- sum(n_alojamientos[n_alojamientos$host_id %in% n_alojamientos[n_alojamientos$alojamientos > 4 & n_alojamientos$alojamientos < 15,]$host_id,]$plazas)
  plazas$mas_de_14[i] <- sum(n_alojamientos[n_alojamientos$host_id %in% n_alojamientos[n_alojamientos$alojamientos > 14,]$host_id,]$plazas)
}

# parse date
anfitriones$datenew <- as.Date(anfitriones$date, "%Y-%m-%d")
alojamientos$datenew <- as.Date(alojamientos$date, "%Y-%m-%d")
plazas$datenew <- as.Date(plazas$date, "%Y-%m-%d")

# hosts per date
anfitriones$sum <- rowSums(anfitriones[,2:6])
alojamientos$sum <- rowSums(alojamientos[,2:6])
plazas$sum <- rowSums(plazas[,2:6])

# creates dataframe for percentages
anfitriones_per <- anfitriones
alojamientos_per <- alojamientos
plazas_per <- plazas

# calculates % -----------------------
anfitriones_per$n1 <- round(anfitriones_per$n1 / anfitriones_per$sum * 100,1)
anfitriones_per$n2 <- round(anfitriones_per$n2 / anfitriones_per$sum * 100,1)
anfitriones_per$n3.4 <- round(anfitriones_per$n3.4 / anfitriones_per$sum * 100,1)
anfitriones_per$mas_de_5 <- round(anfitriones_per$mas_de_5 / anfitriones_per$sum * 100,1)
anfitriones_per$mas_de_14 <- round(anfitriones_per$mas_de_14 / anfitriones_per$sum * 100,1)

alojamientos_per$n1 <- round(alojamientos_per$n1 / alojamientos_per$sum * 100,1)
alojamientos_per$n2 <- round(alojamientos_per$n2 / alojamientos_per$sum * 100,1)
alojamientos_per$n3.4 <- round(alojamientos_per$n3.4 / alojamientos_per$sum * 100,1)
alojamientos_per$mas_de_5 <- round(alojamientos_per$mas_de_5 / alojamientos_per$sum * 100,1)
alojamientos_per$mas_de_14 <- round(alojamientos_per$mas_de_14 / alojamientos_per$sum * 100,1)

plazas_per$n1 <- round(plazas_per$n1 / plazas_per$sum * 100,1)
plazas_per$n2 <- round(plazas_per$n2 / plazas_per$sum * 100,1)
plazas_per$n3.4 <- round(plazas_per$n3.4 / plazas_per$sum * 100,1)
plazas_per$mas_de_5 <- round(plazas_per$mas_de_5 / plazas_per$sum * 100,1)
plazas_per$mas_de_14 <- round(plazas_per$mas_de_14 / plazas_per$sum * 100,1)

# color palete
# palette1 <- c("#4292c6","#6baed6","#9ecae1","#fee5d9")

# Plot Anfitriones -----------------------
png(filename=paste0("images/airbnb/hosts/evolution/",fecha_estudio,"_anfitriones-concentracion_donostia.png"),width = 1000,height = 390)
ggplot(anfitriones, aes(x=datenew))+
  geom_ribbon(aes(ymin = 0, ymax= n1), fill="#fee5d9") +
  geom_ribbon(aes(ymin = n1, ymax= n1+n2 ), fill="#9ecae1") +
  geom_ribbon(aes(ymin = n1+n2, ymax= n1+n2+n3.4 ), fill="#6baed6") +
  geom_ribbon(aes(ymin = n1+n2+n3.4, ymax= n1+n2+n3.4+mas_de_5 ), fill="#4292c6") +
  geom_ribbon(aes(ymin = n1+n2+n3.4+mas_de_5, ymax= n1+n2+n3.4+mas_de_5+mas_de_14 ), fill="#264c65") +
  # text
  geom_text( data=filter(anfitriones,datenew==as.Date("2019-06-30")), 
             aes( datenew,n1/2,label=paste0(format(n1,decimal.mark=',')," (1 anuncio)") ), hjust=0, family = "Roboto Condensed", size=5 ) +
  geom_text( data=filter(anfitriones,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2/2,label=paste0(format(n2,decimal.mark=',')," (2 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  geom_text( data=filter(anfitriones,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2+n3.4/2,label=paste0(format(n3.4,decimal.mark=',')," (3-4 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5 ) +
  geom_text( data=filter(anfitriones,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2+n3.4+mas_de_5/2+12,label=paste0(format(mas_de_5,decimal.mark=',')," (5-14 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  geom_text( data=filter(anfitriones,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2+n3.4+mas_de_5+mas_de_14+30,label=paste0(format(mas_de_14,decimal.mark=',')," (más de 14 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  # # text
  # geom_text(data=filter(anfitriones,datenew==as.Date("2019-06-30")), 
  #                 aes(
  #                   datenew+5,n1/2,label=paste(n1,"1 anuncio")
  #                 )
  # ) +
  # notes
  # annotate("text",x=as.Date("2018-05-26"),y=500,label="acuerdo",color="#000000",size=5,hjust = 1,family = "Roboto Condensed") +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # scale
  xlim(as.Date(min(anfitriones_per$datenew)),as.Date("2020-02-4")) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  # theming
  theme_minimal(base_family = "Roboto Condensed", base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    plot.caption=element_text(size=9)
  ) +
  labs(title = paste("Anfitriones por cantidad de alojamientos que gestionan", sep = ""),
       subtitle = paste(local_activo_name,". ", fechas, sep = ""),
       y = "nº anfitriones",
       x = NULL,
       caption = caption) 
dev.off()

png(filename=paste0("images/airbnb/hosts/evolution/",fecha_estudio,"_anfitriones-concentracion_percent_donostia.png"),width = 1000,height = 390)
ggplot(anfitriones_per, aes(x=datenew))+
  geom_ribbon(aes(ymin = 0, ymax= n1 ), fill="#fee5d9") +
  geom_ribbon(aes(ymin = n1, ymax= n1+n2 ), fill="#9ecae1") +
  geom_ribbon(aes(ymin = n1+n2, ymax= n1+n2+n3.4 ), fill="#6baed6") +
  # geom_ribbon(aes(ymin = n1+n2+n3.4, ymax= 100 ), fill="#4292c6") +
  geom_ribbon(aes(ymin = n1+n2+n3.4, ymax= n1+n2+n3.4+mas_de_5 ), fill="#4292c6") +
  geom_ribbon(aes(ymin = n1+n2+n3.4+mas_de_5, ymax= 100 ), fill="#264c65") +
  # text
  geom_text( data=filter(anfitriones_per,datenew==as.Date("2019-06-30")), 
             aes( datenew,n1/2,label=paste0(format(n1,decimal.mark=','),"% (1 anuncio)") ), hjust=0, family = "Roboto Condensed", size=5 ) +
  geom_text( data=filter(anfitriones_per,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2/2,label=paste0(format(n2,decimal.mark=','),"% (2 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  geom_text( data=filter(anfitriones_per,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2+n3.4/2,label=paste0(format(n3.4,decimal.mark=','),"% (3-4 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5 ) +
  geom_text( data=filter(anfitriones_per,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2+n3.4+mas_de_5/2,label=paste0(format(mas_de_5,decimal.mark=','),"% (5-14 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  geom_text( data=filter(anfitriones_per,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2+n3.4+mas_de_5+mas_de_14+4,label=paste0(format(mas_de_14,decimal.mark=','),"% (más de 14 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  # notes
  # annotate("text",x=as.Date("2018-05-26"),y=5,label="acuerdo",color="#000000",size=5,hjust = 1,family = "Roboto Condensed") +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # scale
  # scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(anfitriones_per$datenew)),as.Date("2019-10-4")) +
  # theming
  theme_minimal(base_family = "Roboto Condensed", base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank()
    # legend.position = "none"
    # plot.caption=element_text(size=9)
  ) +
  labs(title = paste("Anfitriones por cantidad de alojamientos que gestionan (%)", sep = ""),
       subtitle = paste(local_activo_name,". ", fechas, sep = ""),
       y = "% anfitriones",
       x = NULL,
       caption = caption) 
dev.off()

# Plot alojamientos ---------------
png(filename=paste0("images/airbnb/hosts/evolution/",fecha_estudio,"_alojamientos-concentracion_donostia.png"),width = 1000,height = 390)
ggplot(alojamientos, aes(x=datenew))+
  geom_ribbon(aes(ymin = 0, ymax= n1 ), fill="#fee5d9") +
  geom_ribbon(aes(ymin = n1, ymax= n1+n2 ), fill="#9ecae1") +
  geom_ribbon(aes(ymin = n1+n2, ymax= n1+n2+n3.4 ), fill="#6baed6") +
  geom_ribbon(aes(ymin = n1+n2+n3.4, ymax= n1+n2+n3.4+mas_de_5), fill="#4292c6") +
  geom_ribbon(aes(ymin = n1+n2+n3.4+mas_de_5, ymax= n1+n2+n3.4+mas_de_5+mas_de_14 ), fill="#264c65") +
  # text
  geom_text( data=filter(alojamientos,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1/2,label=paste0(format(n1,decimal.mark=',')," (1 anuncio)") ), hjust=0, family = "Roboto Condensed", size=5 ) +
  geom_text( data=filter(alojamientos,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2/2,label=paste0(format(n2,decimal.mark=',')," (2 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  geom_text( data=filter(alojamientos,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2+n3.4/2,label=paste0(format(n3.4,decimal.mark=',')," (3-4 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5 ) +
  geom_text( data=filter(alojamientos,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2+n3.4+mas_de_5/2,label=paste0(format(mas_de_5,decimal.mark=',')," (5-14 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  geom_text( data=filter(alojamientos,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2+n3.4+mas_de_5+mas_de_14/2,label=paste0(format(mas_de_14,decimal.mark=',')," (más de 14 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  
  # notes
  # annotate("text",x=as.Date("2018-05-26"),y=5000,label="acuerdo",color="#000000",size=5,hjust = 1,family = "Roboto Condensed") +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # scale
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(anfitriones_per$datenew)),as.Date("2020-01-4")) +
  # theming
  theme_minimal(base_family = "Roboto Condensed", base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    plot.caption=element_text(size=9)
  ) +
  labs(title = paste("Alojamientos por cantidad de alojamientos que gestiona cada anfitrión", sep = ""),
       subtitle = paste(local_activo_name,". ", fechas, sep = ""),
       y = "nº alojamientos",
       x = NULL,
       caption = caption) 
dev.off()

png(filename=paste0("images/airbnb/hosts/evolution/",fecha_estudio,"_alojamientos-concentracion_percent_donostia.png"),width = 1000,height = 390)
ggplot(alojamientos_per, aes(x=datenew))+
  geom_ribbon(aes(ymin = 0, ymax= n1 ), fill="#fee5d9") +
  geom_ribbon(aes(ymin = n1, ymax= n1+n2 ), fill="#9ecae1") +
  geom_ribbon(aes(ymin = n1+n2, ymax= n1+n2+n3.4 ), fill="#6baed6") +
  geom_ribbon(aes(ymin = n1+n2+n3.4, ymax= n1+n2+n3.4+mas_de_5 ), fill="#4292c6") +
  geom_ribbon(aes(ymin = n1+n2+n3.4+mas_de_5, ymax= n1+n2+n3.4+mas_de_5+mas_de_14 ), fill="#264c65") +
  # text
  geom_text( data=filter(alojamientos_per,datenew==as.Date("2019-06-30")), aes( datenew+5,n1/2,label=paste0(format(n1,decimal.mark=','),"% (1 anuncio)") ), hjust=0, family = "Roboto Condensed", size=5 ) +
  geom_text( data=filter(alojamientos_per,datenew==as.Date("2019-06-30")), aes( datenew+5,n1+n2/2,label=paste0(format(n2,decimal.mark=','),"% (2 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  geom_text( data=filter(alojamientos_per,datenew==as.Date("2019-06-30")), aes( datenew+5,n1+n2+n3.4/2,label=paste0(format(n3.4,decimal.mark=','),"% (3-4 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5 ) +
  geom_text( data=filter(alojamientos_per,datenew==as.Date("2019-06-30")), aes( datenew+5,n1+n2+n3.4+mas_de_5/2,label=paste0(format(mas_de_5,decimal.mark=','),"% (5-14 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  geom_text( data=filter(alojamientos_per,datenew==as.Date("2019-06-30")), aes( datenew+5,n1+n2+n3.4+mas_de_5+mas_de_14/2,label=paste0(format(mas_de_14,decimal.mark=','),"% (más de 14 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  # notes
  # annotate("text",x=as.Date("2018-05-26"),y=5,label="acuerdo",color="#000000",size=5,hjust = 1,family = "Roboto Condensed") +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # scale
  # scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(anfitriones_per$datenew)),as.Date("2019-10-4")) +
  # theming
  theme_minimal(base_family = "Roboto Condensed", base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    plot.caption=element_text(size=9)
  ) +
  labs(title = paste("Alojamientos por cantidad de alojamientos que gestiona cada anfitrión (%)", sep = ""),
       subtitle = paste(local_activo_name,". ", fechas, sep = ""),
       y = "% alojamientos",
       x = NULL,
       caption = caption) 
dev.off()

# Plot plazas -----------------
png(filename=paste0("images/airbnb/hosts/evolution/",fecha_estudio,"_plazas-concentracion_donostia.png"),width = 1000,height = 390)
ggplot(plazas, aes(x=datenew))+
  geom_ribbon(aes(ymin = 0, ymax= n1 ), fill="#fee5d9") +
  geom_ribbon(aes(ymin = n1, ymax= n1+n2 ), fill="#9ecae1") +
  geom_ribbon(aes(ymin = n1+n2, ymax= n1+n2+n3.4 ), fill="#6baed6") +
  geom_ribbon(aes(ymin = n1+n2+n3.4, ymax= n1+n2+n3.4+mas_de_5 ), fill="#4292c6") +
  geom_ribbon(aes(ymin = n1+n2+n3.4+mas_de_5, ymax= n1+n2+n3.4+mas_de_5+mas_de_14 ), fill="#264c65") +
  # text
  geom_text( data=filter(plazas,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1/2,label=paste0(format(n1,decimal.mark=',')," (1 anuncio)") ), hjust=0, family = "Roboto Condensed", size=5 ) +
  geom_text( data=filter(plazas,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2/2,label=paste0(format(n2,decimal.mark=',')," (2 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  geom_text( data=filter(plazas,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2+n3.4/2,label=paste0(format(n3.4,decimal.mark=',')," (3-4 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5 ) +
  geom_text( data=filter(plazas,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2+n3.4+mas_de_5/2,label=paste0(format(mas_de_5,decimal.mark=',')," (5-14 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  geom_text( data=filter(plazas,datenew==as.Date("2019-06-30")), 
             aes( datenew+5,n1+n2+n3.4+mas_de_5+mas_de_14/2,label=paste0(format(mas_de_14,decimal.mark=',')," (más de 14 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  
  # notes
  # annotate("text",x=as.Date("2018-05-26"),y=5000,label="acuerdo",color="#000000",size=5,hjust = 1,family = "Roboto Condensed") +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # scale
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(anfitriones_per$datenew)),as.Date("2020-1-4")) +
  # theming
  theme_minimal(base_family = "Roboto Condensed", base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    plot.caption=element_text(size=9)
  ) +
  labs(title = paste("Plazas por cantidad de alojamientos que gestiona cada anfitrión", sep = ""),
       subtitle = paste(local_activo_name,". ", fechas, sep = ""),
       y = "nº plazas",
       x = NULL,
       caption = caption) 
dev.off()

png(filename=paste0("images/airbnb/hosts/evolution/",fecha_estudio,"_plazas-concentracion_percent_donostia.png"),width = 1000,height = 390)
ggplot(plazas_per, aes(x=datenew))+
  geom_ribbon(aes(ymin = 0, ymax= n1 ), fill="#fee5d9") +
  geom_ribbon(aes(ymin = n1, ymax= n1+n2 ), fill="#9ecae1") +
  geom_ribbon(aes(ymin = n1+n2, ymax= n1+n2+n3.4 ), fill="#6baed6") +
  geom_ribbon(aes(ymin = n1+n2+n3.4, ymax= n1+n2+n3.4+mas_de_5 ), fill="#4292c6") +
  geom_ribbon(aes(ymin = n1+n2+n3.4+mas_de_5, ymax= n1+n2+n3.4+mas_de_5+mas_de_14 ), fill="#264c65") +
  # text
  geom_text( data=filter(plazas_per,datenew==as.Date("2019-06-30")), aes( datenew+5,n1/2,label=paste0(format(n1,decimal.mark=','),"% (1 anuncio)") ), hjust=0, family = "Roboto Condensed", size=5 ) +
  geom_text( data=filter(plazas_per,datenew==as.Date("2019-06-30")), aes( datenew+5,n1+n2/2,label=paste0(format(n2,decimal.mark=','),"% (2 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  geom_text( data=filter(plazas_per,datenew==as.Date("2019-06-30")), aes( datenew+5,n1+n2+n3.4/2,label=paste0(format(n3.4,decimal.mark=','),"% (3-4 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5 ) +
  geom_text( data=filter(plazas_per,datenew==as.Date("2019-06-30")), aes( datenew+5,n1+n2+n3.4+mas_de_5/2,label=paste0(format(mas_de_5,decimal.mark=','),"% (5-14 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  geom_text( data=filter(plazas_per,datenew==as.Date("2019-06-30")), aes( datenew+5,n1+n2+n3.4+mas_de_5+mas_de_14/2,label=paste0(format(mas_de_14,decimal.mark=','),"% (más de 14 anuncios)") ), hjust=0, family = "Roboto Condensed", size=5  ) +
  # notes
  # annotate("text",x=as.Date("2018-05-26"),y=5,label="acuerdo",color="#000000",size=5,hjust = 1,family = "Roboto Condensed") +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # scale
  # scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(anfitriones_per$datenew)),as.Date("2019-10-4")) +
  # theming
  theme_minimal(base_family = "Roboto Condensed", base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    plot.caption=element_text(size=9)
  ) +
  labs(title = paste("Plazas por cantidad de alojamientos que gestiona cada anfitrión (%)", sep = ""),
       subtitle = paste(local_activo_name,". ", fechas, sep = ""),
       y = "% plazas",
       x = NULL,
       caption = caption) 
dev.off()

