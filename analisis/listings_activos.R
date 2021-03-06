# Análisis de Airbnb en Donostia: anuncios activos en base a datos de datahippo.org y scraping para last review
# Incluye generación de visualizaciones para ver y calcular el número de anuncios activos

# # ---- Load libraries -----
# library(tidyverse)
# read shapes
library(rgdal)
library(ggmap) #for theme nothing
library(reshape)
library(gridExtra)
library(gsubfn) # select text in the parenthesis with regex

# ------ Load files ----------
# Airbnb listings 2018
# load("data/output/180423_listings-airbnb-donostia_datahippo_barrio-umenor.Rda")
airbnb2018 <- read.delim("data/output/180423_listings-airbnb-donostia_datahippo_barrio-umenor.csv",sep = ",")
# Translate room type
levels(airbnb2018$room_type) <- c("Vivienda completa","Habitación privada","Habitación compartida")

airbnb2018_jun <- read.delim("data/original/180604_listings-airbnb-donostia_datahippo.csv",sep = ",")
levels(airbnb2018_jun$room_type) <- c("Vivienda completa","Habitación privada","Habitación compartida")

airbnb2018_sep <- read.delim("data/output/listings-airbnb_donostia_datahippo_with-last-review-20181127-reviewed.csv",sep = ",")
airbnb2018_sep$exists_20181127 <- as.factor(airbnb2018_sep$exists_20181127)
levels(airbnb2018_sep$exists_20181127) <- c("No existe","Existe")
# airbnb2018_sep <- read.delim("data/output/180604_listings-airbnb-donostia_datahippo_with-last-review-20180912-reviewed.csv",sep = ",")
# airbnb2018_sep$exists_20180912 <- as.factor(airbnb2018_sep$exists_20180912)
# levels(airbnb2018_sep$exists_20180912) <- c("No existe","Existe")
levels(airbnb2018_sep$room_type) <- c("Vivienda completa","Habitación privada","Habitación compartida")

# ----- Anuncios activos parse dates-----
# airbnb2018$found_year <- as.numeric(strapplyc( as.character(airbnb2018$found), "([0-9]*).*", simplify = TRUE))
# airbnb2018$found_month <- as.numeric(strapplyc( as.character(airbnb2018$found), "[0-9]*-([0-9]*).*", simplify = TRUE))
# airbnb2018$found_day <- as.numeric(strapplyc( as.character(airbnb2018$found), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE))
airbnb2018$found_date <- as.Date(as.POSIXct(airbnb2018$found))
  
# airbnb2018$revised_year <- as.numeric(strapplyc( as.character(airbnb2018$revised), "([0-9]*).*", simplify = TRUE))
# airbnb2018$revised_month <- as.numeric(strapplyc( as.character(airbnb2018$revised), "[0-9]*-([0-9]*).*", simplify = TRUE))
# airbnb2018$revised_day <- as.numeric(strapplyc( as.character(airbnb2018$revised), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE))
airbnb2018$revised_date <- as.Date(as.POSIXct(airbnb2018$revised))

airbnb2018_jun$found_date <- as.Date(as.POSIXct(airbnb2018_jun$found))
airbnb2018_jun$revised_date <- as.Date(as.POSIXct(airbnb2018_jun$revised))

airbnb2018_sep$found_date <- as.Date(as.POSIXct(airbnb2018_sep$found))
airbnb2018_sep$revised_date <- as.Date(as.POSIXct(airbnb2018_sep$revised))
# airbnb2018_sep$lastreview_date  <- as.Date( paste(
#   as.numeric(strapplyc( as.character(airbnb2018_sep$lastreview_20181127), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE)),
#   "/",
#   as.numeric(strapplyc( as.character(airbnb2018_sep$lastreview_20181127), "[0-9]*-([0-9]*).*", simplify = TRUE)),
#   "/",
#   as.numeric(strapplyc( as.character(airbnb2018_sep$lastreview_20181127), "([0-9]*).*", simplify = TRUE)),
#   sep = "" ), "%d/%m/%Y")
airbnb2018_sep$lastreview_date  <- as.Date( paste(
  as.numeric(strapplyc( as.character(airbnb2018_sep$lastreview_20180912), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE)),
  "/",
  as.numeric(strapplyc( as.character(airbnb2018_sep$lastreview_20180912), "[0-9]*-([0-9]*).*", simplify = TRUE)),
  "/",
  as.numeric(strapplyc( as.character(airbnb2018_sep$lastreview_20180912), "([0-9]*).*", simplify = TRUE)),
  sep = "" ), "%d/%m/%Y")

# Calculate time active 1 ----
airbnb2018$time_active <- airbnb2018$revised_date - airbnb2018$found_date
airbnb2018_jun$time_active <- airbnb2018_jun$revised_date - airbnb2018_jun$found_date
airbnb2018_sep$time_active <- airbnb2018_sep$revised_date - airbnb2018_sep$found_date

airbnb2018_sep$review_in_2018 <- "no"
airbnb2018_sep[airbnb2018_sep$lastreview_date > "2018-01-01" & !is.na(airbnb2018_sep$lastreview_date),]$review_in_2018 <- "sí"
airbnb2018_sep$review_in_2018 <- as.factor(airbnb2018_sep$review_in_2018)

# Calculate the max date between revised and last review (time_active2)
# to calculate aa new "active time" line end
airbnb2018_sep$max.date <- as.Date(apply(airbnb2018_sep[,c("revised_date","lastreview_date")],1,max))
airbnb2018_sep[is.na(airbnb2018_sep$max.date),]$max.date <- airbnb2018_sep[is.na(airbnb2018_sep$max.date),]$revised_date
# nrow(airbnb2018_sep[airbnb2018_sep$max.date > "2018-07-31",])
# nrow(airbnb2018_sep[airbnb2018_sep$revised_date > "2018-07-31",])
airbnb2018_sep$time_active2 <- airbnb2018_sep$max.date  - airbnb2018_sep$found_date


# Order listings ---------
# Ordena listings por tiempo activo
airbnb2018_sep <- airbnb2018_sep[order(airbnb2018_sep$time_active),]
# mete orden en variable "order"
airbnb2018_sep$order <- 1:nrow(airbnb2018_sep)

# Ordena listings por tiempo activo 2
airbnb2018_sep <- airbnb2018_sep[order(airbnb2018_sep$time_active2),]
# mete orden en variable "order"
airbnb2018_sep$order <- 1:nrow(airbnb2018_sep)

# Ordena listings por max.date
airbnb2018_sep <- airbnb2018_sep[order(airbnb2018_sep$max.date),]
# mete orden en variable "order"
airbnb2018_sep$order <- nrow(airbnb2018_sep):1

# Ordena listings por número reviews
airbnb2018_sep <- airbnb2018_sep[order(airbnb2018_sep$reviews),]
# mete orden en variable "order"
airbnb2018_sep$order <- 1:nrow(airbnb2018_sep)

# Ordena listings por si existe URL o no
airbnb2018_sep <- airbnb2018_sep[order(airbnb2018_sep$exists_20181127),]
# mete orden en variable "order"
airbnb2018_sep$order <- 1:nrow(airbnb2018_sep)

# airbnb2018 <- airbnb2018[order(airbnb2018$time_active),]
# # mete orden en variable "order"
# airbnb2018$order <- 1:nrow(airbnb2018)

# mete datos de orden arriba calculados en las bases de datos anteriores
airbnb2018 <- merge(airbnb2018, airbnb2018_sep[,c("url","order")], by.x="url", by.y="url")
airbnb2018_jun <- merge(airbnb2018_jun, airbnb2018_sep[,c("url","order")], by.x="url", by.y="url")

# Calculate active listings in a given month -----
# Date of last datahippo scraping: 2018-09-26. September 2018
date_selected <- as.Date("2018-07-31") #select date
# format(date_selected,"%m")
# counts number of listings considered "active" for September
nrow(airbnb2018_sep[airbnb2018_sep$max.date > date_selected,]) 
# ---------------------------------------------------------------------------

# Análisis de anuncios activos: desde cuándo fue encontrado hasta última vez que fue "revisado" 1 ----
png(filename="images/airbnb/activos/activos-airbnb-donostia-revised-to-last-review_2.png",width = 800,height = 3800)
ggplot() +
  geom_segment(data = airbnb2018_sep, aes(
    x= airbnb2018_sep$found_date,
    xend= airbnb2018_sep$max.date,
    y= airbnb2018_sep$order,
    yend=airbnb2018_sep$order
  ),
  size=0.5, alpha = 1,color = "#BBFFEE") +
  geom_segment(data = airbnb2018_sep, aes(
    x= airbnb2018_sep$revised_date,
    xend= airbnb2018_sep$lastreview_date,
    y= airbnb2018_sep$order,
    yend=airbnb2018_sep$order
    ),
    color = "#74bbde",
    arrow = arrow(length = unit(0.1,"cm")),
    size=0.5, alpha = 1) +
geom_point(data = airbnb2018_sep, aes(
    x=airbnb2018_sep$revised_date,
    y=airbnb2018_sep$order),
    size=0.2
    ) +
# geom_segment(data = airbnb2018_jun, aes(
#   x= airbnb2018_jun$found_date,
#   xend= airbnb2018_jun$revised_date,
#   y= airbnb2018_jun$order,
#   yend=airbnb2018_jun$order
# ),size=0.5, alpha = 1, color = "#FFAA44") +
# geom_segment(data = airbnb2018, aes(
#   x= airbnb2018$found_date,
#   xend= airbnb2018$revised_date,
#   y= airbnb2018$order,
#   yend= airbnb2018$order
# ),size=0.5, alpha = 1, color = "#FF77CC") +
theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
    # axis.text.y = element_blank()
  ) +
  scale_x_date(date_minor_breaks = "1 month",limits = c(as.Date("2017-8-1"), as.Date("2018-11-1"))) +
  ylim(0,nrow(airbnb2018_sep)) +
  labs(title = "From revised (2018-09-26) to last review (scraping 2018-11-27)",
       subtitle = "Donostia. Orden por tiempo activos.",
       y = "",
       x = "fecha",
       caption = "Efecto Airbnb. Datos: Septiembre 2018. Datahippo.org") 
dev.off()

# Anuncios que tienen fecha de revised de septiembre 2017 pero que su URL sigue viva ------
airbnb2018_sep[airbnb2018_sep$revised_date < "2017-10-01" & airbnb2018_sep$exists_20181127 == "Existe" & 
                 ( airbnb2018_sep$lastreview_date > "2018-01-01" & !is.na(airbnb2018_sep$lastreview_date)),]

table(airbnb2018_sep[airbnb2018_sep$revised_date < "2017-10-01",]$exists_20181127)

airbnb2018_sep[airbnb2018_sep$url == "https://www.airbnb.com/rooms/6982731",]

# Análisis de anuncios activos: desde cuándo fue encontrado hasta última vez que fue "revisado" orden tipo listing ----
airbnb2018_sep <- airbnb2018_sep[order(airbnb2018_sep$time_active,airbnb2018_sep$room_type),]

png(filename="images/airbnb/activos/activos-airbnb-donostia-room_type-180926.png",width = 800,height = 3800)
ggplot() +
  geom_segment(data = airbnb2018_sep, aes(
    x= airbnb2018_sep$found_date,
    xend= airbnb2018_sep$revised_date,
    y= order(airbnb2018_sep$time_active, airbnb2018_sep$room_type),
    yend= order(airbnb2018_sep$time_active, airbnb2018_sep$room_type),
    color = room_type
  ),size=0.5, alpha = 1 ) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
    # axis.text.y = element_blank()
  ) +
  scale_x_date(limits = c(as.Date("2017-8-1"), as.Date("2018-10-1"))) +
  labs(title = "de cuándo fueron encontrados a revisados",
       subtitle = "Fechas de 'found' y 'revised' en Donostia. Orden por tiempo activos",
       y = "id",
       x = "fecha",
       caption = "Efecto Airbnb. Datos: Septiembre 2018. Datahippo.org") 
dev.off()

# Coloreando por número de anuncios de los hosts----
# Calculate number of listings and accomodates by host.id
n_alojamientos <- airbnb2018_sep %>% group_by(host.id) %>% summarize(alojamientos = n(),plazas = sum(capacity) ) %>% arrange(desc(alojamientos))

airbnb2018_sep <- merge(airbnb2018_sep, n_alojamientos[,c("host.id","alojamientos","plazas")], by.x="host.id", by.y="host.id")

# Ordena por tiempo activo (y en segunda instancia por número de alojamientos, 
# para juntar todos los anuncios de los top hosts)
airbnb2018 <- airbnb2018[order(airbnb2018$time_active,airbnb2018$alojamientos),]
airbnb2018_sep <- airbnb2018_sep[order(airbnb2018_sep$time_active,airbnb2018_sep$alojamientos),]
# mete orden en variable
airbnb2018_sep$order <- 1:nrow(airbnb2018_sep)

# plot tiempo activo 1 ------
active <- airbnb2018_sep
active <- airbnb2018

png(filename="images/airbnb/activos/found-to-revised-airbnb-donostia-comparacion_n-alojamientos-3gg_hosts-mas-de-50-4_abril.png",width = 600,height = 10000)
ggplot() +
  geom_segment(data = active, aes(
    x= active$found_date,
    xend= active$revised_date,
    y= active$order,
    yend=active$order
  ),size=1, alpha = 1, color="#FFEE33" ) +
  geom_segment(data = active[active$alojamientos>4,], aes(
    x= active[active$alojamientos>4,]$found_date,
    xend= active[active$alojamientos>4,]$revised_date,
    y= active[active$alojamientos>4,]$order,
    yend=active[active$alojamientos>4,]$order
  ),size=1, alpha = 1, color="#FFAA00" ) +
  geom_segment(data = active[active$alojamientos>50,], aes(
    x= active[active$alojamientos>50,]$found_date,
    xend= active[active$alojamientos>50,]$revised_date,
    y= active[active$alojamientos>50,]$order,
    yend=active[active$alojamientos>50,]$order
  ),size=1, alpha = 1, color="#FF0000" ) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_x_date(limits = c(as.Date("2017-8-1"), as.Date("2018-10-1"))) +
  labs(title = "De cuándo fueron encontrados a revisados por última vez",
       subtitle = "Fechas de 'found' y 'revised' en Donostia. Orden por tiempo activos. Rojo > 50 anuncios del host, Naranja > 4, Amarillo = 1",
       y = "id",
       x = "fecha",
       caption = "Efecto Airbnb. Datos: Septiembre 2018. Datahippo.org") 
dev.off()

# plot tiempo activo 2 -----
png(filename="images/airbnb/activos/found-to-revised-airbnb-donostia-comparacion_n-alojamientos.png",width = 600,height = 10000)
ggplot() +
  geom_point(data = airbnb2018_sep, 
             aes(
    x= airbnb2018_sep$time_active,
    y= airbnb2018_sep$alojamientos
  ),size=1, alpha = 0.4, color="#000000" ) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  # theme(
  #   # panel.grid.major.y = element_blank(),
  #   # panel.grid.minor.y = element_blank(),
  #   # axis.text.y = element_blank()
  # ) +
  labs(title = "De cuándo fueron encontrados a revisados por última vez",
       subtitle = "Fechas de 'found' y 'revised' en Donostia.",
       y = "nº alojamientos de host",
       x = "días activos",
       caption = "Efecto Airbnb. Datos: Septiembre 2018. Datahippo.org") 
dev.off()


# Find anuncios activos (revisados) después de 2018-01-01 -----
nrow(airbnb2018)
nrow(airbnb2018[airbnb2018$revised_date > "2017-12-31",])
nrow(airbnb2018[airbnb2018$revised_date < "2017-12-31",])

nrow(airbnb2018[airbnb2018$revised_date > "2018-01-31",])
nrow(airbnb2018[airbnb2018$revised_date > "2018-02-28",])
nrow(airbnb2018[airbnb2018$revised_date > "2018-03-31",])

# Pisos encontrados en la plataforma desde primero de abril: 264
nrow(airbnb2018[airbnb2018$revised_date > "2018-04-01",])

# Preparing to plot
found_dates <- as.data.frame(table(airbnb2018$found_date))
colnames(found_dates ) <- c("date","freq")

revised_dates <- as.data.frame(table(airbnb2018$revised_date))
colnames(revised_dates) <- c("date","freq")

png(filename="images/airbnb/activos/found-dates-airbnb-donostia-1804.png",width = 600,height = 400)
ggplot(found_dates,aes(x=date,y=freq)) +
  geom_bar(stat="identity") +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(title = "¿Cuándo fueron encontrados por primera vez en el scraping de Datahippo?",
       subtitle = "Fechas de 'found' en Donostia",
       y = "Nº de listings",
       x = "fechas",
       caption = "Efecto Airbnb. Datos: Abril 2018. Datahippo.org")  +
  coord_flip()
dev.off()

png(filename="images/airbnb/activos/revised-dates-airbnb-donostia-1804.png",width = 600,height = 400)
ggplot(revised_dates,aes(x=date,y=freq)) +
  geom_bar(stat="identity") +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(title = "¿Cuándo fueron encontrados por última vez en el scraping de Datahippo?",
       subtitle = "Fechas de 'revised' en Donostia",
       y = "Nº de listings",
       x = "fechas",
       caption = "Efecto Airbnb. Datos: Abril 2018. Datahippo.org")  +
  coord_flip()
dev.off()


plot(airbnb2018$id,airbnb2018$found_date)

# --- 09-2018 -----
# Parse dates
airbnb2018_jun$found_year <- as.numeric(strapplyc( as.character(airbnb2018_jun$found), "([0-9]*).*", simplify = TRUE))
airbnb2018_jun$found_month <- as.numeric(strapplyc( as.character(airbnb2018_jun$found), "[0-9]*-([0-9]*).*", simplify = TRUE))
airbnb2018_jun$found_day <- as.numeric(strapplyc( as.character(airbnb2018_jun$found), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE))
airbnb2018_jun$found_date <- as.Date( paste(airbnb2018_jun$found_day,"/",airbnb2018_jun$found_month,"/",airbnb2018_jun$found_year,sep = "" ), "%d/%m/%Y")

airbnb2018_jun$revised_year <- as.numeric(strapplyc( as.character(airbnb2018_jun$revised), "([0-9]*).*", simplify = TRUE))
airbnb2018_jun$revised_month <- as.numeric(strapplyc( as.character(airbnb2018_jun$revised), "[0-9]*-([0-9]*).*", simplify = TRUE))
airbnb2018_jun$revised_day <- as.numeric(strapplyc( as.character(airbnb2018_jun$revised), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE))
airbnb2018_jun$revised_date <- as.Date( paste(airbnb2018_jun$revised_day,"/",airbnb2018_jun$revised_month,"/",airbnb2018_jun$revised_year,sep = "" ), "%d/%m/%Y")

# Find anuncios activos (revisados) después de 2018-01-01
nrow(airbnb2018_jun)
nrow(airbnb2018_jun[airbnb2018_jun$revised_date > "2017-12-31",])
nrow(airbnb2018_jun[airbnb2018_jun$revised_date < "2017-12-31",])

nrow(airbnb2018_jun[airbnb2018_jun$revised_date > "2018-01-31",])
nrow(airbnb2018_jun[airbnb2018_jun$revised_date > "2018-02-28",])
nrow(airbnb2018_jun[airbnb2018_jun$revised_date > "2018-03-31",])
nrow(airbnb2018_jun[airbnb2018_jun$revised_date > "2018-05-31",])
nrow(airbnb2018_jun[airbnb2018_jun$revised_date > "2018-06-01",])

# Pisos encontrados en la plataforma desde primero de abril: 1.437
nrow(airbnb2018_jun)
nrow(airbnb2018_jun[airbnb2018_jun$revised_date > "2018-04-01",])
table(airbnb2018_jun[airbnb2018_jun$revised_date > "2018-04-01",]$room_type)

found_dates <- as.data.frame(table(airbnb2018_jun$found_date))
colnames(found_dates ) <- c("date","freq")

revised_dates <- as.data.frame(table(airbnb2018_jun$revised_date))
colnames(revised_dates) <- c("date","freq")

png(filename="images/airbnb/activos/found-dates-airbnb-donostia-1806.png",width = 600,height = 400)
ggplot(found_dates,aes(x=date,y=freq)) +
  geom_bar(stat="identity") +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(title = "¿Cuándo fueron encontrados por primera vez en el scraping de Datahippo?",
       subtitle = "Fechas de 'found' en Donostia",
       y = "Nº de listings",
       x = "fechas",
       caption = "Efecto Airbnb. Datos: Junio 2018. Datahippo.org")  +
  coord_flip()
dev.off()

png(filename="images/airbnb/activos/revised-dates-airbnb-donostia-1806.png",width = 600,height = 400)
ggplot(revised_dates,aes(x=date,y=freq)) +
  geom_bar(stat="identity") +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(title = "¿Cuándo fueron encontrados por última vez en el scraping de Datahippo?",
       subtitle = "Fechas de 'revised' en Donostia",
       y = "Nº de listings",
       x = "fechas",
       caption = "Efecto Airbnb. Datos: Junio 2018. Datahippo.org")  +
  coord_flip()
dev.off()

plot(airbnb2018_jun$id,airbnb2018_jun$found_date)

# --- 09-2018 Euskadi -----
# Parse dates
airbnb2018_eus_jun$found_year <- as.numeric(strapplyc( as.character(airbnb2018_eus_jun$found), "([0-9]*).*", simplify = TRUE))
airbnb2018_eus_jun$found_month <- as.numeric(strapplyc( as.character(airbnb2018_eus_jun$found), "[0-9]*-([0-9]*).*", simplify = TRUE))
airbnb2018_eus_jun$found_day <- as.numeric(strapplyc( as.character(airbnb2018_eus_jun$found), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE))
airbnb2018_eus_jun$found_date <- as.Date( paste(airbnb2018_eus_jun$found_day,"/",airbnb2018_eus_jun$found_month,"/",airbnb2018_eus_jun$found_year,sep = "" ), "%d/%m/%Y")

airbnb2018_eus_jun$revised_year <- as.numeric(strapplyc( as.character(airbnb2018_eus_jun$revised), "([0-9]*).*", simplify = TRUE))
airbnb2018_eus_jun$revised_month <- as.numeric(strapplyc( as.character(airbnb2018_eus_jun$revised), "[0-9]*-([0-9]*).*", simplify = TRUE))
airbnb2018_eus_jun$revised_day <- as.numeric(strapplyc( as.character(airbnb2018_eus_jun$revised), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE))
airbnb2018_eus_jun$revised_date <- as.Date( paste(airbnb2018_eus_jun$revised_day,"/",airbnb2018_eus_jun$revised_month,"/",airbnb2018_eus_jun$revised_year,sep = "" ), "%d/%m/%Y")

# Find anuncios activos (revisados) después de 2018-01-01
nrow(airbnb2018_eus_jun)
nrow(airbnb2018_eus_jun[airbnb2018_eus_jun$revised_date > "2017-12-31",])
nrow(airbnb2018_eus_jun[airbnb2018_eus_jun$revised_date < "2017-12-31",])

nrow(airbnb2018_eus_jun[airbnb2018_eus_jun$revised_date > "2018-01-31",])
nrow(airbnb2018_eus_jun[airbnb2018_eus_jun$revised_date > "2018-02-28",])
nrow(airbnb2018_eus_jun[airbnb2018_eus_jun$revised_date > "2018-03-31",])
nrow(airbnb2018_eus_jun[airbnb2018_eus_jun$revised_date > "2018-05-31",])
nrow(airbnb2018_eus_jun[airbnb2018_eus_jun$revised_date > "2018-06-01",])


plot(table(airbnb2018_eus_jun$revised_date))
test <- as.data.frame(table(airbnb2018_eus_jun$revised_date))
colnames(test) <- c("date","freq")

ggplot(test,aes(x=date,y=freq)) +
  geom_bar(stat="identity") +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(title = "Fechas de 'revised' de listings de Airbnb en Euskadi",
       subtitle = "",
       y = "Number of listings",
       x = "dates",
       caption = "Efecto Airbnb. Data: Datahippo.org")  +
  coord_flip()
