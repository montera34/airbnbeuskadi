# Análisis de Airbnb en Donostia: anuncios activos

# ---- Load libraries -----
library(tidyverse)
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
airbnb2018_eus_jun <- read.delim("data/original/180604_listings-airbnb-euskadi_datahippo.csv",sep = ",")

# ----- Anuncios activos -----
# --- 04-2018 -----
# Parse dates
airbnb2018$found_year <- as.numeric(strapplyc( as.character(airbnb2018$found), "([0-9]*).*", simplify = TRUE))
airbnb2018$found_month <- as.numeric(strapplyc( as.character(airbnb2018$found), "[0-9]*-([0-9]*).*", simplify = TRUE))
airbnb2018$found_day <- as.numeric(strapplyc( as.character(airbnb2018$found), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE))
airbnb2018$found_date <- as.Date( paste(airbnb2018$found_day,"/",airbnb2018$found_month,"/",airbnb2018$found_year,sep = "" ), "%d/%m/%Y")

airbnb2018$revised_year <- as.numeric(strapplyc( as.character(airbnb2018$revised), "([0-9]*).*", simplify = TRUE))
airbnb2018$revised_month <- as.numeric(strapplyc( as.character(airbnb2018$revised), "[0-9]*-([0-9]*).*", simplify = TRUE))
airbnb2018$revised_day <- as.numeric(strapplyc( as.character(airbnb2018$revised), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE))
airbnb2018$revised_date <- as.Date( paste(airbnb2018$revised_day,"/",airbnb2018$revised_month,"/",airbnb2018$revised_year,sep = "" ), "%d/%m/%Y")

airbnb2018$time_active <- airbnb2018$revised_date - airbnb2018$found_date
summary(airbnb2018$time_active )

# Find anuncios activos (revisados) después de 2018-01-01
nrow(airbnb2018)
nrow(airbnb2018[airbnb2018$revised_date > "2017-12-31",])
nrow(airbnb2018[airbnb2018$revised_date < "2017-12-31",])

nrow(airbnb2018[airbnb2018$revised_date > "2018-01-31",])
nrow(airbnb2018[airbnb2018$revised_date > "2018-02-28",])
nrow(airbnb2018[airbnb2018$revised_date > "2018-03-31",])


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


plot(table(airbnb2018_jun$revised_date))

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
