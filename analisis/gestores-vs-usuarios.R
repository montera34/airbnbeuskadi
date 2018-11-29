# Este script clasifica los listings según su último y primer review, según el tipo de usuario que los posee 

library(gsubfn)
library(tidyverse)

location <- "Donostia"

# Load reviews and listings
# listings  <- read.delim("data/listings_donostia_simple.csv",sep = ",")
listings2017 <- read.delim("data/output/170400_listings-airbnb-donostia_insideairbnb-datahippo_barrio-umenor.csv",sep = ",")
listings2018  <- read.delim("data/output/180423_listings-airbnb-donostia_datahippo_barrio-umenor.csv",sep = ",")

superuser2017 <- listings2017 %>% group_by(host_id)  %>% summarise(count_listings=n())  %>% arrange(desc(count_listings))
superuser2018 <- listings2018 %>% group_by(host.id)  %>% summarise(count_listings=n())  %>% arrange(desc(count_listings))

png(filename="images/airbnb/superuser/airbnb-antfitriones-2017.png",width = 550,height = 600)
ggplot(superuser2017[1:30,],aes(x=reorder(as.character(host_id),count_listings),y=count_listings)) +
  geom_bar(stat="identity") +
  ylim(0, 150) +
  coord_cartesian(ylim = c(0, 130)) +
  coord_flip() +
  theme_minimal(base_family = "Roboto", base_size = 15) +
  # ylab(c(0,120)) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title=element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(title = "Usuarios con más anuncios. Donostia - San Sebastián. 2017",
       subtitle = "",
       y = "anuncios",
       x = "anfitriones",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")
dev.off()

png(filename="images/airbnb/superuser/airbnb-antfitriones-2018.png",width = 550,height = 600)
ggplot(superuser2018[1:30,],aes(x=reorder(as.character(host.id),count_listings),y=count_listings)) +
  geom_bar(stat="identity") +
  ylim(0, 150) +
  coord_flip() +
  theme_minimal(base_family = "Roboto", base_size = 15) +
    theme(
      # panel.grid.minor.x = element_blank(), 
      panel.grid.minor.y = element_blank(),
      # panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.title=element_blank(),
      axis.text.y = element_blank()
    ) +
  labs(title = "Usuarios con más anuncios. Donostia - San Sebastián. 2018",
       subtitle = "",
       y = "anuncios",
       x = "anfitriones",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")
dev.off()

# Create date field for first and last review
listings$first_reviewx <- as.Date(listings$first_review, "%Y-%m-%d")
listings$last_reviewx <- as.Date(listings$last_review, "%Y-%m-%d")
# Calculate time the listing has been active
listings$time_active <- listings$last_reviewx - listings$first_reviewx
# Calculate reviews per day
listings$reviews_per_day <- listings$number_of_reviews / as.integer(listings$time_active)

# Create list of host_id by number of listings
list_hosts <- listings %>% 
  group_by(host_id) %>% 
  summarise(count_listings_host_id=n()) %>% arrange(-count_listings_host_id)

listings <- merge(listings,list_hosts,by="host_id")

plot(listings$calculated_host_listings_count,listings$count_listings_host_id)

# Classify listings by its host type:
listings$type_of_host <- "desconocido"
listings[listings$count_listings_host_id==1,]$type_of_host <- "1 anuncio"
listings[listings$count_listings_host_id==2,]$type_of_host <- "2 anuncios"
listings[listings$count_listings_host_id>3,]$type_of_host <- "multianuncio (>3 anuncios)"

table(listings$type_of_host)

# ----
png(filename="images/airbnb/superuser/puntos-airbnb-superuser-reviews-per-day-2017.png",width = 800,height = 600)
ggplot(listings[!listings$time_active==0,]) +
  geom_point(aes(type_of_host,reviews_per_day),position = "jitter", alpha=0.3)+
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) + 
  labs(title = "Cuántas reviews se dejan por día según tipo de propietario",
       subtitle = "Airbnb en Donostia.",
       y = "nº reviews / días desde que se abrió",
       x = NULL,
       caption = "Datos: Insideairbnb (marzo 2017). Gráfico: lab.montera34.com/airbnb") 
dev.off()

png(filename="images/airbnb/superuser/puntos-airbnb-superuser-reviews-2017.png",width = 800,height = 600)
ggplot(listings) +
  geom_point(aes(type_of_host,number_of_reviews),position = "jitter", alpha=0.3) +
  coord_flip()+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) + 
  labs(title = "Cuántas reviews se dejan según tipo de propietario",
       subtitle = "Airbnb en Donostia.",
       y = "nº reviews desde que se abrió",
       x = NULL,
       caption = "Datos: Insideairbnb (marzo 2017). Gráfico: lab.montera34.com/airbnb")
dev.off()

png(filename="images/airbnb/superuser/puntos-airbnb-superuser-mínimo-numero-noches-2017.png",width = 800,height = 600)
ggplot(listings) +
  geom_point(aes(type_of_host,minimum_nights),position = "jitter", alpha=0.3) +
  coord_flip()+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) + 
  labs(title = "Cuántas reviews se dejan según tipo de propietario",
       subtitle = "Airbnb en Donostia.",
       y = "nº mínimo de noches",
       x = NULL,
       caption = "Datos: Insideairbnb (marzo 2017). Gráfico: lab.montera34.com/airbnb")
dev.off()

tipo_user <- listings %>% group_by(type_of_host) %>% summarise(alojamientos=n())
png(filename="images/airbnb/superuser/barras-airbnb-tipo-usuario-numero-alojamientos-noches-medias-2017.png",width = 800,height = 600)
ggplot(tipo_user,aes(x=type_of_host,y=alojamientos)) +
  geom_bar(stat="identity") +
  coord_flip()+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) + 
  labs(title = "Propiedad de pisos según su tipo de propietario",
       subtitle = "Airbnb en Donostia.",
       y = "alojamientos",
       x = "tipo de host",
       caption = "Datos: Insideairbnb (marzo 2017). Gráfico: lab.montera34.com/airbnb")
dev.off()

tipo_user_2 <- listings %>% group_by(type_of_host,room_type) %>% summarise(alojamientos=n())

ggplot(n,aes(x = reorder(barrios, -pos_ratio2018), y = Value)) +
  geom_bar(aes(fill = Year), position = "dodge", stat="identity")+

png(filename="images/airbnb/superuser/barras-airbnb-tipo-usuario-tipo_room_numero-alojamientos-noches-medias-2017.png",width = 800,height = 600)
ggplot(tipo_user_2,aes(x=type_of_host,y=alojamientos)) +
  geom_bar(aes(fill = room_type), position = "dodge",stat="identity") +
  coord_flip()+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) + 
  labs(title = "Propiedad de pisos según su tipo de propietario",
       subtitle = "Airbnb en Donostia.",
       y = "alojamientos",
       x = "tipo de host",
       caption = "Datos: Insideairbnb (marzo 2017). Gráfico: lab.montera34.com/airbnb")
dev.off()

min_night <- listings %>% group_by(type_of_host) %>% summarise(media=mean(minimum_nights))

png(filename="images/airbnb/superuser/barras-airbnb-tipo-usuario-mínimo-numero-noches-medias-2017.png",width = 800,height = 600)
ggplot(min_night,aes(x=type_of_host,y=media)) +
  geom_bar(stat="identity") +
  coord_flip()+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) + 
  labs(title = "Mínimo número de noches según tipo de propietario",
       subtitle = "Airbnb en Donostia.",
       y = "nº mínimo de noches",
       x = "tipo de host",
       caption = "Datos: Insideairbnb (marzo 2017). Gráfico: lab.montera34.com/airbnb")
dev.off()

# why this not give a 45 degree line??? TODO: Ask Murray
plot(listings$reviews_per_day,listings$reviews_per_month) #not ok
plot(listings$host_listings_count,listings$host_total_listings_count) #ok
plot(listings$host_listings_count,listings$calculated_host_listings_count) #not ok, what is calculated_host_listings_count