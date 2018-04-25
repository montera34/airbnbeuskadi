library(tidyverse)
# loads airbnb listings
airbnb <- read.delim("data/original/180423_listings-airbnb-donostia_datahippo.csv",sep = ",")
# removes que "airbnb" from listing id
airbnb$host.id <-  sub("airbnb", "", airbnb$host.id )

# creates list with unique hostings id
users <- data.frame(unique(airbnb$host.id))
# writes csv with host id listings
write.csv(users, file = "data/output/180423_airbnb-hosts-id-donostia_datahippo.csv",row.names = FALSE)

# creates list with listings id and writes it as csv
listings_id <- data.frame(unique(airbnb$id))
write.csv(listings_id, file = "data/output/180423_airbnb-listings-id-donostia_datahippo.csv",row.names = FALSE)

# Step in Python: scrap information based on listings id. View script: https://wiki.montera34.com/airbnb/recetas#scrapingobtiene-info-a-partir-de-listado-de-id-de-listings-en-airbnb
# listings id have to be im the following format for a list in python [33333,22313112,435345,2342534]

# Load reslut from web scraper
airbnblistings <- read.delim("analisis/180424_airbnb-listings-donostia_scraping-last-review.csv",sep = ",")

# Loads libreries
library(rvest)
library(stringr)
library(gsubfn) # select text in the parenthesis with regex

# Extracts month from last review 
airbnblistings$monthlastreview  <-  strapplyc( as.character(airbnblistings$last_review), "(.*) [0-9]*", simplify = TRUE)
# Converts month string name to number
airbnblistings[airbnblistings$monthlastreview=="January",]$monthlastreview <- "1"
airbnblistings[airbnblistings$monthlastreview=="February",]$monthlastreview <- "2"
airbnblistings[airbnblistings$monthlastreview=="March",]$monthlastreview <- "3"
airbnblistings[airbnblistings$monthlastreview=="April",]$monthlastreview <- "4"
airbnblistings[airbnblistings$monthlastreview=="May",]$monthlastreview <- "5"
airbnblistings[airbnblistings$monthlastreview=="June",]$monthlastreview <- "6"
airbnblistings[airbnblistings$monthlastreview=="July",]$monthlastreview <- "7"
airbnblistings[airbnblistings$monthlastreview=="August",]$monthlastreview <- "8"
airbnblistings[airbnblistings$monthlastreview=="September",]$monthlastreview <- "9"
airbnblistings[airbnblistings$monthlastreview=="October",]$monthlastreview <- "10"
airbnblistings[airbnblistings$monthlastreview=="November",]$monthlastreview <- "11"
airbnblistings[airbnblistings$monthlastreview=="December",]$monthlastreview <- "12"

airbnblistings$monthlastreview <- as.numeric(airbnblistings$monthlastreview)

# Extracts year from last review
airbnblistings$yearlastreview <- as.numeric(strapplyc( as.character(airbnblistings$last_review), ".* ([0-9]*)", simplify = TRUE))

# removes "airbnb" in id variable to allow join 
airbnb$id <- sub("airbnb", "", airbnb$id )

airbnb_merged <- merge(airbnb,airbnblistings, by.x="id", by.y="id")

# Creates date variable for last review. Day date is inviented.
airbnb_merged$date_lastreview <- as.Date( paste("01","/",airbnb_merged$monthlastreview,"/",airbnb_merged$yearlastreview,sep = "" ), "%d/%m/%Y")

length(airbnb_merged[airbnb_merged$date_lastreview > "2017-05-01",]$last_review)

# chow many listings per year?
length(airbnb_merged[airbnb_merged$yearlastreview == "2015",]$last_review)
length(airbnb_merged[airbnb_merged$yearlastreview == "2016",]$last_review)
length(airbnb_merged[airbnb_merged$yearlastreview == "2017",]$last_review)
length(airbnb_merged[airbnb_merged$yearlastreview == "2018",]$last_review)


ggplot(data = airbnb_merged) + 
  geom_bar(mapping = aes(x = yearlastreview, fill=room_type) ) +
  labs(title = "Donostia: Anuncios Airbnb con la última review en año...")


ggplot(data = airbnb_merged[airbnb_merged$yearlastreview == "2016",]) + 
  geom_bar(mapping = aes(x = monthlastreview, fill=room_type) ) +
  labs(title = "Donostia: Anuncios Airbnb con la última review en 2016")

ggplot(data = airbnb_merged[airbnb_merged$yearlastreview == "2017",]) + 
  geom_bar(mapping = aes(x = monthlastreview, fill=room_type) ) +
  labs(title = "Donostia: Anuncios Airbnb con la última review en 2017")

ggplot(data = airbnb_merged[airbnb_merged$yearlastreview == "2018",]) + 
  geom_bar(mapping = aes(x = monthlastreview, fill=room_type) ) +
  labs(title = "Donostia: Anuncios Airbnb con la última review en 2018")


ggplot(data = airbnb_merged) + 
  geom_bar(mapping = aes(x = date_lastreview) ) +
  labs(title = "Donostia: nº de anuncios Airbnb con la última review por mes")
