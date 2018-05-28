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
listings_id <- data.frame(unique(airbnb$id)) # there is no need to use unique
write.csv(listings_id, file = "data/output/180423_airbnb-listings-id-donostia_datahippo.csv",row.names = FALSE)


# Step in Python: scrap information based on listings id. View script: https://github.com/montera34/airbnbeuskadi/blob/master/scraping/airbnb.lastreview.py
# Scraping 1.
# listings id have to be im the following format for a list in python [33333,22313112,435345,2342534]

# Load result from web scraper
airbnblistings <- read.delim("scraping/180423_listings-airbnb-donostia_datahippo_with-last-review_2.csv",sep = ",")

names(airbnblistings) <- c("id","n_reviews","last_review")

airbnb$id <-  sub("airbnb", "", airbnb$id )

# If some listings are not in the scraped final output we should recount which listings are not in the list
listings_faltan <- filter(airbnb, ! airbnb$id %in% airbnblistings$id)
# we wirte a list of listings needed to re/scrape and go to python again
write.csv(listings_faltan$id, file = "scraping/listings_faltan.csv",row.names = FALSE)

airbnblistings2 <- read.delim("scraping/180423_listings-airbnb-donostia_datahippo_with-last-review_3.csv",sep = ",")
names(airbnblistings2) <- c("id","n_reviews","last_review")

# Join second scraping with second one
lastreview_merged <- rbind(airbnblistings,airbnblistings2)

# We have 84.4% from all the listings
nrow(lastreview_merged) / nrow (airbnb) * 100

# If some listings are not in the scraped final output we should recount which listings are not in the list
listings_faltan2 <- filter(airbnb, ! airbnb$id %in% lastreview_merged$id)
# we wirte a list of listings needed to re/scrape and go to python again
write.csv(listings_faltan2$id, file = "scraping/listings_faltan2.csv",row.names = FALSE)

airbnblistings3 <- read.delim("scraping/180423_listings-airbnb-donostia_datahippo_with-last-review_4.csv",sep = ",")
names(airbnblistings3) <- c("id","n_reviews","last_review")

lastreview_merged <- rbind(lastreview_merged,airbnblistings3)

airbnblistings4 <- read.delim("scraping/180423_listings-airbnb-donostia_datahippo_with-last-review_5.csv",sep = ",")
names(airbnblistings3) <- c("id","n_reviews","last_review")

lastreview_merged <- rbind(lastreview_merged,airbnblistings4)

# Loads libraries
library(rvest)
library(stringr)
library(gsubfn) # select text in the parenthesis with regex

# Extracts month from last review 
lastreview_merged$monthlastreview  <-  strapplyc( as.character(lastreview_merged$last_review), "(.*) [0-9]*", simplify = TRUE)

# Converts month string name to number
# airbnblistings[airbnblistings$monthlastreview=="January",]$monthlastreview <- "1"
# airbnblistings[airbnblistings$monthlastreview=="February",]$monthlastreview <- "2"
# airbnblistings[airbnblistings$monthlastreview=="March",]$monthlastreview <- "3"
# airbnblistings[airbnblistings$monthlastreview=="April",]$monthlastreview <- "4"
# airbnblistings[airbnblistings$monthlastreview=="May",]$monthlastreview <- "5"
# airbnblistings[airbnblistings$monthlastreview=="June",]$monthlastreview <- "6"
# airbnblistings[airbnblistings$monthlastreview=="July",]$monthlastreview <- "7"
# airbnblistings[airbnblistings$monthlastreview=="August",]$monthlastreview <- "8"
# airbnblistings[airbnblistings$monthlastreview=="September",]$monthlastreview <- "9"
# airbnblistings[airbnblistings$monthlastreview=="October",]$monthlastreview <- "10"
# airbnblistings[airbnblistings$monthlastreview=="November",]$monthlastreview <- "11"
# airbnblistings[airbnblistings$monthlastreview=="December",]$monthlastreview <- "12"
lastreview_merged[lastreview_merged$monthlastreview=="Enero de",]$monthlastreview <- "1"
lastreview_merged[lastreview_merged$monthlastreview=="Febrero de",]$monthlastreview <- "2"
lastreview_merged[lastreview_merged$monthlastreview=="Marzo de",]$monthlastreview <- "3"
lastreview_merged[lastreview_merged$monthlastreview=="Abril de",]$monthlastreview <- "4"
lastreview_merged[lastreview_merged$monthlastreview=="Mayo de",]$monthlastreview <- "5"
lastreview_merged[lastreview_merged$monthlastreview=="Junio de",]$monthlastreview <- "6"
lastreview_merged[lastreview_merged$monthlastreview=="Julio de",]$monthlastreview <- "7"
lastreview_merged[lastreview_merged$monthlastreview=="Agosto de",]$monthlastreview <- "8"
lastreview_merged[lastreview_merged$monthlastreview=="Septiembre de",]$monthlastreview <- "9"
lastreview_merged[lastreview_merged$monthlastreview=="Octubre de",]$monthlastreview <- "10"
lastreview_merged[lastreview_merged$monthlastreview=="Noviembre de",]$monthlastreview <- "11"
lastreview_merged[lastreview_merged$monthlastreview=="Diciembre de",]$monthlastreview <- "12"

lastreview_merged$monthlastreview <- as.numeric(lastreview_merged$monthlastreview)

# Extracts year from last review
lastreview_merged$yearlastreview <- as.numeric(strapplyc( as.character(lastreview_merged$last_review), ".* ([0-9]*)", simplify = TRUE))

# removes "airbnb" in id variable to allow join 
airbnb$id <- sub("airbnb", "", airbnb$id )

airbnb_merged <- merge(airbnb,lastreview_merged, by.x="id", by.y="id")

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

