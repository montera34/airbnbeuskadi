# Este script filtra una base de datos de Airbnb para seleccionar los listings activos
# Se basa en datos de datahippo.org y de scraping posterior para obener fecha de last review

# ---- Load libraries -----
library(tidyverse)
library(gsubfn) # select text in the parenthesis with regex

# ------ Load files ----------
airbnb2018_sep <- read.delim("data/output/listings-airbnb_donostia_datahippo_with-last-review-20181127-reviewed.csv",sep = ",")
airbnb2018_sep$exists_20181127 <- as.factor(airbnb2018_sep$exists_20181127)
levels(airbnb2018_sep$exists_20181127) <- c("No existe","Existe")

# airbnb2018_sep <- read.delim("data/output/180604_listings-airbnb-donostia_datahippo_with-last-review-20180912-reviewed.csv",sep = ",")
# airbnb2018_sep$exists_20180912 <- as.factor(airbnb2018_sep$exists_20180912)
# levels(airbnb2018_sep$exists_20180912) <- c("No existe","Existe")

# Rename room type
levels(airbnb2018_sep$room_type) <- c("Vivienda completa","Habitación privada","Habitación compartida")

# ----- Anuncios activos parse dates-----
airbnb2018_sep$found_date <- as.Date(as.POSIXct(airbnb2018_sep$found))
airbnb2018_sep$revised_date <- as.Date(as.POSIXct(airbnb2018_sep$revised))
# airbnb2018_sep$lastreview_date  <- as.Date( paste(
#   as.numeric(strapplyc( as.character(airbnb2018_sep$lastreview_20180912), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE)),
#   "/",
#   as.numeric(strapplyc( as.character(airbnb2018_sep$lastreview_20180912), "[0-9]*-([0-9]*).*", simplify = TRUE)),
#   "/",
#   as.numeric(strapplyc( as.character(airbnb2018_sep$lastreview_20180912), "([0-9]*).*", simplify = TRUE)),
#   sep = "" ), "%d/%m/%Y")
airbnb2018_sep$lastreview_date  <- as.Date( paste(
  as.numeric(strapplyc( as.character(airbnb2018_sep$lastreview_20181127), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE)),
  "/",
  as.numeric(strapplyc( as.character(airbnb2018_sep$lastreview_20181127), "[0-9]*-([0-9]*).*", simplify = TRUE)),
  "/",
  as.numeric(strapplyc( as.character(airbnb2018_sep$lastreview_20181127), "([0-9]*).*", simplify = TRUE)),
  sep = "" ), "%d/%m/%Y")

# Reqrite factor for "exists URL"
airbnb2018_sep$review_in_2018 <- "no"
airbnb2018_sep[airbnb2018_sep$lastreview_date > "2018-01-01" & !is.na(airbnb2018_sep$lastreview_date),]$review_in_2018 <- "sí"
airbnb2018_sep$review_in_2018 <- as.factor(airbnb2018_sep$review_in_2018)

# Calculate the max date between revised and last review (time_active2) -------
# to calculate aa new "active time" line end
airbnb2018_sep$max.date <- as.Date(apply(airbnb2018_sep[,c("revised_date","lastreview_date")],1,max))
airbnb2018_sep[is.na(airbnb2018_sep$max.date),]$max.date <- airbnb2018_sep[is.na(airbnb2018_sep$max.date),]$revised_date
# nrow(airbnb2018_sep[airbnb2018_sep$max.date > "2018-07-31",])
# nrow(airbnb2018_sep[airbnb2018_sep$revised_date > "2018-07-31",])
airbnb2018_sep$time_active2 <- airbnb2018_sep$max.date  - airbnb2018_sep$found_date

# Order listings ---------
# Ordena listings por tiempo activo
# airbnb2018_sep <- airbnb2018_sep[order(airbnb2018_sep$time_active),]
# mete orden en variable "order"
# airbnb2018_sep$order <- 1:nrow(airbnb2018_sep)

# Ordena listings por tiempo activo 2
# airbnb2018_sep <- airbnb2018_sep[order(airbnb2018_sep$time_active2),]
# mete orden en variable "order"
# airbnb2018_sep$order <- 1:nrow(airbnb2018_sep)

# Ordena listings por max.date
airbnb2018_sep <- airbnb2018_sep[order(airbnb2018_sep$max.date),]
# mete orden en variable "order"
airbnb2018_sep$order <- nrow(airbnb2018_sep):1

# Calculate active listings in a given month -----
# Date of last datahippo scraping: 2018-09-26. September 2018
date_selected <- as.Date("2018-07-31") #select date
# Date of last datahippo scraping: 2018-06-4. September 2018
# date_selected <- as.Date("2018-04-30") #select date
# format(date_selected,"%m")
# counts number of listings considered "active" for September
nrow(airbnb2018_sep[airbnb2018_sep$max.date > date_selected,])
active_listings <- airbnb2018_sep[airbnb2018_sep$max.date > date_selected,]
# table(active_listings$room_type)
# table(airbnb2018_sep$lastreview_date)
# table(airbnb2018_sep$exists_20180912)

write.csv(active_listings, file = "data/output/temp/180926_listings-airbnb_donostia_datahippo_with-last-review-20181127-reviewed_active-filtered.csv", row.names = FALSE)