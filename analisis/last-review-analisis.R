# analisis last review para activos

# ---- Load libraries -----
library(tidyverse)
# read shapes
library(gsubfn) # select text in the parenthesis with regex

# Load data
last_review  <- read.delim("data/output/180604_listings-airbnb-donostia_datahippo_with-last-review-180911.csv",sep = ",")
last_review  <- read.delim("data/output/180604_listings-airbnb-donostia_datahippo_with-last-review-180911-reviewed.csv",sep = ",")

last_review  <- read.delim("data/output/180618_listings-airbnb-bilbao_datahippo.csv_with-last-review-180912.csv",sep = ",")
last_review  <- read.delim("data/output/180618_listings-airbnb-bilbao_datahippo.csv_with-last-review-180912-reviewed.csv",sep = ",")


# Prepare data
# Create date format for revised
last_review$revised_date  <- as.Date( paste(
  as.numeric(strapplyc( as.character(last_review$revised), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE)),
  "/",
  as.numeric(strapplyc( as.character(last_review$revised), "[0-9]*-([0-9]*).*", simplify = TRUE)),
  "/",
  as.numeric(strapplyc( as.character(last_review$revised), "([0-9]*).*", simplify = TRUE)),
  sep = "" ), "%d/%m/%Y")

# Create date format for last review
last_review$lastreview_date  <- as.Date( paste(
  as.numeric(strapplyc( as.character(last_review$lastreview_20180912), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE)),
  "/",
  as.numeric(strapplyc( as.character(last_review$lastreview_20180912), "[0-9]*-([0-9]*).*", simplify = TRUE)),
  "/",
  as.numeric(strapplyc( as.character(last_review$lastreview_20180912), "([0-9]*).*", simplify = TRUE)),
  sep = "" ), "%d/%m/%Y")

# Número total de anuncios
nrow(last_review)

# Número de disponibles para alquilar a partir de fecha
nrow(last_review[last_review$revised_date > "2018-03-31",])
nrow(last_review[last_review$revised_date < "2018-03-31",])

# Cuántos pisos existe su URL
table(last_review$exists_20180911)
table(last_review$exists_20180912)
# 0    1 
# 903 1303
nrow(last_review[last_review$exists_20180911 == 1,])
nrow(last_review[last_review$exists_20180912 == 1,])


# cuenta los anuncios con review posterior a una fecha, que exite su URL y que su last_review no es 0 
# (esto es, que el scraping ha funcionado y captirado esa fecha)
nrow(last_review[last_review$lastreview_date > "2018-03-31" & 
                   last_review$exists_20180911 == 1 & 
                   last_review$lastreview_20180911 != "0"
                 ,])

nrow(last_review[last_review$lastreview_date > "2018-03-31" & 
                   last_review$exists_20180912 == 1 & 
                   last_review$lastreview_20180912 != "0"
                 ,])



# De los pisos que existen ¿cuales son habitaciones o pisos completos?
table(last_review[last_review$exists_20180912==1,]$room_type)
# entire_home private_room  shared_room 
# 958          339            6

# De los pisos que existen ¿cuáles tuvieron su última review después de marzo 2018?
table(last_review[last_review$exists_20180912==1 & last_review$lastreview_date > "2018-09-01",]$room_type)
# entire_home private_room  shared_room 
# 753          228            3


# Detectar los pisos que tienen reviews seg'un datahippo y no tienen reviews segun el scraping
nrow(last_review[last_review$reviews > 0 & last_review$reviews_20180911 == 0 & last_review$exists_20180911 ==1 ,])
last_review[last_review$reviews > 0 & last_review$reviews_20180911 == 0 & last_review$exists_20180911 ==1 ,]

last_review$dif_reviews <- last_review$reviews_20180911 - last_review$reviews
plot(last_review[last_review$exists_20180911==1,]$dif_reviews)

