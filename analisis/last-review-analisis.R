# analisis last review para activos

# ---- Load libraries -----
library(tidyverse)
# read shapes
library(gsubfn) # select text in the parenthesis with regex

# Load data -------
# last_review  <- read.delim("data/output/180604_listings-airbnb-donostia_datahippo_with-last-review-180911.csv",sep = ",")
last_review  <- read.delim("data/output/180604_listings-airbnb-donostia_datahippo_with-last-review-180911-reviewed.csv",sep = ",")

# last_review  <- read.delim("data/output/180618_listings_airbnb-bilbao_datahippo_with-last-review-20180912.csv",sep = ",")
# last_review  <- read.delim("data/output/180618_listings_airbnb-bilbao_datahippo_with-last-review-20180912-reviewed.csv",sep = ",")

# Create 
results <- data.frame(matrix(ncol = 4 ))
colnames(results) <- c("listings_total","con_fechas_disponibles","found_url","last_review_since")

# Prepare data --------
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

# Create date format for last review
last_review$lastreview_date  <- as.Date( paste(
  as.numeric(strapplyc( as.character(last_review$lastreview_20180911), "[0-9]*-[0-9]*-([0-9]*).*", simplify = TRUE)),
  "/",
  as.numeric(strapplyc( as.character(last_review$lastreview_20180911), "[0-9]*-([0-9]*).*", simplify = TRUE)),
  "/",
  as.numeric(strapplyc( as.character(last_review$lastreview_20180911), "([0-9]*).*", simplify = TRUE)),
  sep = "" ), "%d/%m/%Y")

# Número total de anuncios
nrow(last_review)
results$listings_total <- nrow(last_review)


# Número de disponibles para alquilar a partir de fecha
nrow(last_review[last_review$revised_date > "2018-03-31",])
nrow(last_review[last_review$revised_date < "2018-03-31",])

results$con_fechas_disponibles <- nrow(last_review[last_review$revised_date > "2018-03-31",])

disponibles <- last_review[last_review$revised_date > "2018-03-31",]

# Calcula cuántos pisos tuvieron reviews de los disponibles posteriores a una fecha:
nrow(disponibles[disponibles$lastreview_date > "2018-03-31" & !is.na(disponibles$lastreview_date),]) # posteriores a "2018-03-31"
nrow(disponibles[disponibles$lastreview_date < "2018-03-31" & !is.na(disponibles$lastreview_date),]) # anteriores a "2018-03-31"
nrow(disponibles[disponibles$reviews_20180911 == 0 & disponibles$exists_20180911==0,]) # reviews 0 tras segundo scraping, bien


# Cuántos pisos existe su URL
table(last_review$exists_20180911)
table(last_review$exists_20180912)
# 0    1 
# 903 1303
nrow(last_review[last_review$exists_20180911 == 1,])
nrow(last_review[last_review$exists_20180912 == 1,])

results$found_url <- nrow(last_review[last_review$exists_20180911 == 1,])
existen <- last_review[last_review$exists_20180911 == 1,]

results$found_url <- nrow(last_review[last_review$exists_20180912 == 1,])
existen <- last_review[last_review$exists_20180912 == 1,]

# cuenta los anuncios con review posterior a una fecha (marzo 2018), que exite su URL 
# (esto es, que la URL existe, que el scraping ha funcionado y capturado esa fecha de last review posterior a marzo 2018)
nrow(last_review[last_review$lastreview_date > "2018-03-31" & 
                   last_review$exists_20180911 == 1 &
                   !is.na(last_review$lastreview_date) # se excluyen así los que no tienen fecha de última review (porque no tienen reviews y su valor es NA)
                 ,])

results$last_review_since <- nrow(last_review[last_review$lastreview_date > "2018-03-31" & 
                                                 last_review$exists_20180911 == 1 &
                                                !is.na(last_review$lastreview_date)
                                               ,])

review_posterior <- last_review[last_review$lastreview_date > "2018-03-31" & 
                                  last_review$exists_20180911 == 1 &
                                  !is.na(last_review$lastreview_date)
                                ,]


nrow(last_review[
                   last_review$exists_20180912 == 1 
                 ,])
results$last_review_since <- nrow(last_review[last_review$lastreview_date > "2018-03-31" & 
                                                last_review$exists_20180912 == 1 &
                                                !is.na(last_review$lastreview_date)
                                              ,])

# Resultados de calcular pisos activos
results 

# Resultados en porcentaje
results_per <- round(results/results[1,1], digits =4) *100
results_per


# Cuantos de los anuncios que estaban disponibles cuando el scraping siguen existiendo en septiembre
table(disponibles$url %in% existen$url)

# Cuantos de los anuncios que siguen existiendo en septiembre estaban disponibles cuando el scraping
table(existen$url %in% disponibles$url)
# De los pisos que existen ¿cuales son habitaciones o pisos completos?
table(last_review[last_review$exists_20180911==1,]$room_type)
# entire_home private_room  shared_room 
# 958          339            6

# De los pisos que existen ¿cuáles tuvieron su última review después de marzo 2018?
table(last_review[last_review$exists_20180911==1 & last_review$lastreview_date > "2018-03-31",]$room_type)
# entire_home private_room  shared_room 
# 753          228            3


# Detectar los pisos que tienen reviews segun datahippo y no tienen reviews segun el scraping
nrow(last_review[last_review$reviews > 0 & last_review$reviews_20180911 == 0 & last_review$exists_20180911 ==1 ,])
last_review[last_review$reviews > 0 & last_review$reviews_20180911 == 0 & last_review$exists_20180911 ==1 ,]

# last_review$dif_reviews <- "NA"

last_review$dif_reviews <- last_review$reviews_20180911 - last_review$reviews
last_review[last_review$exists_20180911 ==0,]$dif_reviews <- NA # inserts NA if the do-t existed in september scraping


plot(last_review[last_review$exists_20180911==1,]$dif_reviews)

# Maneras de calcular los que recibieron review a partir de... -------

# A. Abril 2018 (inclusive). Tienen URL activa y recibieron review en abril 2018 o más tarde
nrow(last_review[last_review$lastreview_date > "2018-03-31" & 
                               last_review$exists_20180911 == 1 &
                               !is.na(last_review$lastreview_date) # se excluyen así los que no tienen fecha de última review 
                                                                  # (porque no tienen reviews y su valor es NA)
                 # 1.000 anuncios
                             ,])
table(last_review[last_review$exists_20180911==1 & last_review$lastreview_date > "2018-03-31",]$dif_reviews)

# B. 4 de junio 2018. Tienen URL activa y aumentaron su número de reviews, esto es recibieron review posterior a la fecha del scraping (4 de junio).... 
# la diferencia positiva entre sus reviews del scraping de septiembre y las reviews que tenían en junio es lo que han aumentado
nrow(last_review[last_review$exists_20180911==1 & last_review$dif_reviews > 0 & !is.na(last_review$dif_reviews),])
#                   existe URL                    reviews positivo en periodo             reviews no es NA
# 1.015 anuncios
table(last_review[last_review$exists_20180911==1 & last_review$dif_reviews > 0 & !is.na(last_review$dif_reviews),]$lastreview_date)

# ¿¿¿cuales y cuántos aumentaron su reviews desde el último scraping pero no tuvieron last review después del 4 junio (fecha del scraping)?????
table(last_review[last_review$exists_20180911==1 & last_review$dif_reviews > 0 & 
                    !is.na(last_review$dif_reviews) & last_review$lastreview_date < "2018-06-01",]$revised_date)
test <- last_review[last_review$exists_20180911==1 & last_review$dif_reviews > 0 & 
                      !is.na(last_review$dif_reviews) & last_review$lastreview_date < "2018-06-02",]
table(test$revised_date)
# Parece que fueron 51 ¿cómo puede ser eso?
# Puede ser debido a dos causas:
# Una: que el segundo scraping no haya capturado los last review que estén en otras páginas (paginación) que no sean la primera
# Dos: que el scraping de datahippo no haya vuelto a encontrar el anuncio aunque sí estuviera online y con fechas disponibles

# Abril 2018 Y fueron revised después de abril 2018.
# Busco los que fueron encontrados por datahippo desde abril 2018 y que aumentaron su númeo de reviews
revised <- last_review[last_review$exists_20180911==1 & last_review$dif_reviews > 0 & 
                      !is.na(last_review$dif_reviews) & last_review$lastreview_date > "2018-03-31" &
                      last_review$revised_date > "2018-03-31",]

table(revised$dif_reviews)
mean(revised$dif_reviews)
median(revised$dif_reviews)


