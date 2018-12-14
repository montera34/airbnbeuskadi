# Este script analiza los reviews de una localización para estudiar estacionalidad y evolución de uso

# Load libraries ---------------
library(gsubfn)
library(tidyverse) 
library(R.utils)

location <- "Donostia"

# Load one single review file ------------
# Load reviews and listings
# Old way to get files
# reviews <- read.delim("data/original/reviews_euskadi.csv",sep = ",")
# reviewsx  <- read.delim("data/original/euskadi/insideairbnb/2018-04-21/visualisations/reviews.csv",sep = ",")
# Allows parsing of files
# reviews <- read_csv("data/original/reviews_euskadi.csv",  col_names = TRUE,
#                      col_types = cols(
#                        # listing_id= col_skip(),
#                        date= col_date(),
#                        comments=col_skip(),
#                        reviewer_id=col_skip(),
#                        # id=col_skip(),
#                        reviewer_name=col_skip(),
#                        .default = col_guess()
#                      )
#                    )
# listings  <- read.delim("data/listings_donostia_simple.csv",sep = ",")
# library(ggmap)
# qmplot(longitude, latitude, data = listings, maptype = "toner-lite", color = I("red"),alpha = I(.2)) + labs(title= "data set 1 anuncios que no están en d")
# qmplot(longitude, latitude, data = filter(listings, neighbourhood=="Donostia-San SebastiÃ¡n"), maptype = "toner-lite", color = I("red"),alpha = I(.2)) + labs(title= "data set 1 anuncios que no están en d")
# qmplot(longitude, latitude, data = filter(listings, !neighbourhood=="Donostia-San SebastiÃ¡n"), maptype = "toner-lite", color = I("red"),alpha = I(.2)) + labs(title= "data set 1 anuncios que no están en d")
# qmplot(longitude, latitude, data = filter(listings, neighbourhood=="Bilbao"), maptype = "toner-lite", color = I("red"),alpha = I(.2)) + labs(title= "data set 1 anuncios que no están en d")
# filter(listings, neighbourhood=="Donostia-San SebastiÃ¡n")

# listings  <- read.delim("data/original/euskadi/insideairbnb/2018-04-21/visualisations/listings.csv",sep = ",")
# listings  <- read.delim("data/listings_bilbao_simple.csv",sep = ",")
# Airbnb listings 2017 merged
# listings <- read.delim("data/output/170400_listings-airbnb-donostia_insideairbnb-datahippo_barrio-umenor.csv",sep = ",")
# listings2018 <- read.delim("data/output/180423_listings-airbnb-donostia_datahippo_barrio-umenor.csv",sep = ",")
# ids <- c(listings$id,listings2018$id)

airdna <- read.delim("data/original/donostia/airdna_n-active-airbnb-listings-donostia-per-month.csv",sep = ",")

# Load multiple reviews files --------
# Merge reviews and remove duplicates

# Loads dates with review data
dates <- c("2018-11-26","2018-10-20","2018-09-27","2018-08-28","2018-07-31","2018-04-21","2017-03-00")
# Las fechas tienen que estar de la más reciente a la más antigua, si no no funcionar el loop siguiente
# dates <- (dates)

reviews <-data.frame(matrix(ncol = 3  ))
names(reviews)  <- c("listing_id","date","scrapdate")

listings <-data.frame(matrix(ncol = 2  ))
names(listings)  <- c("id","neighbourhood")

listings_total <- read.delim("data/original/euskadi/insideairbnb/2018-11-26/visualisations/listings.csv", sep = ",")
reviews_total<- read.delim(gzfile("data/original/euskadi/insideairbnb/2018-11-26/data/reviews.csv.gz"), sep = ",")

# Loop para ir comparando las reviews e insertando las de los listings que no están.
for (i in 1:length(dates)) {
  # name <- as.data.frame(assign(paste("reviews",i,sep = ""),4))
  print("i: ")
  print (i)
  print (dates[i])
  # Create dataframe with reviews of that day
  # assign(paste("reviews",i,sep = ""), read.delim(paste("data/original/airbnb/",dates[i],"/reviews_summary_barcelona_insideairbnb.csv",sep=""),sep = ",")) 
  # Adds rows to the original review file
  reviews_temp <- read.delim(paste("data/original/euskadi/insideairbnb/",dates[i],"/visualisations/reviews.csv",sep=""),sep = ",")
  # Stores date of scraping
  reviews_temp$scrapdate <- dates[i]
  # only stores reviews from listings that were not in database. It asumes that it is not possible to remove reviews
  reviews <- rbind(reviews,
                   reviews_temp[!(reviews_temp$listing_id %in% reviews$listing_id),]
                  )
  # store
  reviews_total_temp <- read.delim(gzfile(paste("data/original/euskadi/insideairbnb/",dates[i],"/data/reviews.csv.gz",sep="")),
      sep = ",")
  reviews_total<- rbind(reviews_total,
                   reviews_total_temp[!(reviews_total_temp$id %in% reviews_total$id),]
  )
  
  listings_temp <- read.delim(paste("data/original/euskadi/insideairbnb/",dates[i],"/visualisations/listings.csv",sep=""),sep = ",")
  # only stores listings that were not in the database
  # listigns with only two variables
  listings <- rbind(listings,
                    select(listings_temp[!(listings_temp$id %in% listings$id),], id,neighbourhood)
                    )
  # listings with all the variables
  listings_total <- rbind(listings_total, listings_temp[!(listings_temp$id %in% listings_total$id),])
  
  print(paste("reviews: ", nrow(reviews), sep=""))
  print(paste("reviews_total_temp: ", nrow(reviews_total_temp), sep=""))
  print(paste("reviews_total: ", nrow(reviews_total), sep=""))
  print(paste("listings: ",nrow(listings), sep=""))
  print(paste("listings_temp: ", nrow(listings_temp), sep=""))
  print(paste("listings_total: ", nrow(listings_total), sep=""))
}
rm(reviews_temp)
rm(listings_temp)
table(reviews$scrapdate)
# save only donostia listings
listings <- filter(listings, neighbourhood=="Donostia-San SebastiÃ¡n")
names(listings) <-c("listing_id","neighbourhood")
# select donostia reviews
reviews <- reviews %>% right_join(filter(listings, neighbourhood=="Donostia-San SebastiÃ¡n"), by="listing_id")

# Inssert room_type in every review
reviews <- reviews %>% left_join(select(listings_total, id, room_type), by= c("listing_id" = "id"))

# Save data in R format -----
# save(reviews ,file="data/output/reviews-donostia-airbnb-insideairbnb-2017.Rda")
# write.csv(reviews, file="../airbnb.madrid.analisis/data/output/reviews-madrid-airbnb-insideairbnb-2010-2018.csv")
save(reviews ,file="data/output/temp/reviews-donostia-airbnb-insideairbnb-2011-2018.Rda")

# ------ Process review to insert year, month, day, hour ------------------
reviews$year <- as.numeric(strapplyc( as.character(reviews$date), "([0-9]*).*", simplify = TRUE))
reviews$month <- as.numeric(strapplyc( as.character(reviews$date), "[0-9]*-([0-9]*)-[0-9]*", simplify = TRUE))
reviews$day <- as.numeric(strapplyc( as.character(reviews$date), ".*[0-9]*-[0-9]*-([0-9]*)", simplify = TRUE))
# Create date field
reviews$date <- as.Date( reviews$date , "%Y-%m-%d")

reviews$fix <- "1"

# test2 <- as.data.frame(unique(reviews[reviews$listing_id %in% listings$id,]$id))

# ------ Load data in case data are already produced------------------
# load("data/output/reviews-donostia-airbnb-insideairbnb-2017.Rda")
# load("data/output/reviews-madrid-airbnb-insideairbnb-2010-2018.Rda")
reviews$fix <- 1

# Change this list of listings to select only a particular kind of listing
# listings <- data.frame(unique(reviews$listing_id))
# colnames(listings) <- "id"

# ------ Analyze reviews----------
# plot(reviews$date,reviews$fix)

# reviews <- reviews[sample(nrow(reviews), 10000), ]

# ------ Single strip: each point is a review. full period analyzed ------
png(filename="images/airbnb/reviews/airbnb-reviews-donostia-2012-2018.png",width = 800,height = 400)
ggplot(reviews, aes(date,fix)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.012) +
  labs(title = paste("Reviews por fecha. ",location,".",sep=""),
       subtitle = "",
       x = "Fecha review",
       y = "",
       caption = "Efecto Airbnb. Data: InsideAirbnb") 
dev.off()

# ------ Single strip: each point is a review. One year ------
png(filename="images/airbnb/reviews/airbnb-reviews-donostia-pormes-2016.png",width = 800,height = 400)
ggplot(reviews[reviews$year==2016,], aes(month,year)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.05) +
  # geom_point(aes(),position = "jitter", alpha=0.2, size=0.01) +
  labs(title = paste("Reviews por mes, 2016. ",location,".",sep=""),
       subtitle = "",
       x = "Mes",
       y = "year",
       caption = "Efecto Airbnb. Data: InsideAirbnb") 
dev.off()

# ------ Single strip each point is a review. One year analyzed ------
# Only display listings included in a predefined listings data.frame (for example, 
#  listings in a location or belonging to a certain list of hosts)
png(filename="images/airbnb/reviews/airbnb-reviews-donostia-pormes-2016-b.png",width = 800,height = 400)
ggplot(reviews[reviews$year==2016 & reviews$listing_id %in% unique(listings$id),], aes(month,year)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.08) +
  labs(title = "Reviews per date. 2016. Donostia",
       subtitle = "",
       x = "Review date",
       y = "2016",
       caption = "Efecto Airbnb. Data: InsideAirbnb") 
dev.off()

png(filename="images/airbnb/reviews/airbnb-reviews-donostia-pormes-puntos-2011-2018.png",width = 800,height = 400)
ggplot(reviews, aes(month,year)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank()
    # axis.text.y=element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.08) +
  labs(title = "Reviews per date. 2011-2018. Donostia",
       subtitle = "",
       x = "Month",
       # y = "2016",
       caption = "Efecto Airbnb. Data: InsideAirbnb") +
  scale_x_continuous(breaks=seq(0,12,1))+
  scale_y_continuous(breaks=seq(2011,2018,1))
dev.off()

# ------ Single strip each point is a review. Full period analyzed ------
# Only display listings included in a predefined listings data.frame
png(filename="images/airbnb/reviews/airbnb-reviews-donostia-2012-2017.png",width = 800,height = 400)
# ggplot(reviews[reviews$listing_id %in% unique(listings$id),], aes(date,fix)) +
ggplot(reviews, aes(date,fix)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.02) +
  labs(title = "Reviews por fecha. Donostia",
       subtitle = "2012-2017",
       x = "Fecha",
       y = "",
       caption = "Efecto Airbnb. Data: InsideAirbnb")  +
  scale_x_date(date_minor_breaks = "1 year", date_labels = "%Y")
dev.off()

# ------ Single strip each point is a review. Full period analyzed, faceted in years ------
# Only display listings included in a predefined listings data.frame
png(filename="images/airbnb/reviews/airbnb-reviews-donostia-2012-201811_g.png",width = 1200,height = 600)
# ggplot(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2011,], aes(month,fix)) +
filter(reviews, year > 2012 ) %>%
ggplot(aes(month,1)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.05) +
  labs(title = "Airbnb reviews por mes y año. Donostia - San Sebastián",
       subtitle = "",
       x = "Review date",
       y = "",
       caption = "Efecto Airbnb. Data: InsideAirbnb")+
  scale_x_continuous(breaks=seq(0,12,1))+
  facet_wrap(~ year)
dev.off()

# ------ reviews by month as points. not very useful------
ggplot(reviews, aes(month,factor(listing_id))) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_point(aes(),position = "jitter",alpha=0.009) +
  labs(title = "Reviews per date. Euskadi",
       # subtitle = "",
       # x = "Review date",
       # y = "year",
       caption = "Efecto Airbnb. Data: InsideAirbnb") 

# ------ ????? ------
# qplot(reviews$month, geom="histogram") 

# Histogram of number reviews per room_type
listings %>% filter(number_of_reviews < 100) %>% 
  ggplot() + geom_freqpoly(mapping = aes(x=number_of_reviews, color = room_type ), binwidth = 1)
listings %>% 
  ggplot() + geom_boxplot(aes(x=reorder(room_type, number_of_reviews, FUN = median),y=number_of_reviews))
listings %>% 
  ggplot() + geom_count(mapping = aes(x = room_type, y = cancellation_policy))
listings %>% 
  ggplot() + geom_point(mapping = aes(x = number_of_reviews, y = minimum_nights),alpha = 0.05)
listings %>% 
  ggplot() + geom_point(mapping = aes(x = number_of_reviews, y = availability_365),alpha = 0.05)

# ------ Reviews per trimester histogram ------
png(filename="images/airbnb/reviews/airbnb-reviews-trimestral-donostia-2012-2016.png",width = 800,height = 400)
# ggplot(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2011 & !reviews$year==2017,],
#        aes(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2011 & !reviews$year==2017,]$month)) + 
ggplot(reviews, aes(reviews$month)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_histogram(breaks=seq(0, 12, by = 3), 
                 # col="red", 
                 # fill="green", 
                 alpha = .9) + 
  scale_x_continuous(breaks=seq(1,12,1))+
  labs(title = "Reviews por trimestre. Donostia - San Sebastián. 2012-2016",
       subtitle = "",
       x = "Mes",
       y = "Número reviews",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")
  # stat_bin(binwidth= 4, geom="text", colour="white", size=3.5, aes(label=..count..) , 
           # position=position_stack(vjust=0.5)) 
dev.off()

# ------ Reviews per month histogram ------
png(filename="images/airbnb/reviews/airbnb-reviews-trimestral-donostia-2012-2016.png",width = 800,height = 600)
ggplot(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2011 & !reviews$year==2017,],
       aes(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2011 & !reviews$year==2017,]$month)) + 
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_histogram(breaks=seq(0, 12, by = 1), 
                 # col="red", 
                 # fill="green", 
                 alpha = .9) + 
  scale_x_continuous(breaks=seq(1,12,1))+
  labs(title = "Reviews por mes. Donostia - San Sebastián. 2012-2016",
       subtitle = "",
       x = "Mes",
       y = "Número reviews",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")
dev.off()

table(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2017 & !reviews$year==2011& !reviews$year==2012,]$month)

# ------ Reviews per month in a year, colored bar by year------
cbPalette <- c("#08306b", "#084594", "#2171b5","#4292c6","#6baed6","#9ecae1", "#c6dbef","#deebf7")
  
png(filename="images/airbnb/reviews/airbnb-reviews-por-mes-stacked-donostia-2011-2018.png",width = 800,height = 600)

reviews %>% 
  group_by(month,year) %>% 
  summarise(count=n()) %>% 
ggplot(aes(x = month, y = count,fill = fct_rev(factor(year)))) + 
  # scale_fill_manual(values = c("#6baed6", "#4292c6", "#2171b5", "#084594")) +
  scale_fill_manual(values = cbPalette) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title=element_blank()
  ) +
  scale_x_continuous(breaks=seq(1,12,1))+
  geom_bar(stat="identity") +
  labs(title = "Reviews por mes en Airbnb en Donostia - San Sebastián. 2011-2018",
       subtitle = "",
       x = "Mes",
       y = "Número de reviews",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")  +
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5),size=3,color="#FFFFFF")
dev.off()

# ------ Acumulation of reviews: date vs listing_id. Full period analyzed ------
#  See gaps in the listings, if listings is not factorized you'll see gaps related to difference in number of listing id
png(filename="images/airbnb/reviews/airbnb-reviews-puntos-donostia-2011-2018-11.png",width = 900,height = 600)
# ggplot(reviews[reviews$listing_id %in% unique(listings$id),], aes(date,factor(listing_id))) +
ggplot(reviews, aes(date,factor(listing_id))) +
  theme_minimal(base_family = "Roboto", base_size = 12) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.07) +
  # labs(title = "Reviews per date and listing. Donostia 2011-2017 (abril)",
  #      subtitle = "Every line is one listing. Look at the line gaps: listings without reviews.",
  #      x = "Review date",
  #      y = "listing id",
  #      caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") 
labs(title = "Reviews por fecha y número de id de alojamiento Airbnb. Donostia 2011-2018 (noviembre)",
     subtitle = "Cada línea es un alojamiento.",
     x = "Fecha de review",
     y = "id de alojamiento",
     caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")  #minor_breaks = "3 month", 
dev.off()

# ------ Acumulation of reviews: date vs listing_id. Smaller darker points. Full period analyzed ------
#  Seen gaps in the listings, if listings is not factorized you'll see gaps related to difference in number of listing id
png(filename="images/airbnb/reviews/airbnb-reviews-puntos-donostia-2011-2017_2.png",width = 900,height = 600)
ggplot() +
  theme_minimal(base_family = "Roboto", base_size = 13) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(date,factor(listing_id)), alpha=1,size=0.0001) +
  labs(title = "Reviews por fecha y número de id de alojamiento Airbnb. Donostia 2011-2017 (abril)",
       subtitle = "Cada línea es un alojamiento.",
       x = "Fecha de review",
       y = "id de alojamiento",
       caption = "Efecto Airbnb. lab.montera34.com Datos: InsideAirbnb") +
  scale_x_date(date_breaks = "1 year", date_labels = "%m.%Y")  #minor_breaks = "3 month", 
dev.off()
# ------ Acumulation of reviews: date vs listing_id. Smaller darker points. Full period analyzed ------
# Factorized listings avoids the gaps due to difference in listings id number
ggplot() +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(date,factor(listing_id)), alpha=1,size=0.0001) +
  labs(title = "Reviews per date and listing. Donostia 2011-2017 (abril)",
       subtitle = "Every line is one listing. Look at the line gaps: listings without reviews.",
       x = "Review date",
       y = "listing id",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# ------ Number of reviews per month. Histogram. Full period --------
# ggplot(reviews[ reviews$year==2015,], aes(date, ..count..)) +
# Donostia
png(filename="images/airbnb/reviews/airbnb-reviews-mes-2011-2018.png",width = 900,height = 600)
ggplot(reviews, aes(date, ..count..)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  geom_histogram(binwidth = 30.41, colour="white") + #  bins = 72
  labs(title = "Reviews de Airbnb por mes en Donostia",
       subtitle = "2011-2017 (abril)",
       x = "Año",
       y = "Número de reviews por mes",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()

# Faceted
png(filename="images/airbnb/reviews/airbnb-reviews-mes-2011-2018_faceted-month.png",width = 900,height = 600)
ggplot(reviews, aes(year)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  geom_bar() + #  bins = 72
  labs(title = "'Reviews' de Airbnb por mes en Donostia",
       subtitle = "2011-2018",
       x = "Año",
       y = "Número de reviews por mes año a año",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")+
  facet_wrap(~ month)
dev.off()

png(filename="images/airbnb/reviews/airbnb-reviews-mes-2011-2018_fecha-scraping_faceted-month.png",width = 900,height = 600)
ggplot(reviews[!is.na(reviews$date),], aes(year,fill=scrapdate)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  geom_bar() + #  bins = 72
  labs(title = "'Reviews' de Airbnb por mes en Donostia",
       subtitle = "2011-2017",
       x = "Año",
       y = "Número de reviews por mes año a año",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")+
  facet_wrap(~ month)
dev.off()

png(filename="images/airbnb/reviews/airbnb-reviews-mes-2011-2017_faceted.png",width = 900,height = 600)
ggplot(reviews[!is.na(reviews$date),], aes(month)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  geom_bar() + #  bins = 72
  labs(title = "'Reviews' de Airbnb por mes en Donostia",
       subtitle = "2011-2017",
       x = "Año",
       y = "Número de reviews por mes",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")+
  facet_wrap(~ year)
dev.off()

png(filename="images/airbnb/reviews/airbnb-reviews-mes-2011-2018_fecha-scraping_faceted.png",width = 900,height = 600)
ggplot(reviews[!is.na(reviews$date),], aes(month, fill=scrapdate)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  geom_bar() + #  bins = 72
  labs(title = "'Reviews' de Airbnb por mes en Donostia",
       subtitle = "2011-2017",
       x = "Año",
       y = "Número de reviews por mes",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")+
  facet_wrap(~ year)
dev.off()

# ------ Number of listings reviewd per month/year. Histogram. Full period --------
# Converts year and month to factors
reviews$year <- as.factor(reviews$year)
reviews$month <- as.factor(reviews$month)

# get only reviews in Donostia
# reviews <- reviews[reviews$listing_id %in% listings$id,]

# get only reviews in Bilbao
# reviews <- reviews[reviews$city == "Bilbao",]
# reviews <- reviews[!is.na(reviews$city),]


# Creates results data frame
results <- data.frame(matrix(ncol = 5,nrow = 96  ))
# names(results)  <- c("date","listings","entire_home","entire_home2","private_room","private_room2","shared_room","shared_room2","not_listed")
names(results)  <- c("date","listings","entire_home","private_room","shared_room")

k <- 1
# for ( i in as.list(levels(reviews$year))) {
for ( i in 2011:2018 ) {
  for ( j in 1:12) {
    paste(print(i),print(j),sep = "-")
    print(length(unique(reviews[reviews$year==i & reviews$month==j,]$listing_id)))
    print( as.Date( paste(i,j,1,sep = "-") ) )
    results$date[k] <- as.Date(paste(i,j,1,sep = "-"),origin="1970-01-01")
    results$listings[k] <- length(unique(reviews[reviews$year==i & reviews$month==j & !is.na(reviews$date),]$listing_id))
    results$entire_home[k] <- length(unique(reviews[reviews$year==i & reviews$month==j & reviews$room_type == "Entire home/apt" & !is.na(reviews$date),]$listing_id))
    results$private_room[k] <- length(unique(reviews[reviews$year==i & reviews$month==j & reviews$room_type == "Private room" & !is.na(reviews$date),]$listing_id))
    results$shared_room[k] <- length(unique(reviews[reviews$year==i & reviews$month==j & reviews$room_type == "Shared room" & !is.na(reviews$date),]$listing_id))
    k <- k+1
  }
}

results$date <- as.Date(as.numeric(results$date),origin="1970-01-01")

png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-donostia-with-review-mes-2011-2018.png",width = 900,height = 600)
ggplot(results, aes(date, listings)) +
  geom_col() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  labs(title = "Listings con reviews de Airbnb por mes en Donostia",
       subtitle = "2011-2018 (november)",
       x = "Fecha",
       y = "Número de listing con review por mes",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2017 y 2018") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()


library(lubridate) #to extract month in following plot
png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-donostia-with-review-mes-2011-2018_marzo-marcado.png",width = 900,height = 600)
ggplot(results, aes(date, listings, fill = month(date) == 3 )) +
  geom_col() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  labs(title = "Listings con reviews de Airbnb por mes en Donostia",
       subtitle = "2011-2018 (noviembre)",
       x = "Fecha",                 
       y = "Número de listing con review por mes",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2017-2018") +
  guides(fill=guide_legend(title="Marzo")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()

# -------------- Room type classification
library(reshape2)
# Convert data to long-form with 'melt' from the reshape2 package.
results_melt <- melt(results, id.vars=c("date"),
             measure.vars=c("entire_home", "private_room","shared_room"))

# Remove dates without data
# results_melt <- results_melt[results_melt$date < "2017-04-01" , ]

png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-donostia-with-review-mes-2011-2018_rooom-type_bar.png",width = 900,height = 600)
ggplot(results_melt, aes(x=date, y=value, fill=variable)) +
  geom_bar(position="stack", stat="identity")  +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  labs(title = "Listings con reviews de Airbnb por mes en Donostia",
       subtitle = "2011-2018 (noviembre)",
       x = "Fecha",
       y = "Número de listing con review por mes por tipo de alojamiento",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2017-2018") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()

png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-donostia-with-review-mes-2011-2018_rooom-type_line.png",width = 900,height = 600)
ggplot(filter(results_melt,date < "2018-12-01"), aes(x=date, y=value, color=variable)) +
  geom_line()  +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  labs(title = "Listings con reviews de Airbnb por mes en Donostia",
       subtitle = "2011-2018 (noviembre)",
       x = "Fecha",
       y = "Número de listing con review por mes por tipo de alojamiento",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2017-2018") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()

# Active listings. Calculate active listings if 30% of users leave review ----------

results_melt$value_calculated_max <- round(results_melt$value / 0.3, 0)
results_melt$value_calculated_min <- round(results_melt$value / 0.7, 0)

png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-donostia-with-review-mes-2011-2018_rooom-type_line_calculated.png",width = 900,height = 600)
ggplot(filter(results_melt,date < "2018-12-01"),aes(x=date, color=variable)) +
  geom_line(aes(y=value))  +
  geom_line(aes(y=value_calculated_max),linetype = 2  )  +
  
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  labs(title = "Listings con reviews de Airbnb por mes en Donostia",
       subtitle = "2011-2018 (noviembre). Línea de puntos: cálculo de listings si 30% dejan reviews.",
       x = "Fecha",
       y = "Número de listing con review por mes por tipo de alojamiento",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2017-2018") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()

# Compare with AIRDNA active listings data for Donostia-----
airdna$date <- as.Date(airdna$date)
names(airdna) <- c("date","entire_home_airdna","private_room_airdna","shared_room_airdna","total_airdna")
# results_airdna <- merge(results,airdna, by.x="date",by.y = "date",all = TRUE)

# Convert data to long-form with 'melt' from the reshape2 package.
# results_airdna_melt <- melt(results_airdna, id.vars=c("date"),
#                      measure.vars=c("entire_home", "private_room","shared_room",
#                                     "entire_home_airdna", "private_room_airdna","shared_room_airdna"))

airdna_melt <- melt(airdna, id.vars=c("date"),
                     measure.vars=c("entire_home_airdna", "private_room_airdna","shared_room_airdna"))


# names(airdna_melt) <- c("date","variable","value_airdna")
# resuls_melt <- merge(results_melt,airdna_melt, by.x="date",by.y = "date",all = TRUE)
# reviews <- merge(reviews,listings[,c("id","room_type","city")], by.x = "listing_id", by.y = "id", all.x=TRUE)

# results_airdna_melt$value_calculated <- round(results_airdna_melt$value / 0.3, 0)

# Remove NA
results_melt <- results_melt[complete.cases(results_melt),]

library(RColorBrewer)

png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-donostia-with-review-mes-2011-2018_rooom-type_line_calculated_vs_airdna.png",width = 900,height = 600)
ggplot(NULL,aes(x=date, color=variable)) +
  # geom_line(data = results_melt,aes(y=value))  +
  geom_ribbon(data = filter(results_melt,date < "2018-12-01",variable == "entire_home"),
              aes(ymin=value_calculated_min,ymax=value_calculated_max), fill="pink",color="white",alpha=0.5)  +
  geom_ribbon(data = filter(results_melt,date < "2018-12-01",variable == "private_room"),
              aes(ymin=value_calculated_min,ymax=value_calculated_max), fill="green",color="white",alpha=0.3)  +
  # geom_line(data = filter(results_melt,date < "2018-12-01"),aes(y=value_calculated_min),linetype=2)  +
  geom_line(data = airdna_melt,aes(y=value,fill=variable), size=1.5  )  +
  # scale_linetype_manual("",values=c(1, 2, 3,2, 2, 2),guide=FALSE)+
  scale_color_manual("",values = c("#F8766D","#44BB55","#AAAADD","#AAAA55")) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  labs(title = "Anuncios con reviews de Airbnb por mes vs Anuncios activos. Donostia",
       subtitle = "2011-2018 (noviembre). Área: cálculo entre 30%-70% evaluaciones basado en reviews (fuente InsideAirbnb).",
       x = "Fecha",
       y = "Número de listing con review por mes por tipo de alojamiento",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2017-2018 y Airdna.") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()

# ------ Acumulation of reviews: date vs listing_id. Full period analyzed ------
ggplot() +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(date,factor(listing_id)), alpha=1,size=0.0001) +
  labs(title = "Reviews per date and listing. Donostia 2011-2017 (abril)",
       subtitle = "Every line is one listing. Look at the line gaps: listings without reviews.",
       x = "Review date",
       y = "listing id",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") +
  scale_x_date(date_minor_breaks = "1 month")

# ------ Acumulation of reviews: date vs listing_id. Plot points by host id selected in other color. Full period analyzed ------
# Which listings are owned by hosts that have multiple listings
# group hosts id and order them by number of listings
list2 <- listings %>% 
  group_by(host_id) %>% 
  summarise(count=n()) %>% arrange(-count)

# Select the top 30 host id
listings$host_id %in% list2$host_id[1:30]

listings_top <- listings[listings$host_id==15707672 |listings$host_id==98419892 |listings$host_id== 3952766 |listings$host_id== 5254782,]$id

# Use this if do not know which listings to select
# listings_top <- listings[1:100,]
png(filename="images/airbnb/reviews/airbnb-reviews-mes-top4-marcado-2011-2017.png",width = 900,height = 600)
ggplot() +
  theme_minimal(base_family = "Roboto", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(date,factor(listing_id)), 
             alpha=1,size=0.0001) +
  geom_point(data=reviews[reviews$listing_id %in% listings_top,],aes(date,factor(listing_id)),
             alpha=1,size=0.4,color="#FF0000") +
  labs(title = "Reviews de Airbnb por mes en Donostia 2011-2017 (abril)",
     subtitle = "Cada línea es un listing. En rojo los top 4 anfitriones que gestionan 164 alojamientos.",
     x = "Año",
     y = "id de alojamiento",
     caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_hline(yintercept = seq(0,1300,by=100),colour = "#999999",size=0.5,alpha=0.1) +
  geom_text(aes(x = as.Date("2012-01-01"), y = 510, label = "500"), color = "#999999", size=3) +
  geom_text(aes(x = as.Date("2012-01-01"), y = 1010, label = "1.000"), color = "#999999", size=3)
  # geom_text(aes(x = as.Date("2012-01-01"), y = 16000, label = "15.000"), color = "#999999", size=3) +
  # geom_text(aes(x = as.Date("2012-01-01"), y = 21000, label = "20.000"), color = "#999999", size=3) +
  # geom_text(aes(x = as.Date("2012-01-01"), y = 26000, label = "25.000"), color = "#999999", size=3) 
dev.off()

# ------ Acumulation of reviews: date vs listing_id.Select per date -------------
ggplot() +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id) & reviews$date > "2016-04-01",],aes(date,factor(listing_id)), alpha=0.3,size=0.0001) +
  # geom_point(data=reviews[reviews$listing_id %in% listings_top,],aes(date,factor(listing_id)), alpha=1,size=0.005,color="#FF0000") +
  labs(title = "Reviews per date and listing. Donostia 2011-2017 (abril)",
       subtitle = "Every line is one listing. In red: the top 4 hosts (users with more listings) manage 155 ads.",
       x = "Review date",
       y = "listing id",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") 

# ------ Acumulation of reviews: date vs listing_id.Select per date -------------
ggplot(data = reviews, aes(x=date, y=listing_id, fill=month)) + 
  geom_tile()

# ------ Others. TODO -------------
ggplot(data = reviews, aes(x=date, y=factor(listing_id))) + 
  stat_density(aes(fill = ..density..), geom = "raster", position = "identity")
