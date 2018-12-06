# Este script analiza los reviews de una localización para estudiar estacionalidad y evolución de uso

# Load libraries
library(gsubfn)
library(tidyverse) 

location <- "Donostia"

# Load reviews and listings
reviews <- read.delim("data/original/reviews_euskadi.csv",sep = ",")
listings  <- read.delim("data/listings_donostia_simple.csv",sep = ",")
# listings  <- read.delim("data/listings_bilbao_simple.csv",sep = ",")
# Airbnb listings 2017 merged
# listings <- read.delim("data/output/170400_listings-airbnb-donostia_insideairbnb-datahippo_barrio-umenor.csv",sep = ",")
# listings2018 <- read.delim("data/output/180423_listings-airbnb-donostia_datahippo_barrio-umenor.csv",sep = ",")
# ids <- c(listings$id,listings2018$id)

airdna <- read.delim("data/original/donostia/airdna_n-active-airbnb-listings-donostia-per-month.csv",sep = ",")

# ------ Merge reviews and remove duplicates --------
# reviews1 <- read.delim("../airbnb.madrid.analisis/data/original/insideairbnb_reviews_madrid_150717.csv",sep = ",")
# reviews2 <- read.delim("../airbnb.madrid.analisis/data/original/insideairbnb_reviews_madrid_150904.csv",sep = ",")
# reviews3 <- read.delim("../airbnb.madrid.analisis/data/original/insideairbnb_reviews_madrid_151002.csv",sep = ",")
# reviews4 <- read.delim("../airbnb.madrid.analisis/data/original/insideairbnb_reviews_madrid_170306.csv",sep = ",")
# reviews5 <- read.delim("../airbnb.madrid.analisis/data/original/insideairbnb_reviews_madrid_170408.csv",sep = ",")
# reviews6 <- read.delim("../airbnb.madrid.analisis/data/original/insideairbnb_reviews_madrid_180117.csv",sep = ",")

reviews <- reviews1
# de 1 que no están en 2
# db1_not_in_2 <- reviews[!(reviews$id %in% reviews2$id),]
# db1_in_2 <- reviews[reviews$id %in% reviews2$id,]
# de 2 que no están en 1
db2_not_in_1 <- reviews2[!(reviews2$id %in% reviews$id),]
# db2_in_1 <- reviews2[reviews2$id %in% reviews$id,]
reviews <- rbind(reviews1,db2_not_in_1)

# from 3 not in merged
db3_not_in_1 <- reviews3[!(reviews3$id %in% reviews$id),]
reviews <- rbind(reviews,db3_not_in_1)
# from 4 not in merged
db4_not_in_1 <- reviews4[!(reviews4$id %in% reviews$id),]
reviews <- rbind(reviews,db4_not_in_1)
# from 5 not in merged
db5_not_in_1 <- reviews5[!(reviews5$id %in% reviews$id),]
reviews <- rbind(reviews,db5_not_in_1)
# from 6 not in merged
db6_not_in_1 <- reviews6[!(reviews6$id %in% reviews$id),]
reviews <- rbind(reviews,db6_not_in_1)

save(reviews ,file="data/output/reviews-donostia-airbnb-insideairbnb-2017.Rda")
write.csv(reviews, file="../airbnb.madrid.analisis/data/output/reviews-madrid-airbnb-insideairbnb-2010-2018.csv")

# ------ Process review to insert year, month, day, hour ------------------
reviews$year <- as.numeric(strapplyc( as.character(reviews$date), "([0-9]*).*", simplify = TRUE))
reviews$month <- as.numeric(strapplyc( as.character(reviews$date), "[0-9]*-([0-9]*)-[0-9]*", simplify = TRUE))
reviews$day <- as.numeric(strapplyc( as.character(reviews$date), ".*[0-9]*-[0-9]*-([0-9]*)", simplify = TRUE))
# Create date field
reviews$datex <- as.Date( paste(reviews$day,"/",reviews$month,"/",reviews$year,sep = "" ), "%d/%m/%Y")

reviews$fix <- "fix"

# test2 <- as.data.frame(unique(reviews[reviews$listing_id %in% listings$id,]$id))

# ------ Load data in case data are already produced------------------
load("data/output/reviews-donostia-airbnb-insideairbnb-2017.Rda")
# load("data/output/reviews-madrid-airbnb-insideairbnb-2010-2018.Rda")
reviews$fix <- 1

# Change this list of listings to select only a particular kind of listing
# listings <- data.frame(unique(reviews$listing_id))
# colnames(listings) <- "id"

# ------ Analyze reviews----------
plot(reviews$datex,reviews$fix)

reviews <- reviews[sample(nrow(reviews), 10000), ]

# ------ Single strip: each point is a review. full period analyzed ------
png(filename="images/airbnb/reviews/airbnb-reviews-donostia-2012-2018.png",width = 800,height = 400)
ggplot(reviews, aes(datex,fix)) +
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

# ------ Single strip each point is a review. Full period analyzed ------
# Only display listings included in a predefined listings data.frame
png(filename="images/airbnb/reviews/airbnb-reviews-donostia-2012-2017.png",width = 800,height = 400)
ggplot(reviews[reviews$listing_id %in% unique(listings$id),], aes(datex,fix)) +
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
png(filename="images/airbnb/reviews/airbnb-reviews-donostia-2012-2017-b.png",width = 800,height = 400)
ggplot(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2011,], aes(month,fix)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.1) +
  labs(title = "Airbnb reviews por mes y año. 2016. Donostia - San Sebastián",
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

# ------ Reviews per trimester histogram ------
png(filename="images/airbnb/reviews/airbnb-reviews-trimestral-donostia-2012-2016.png",width = 800,height = 400)
ggplot(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2011 & !reviews$year==2017,],
       aes(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2011 & !reviews$year==2017,]$month)) + 
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
# Donostia
res2 <- reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2017 & !reviews$year==2011& !reviews$year==2012,] %>% 
  group_by(month,year) %>% 
  summarise(count=n())

cbPalette <- c("#bbbbbb","#448fd6","#bbbbbb","#a044d6","#598413","#f45042","#598413")

png(filename="images/airbnb/reviews/airbnb-reviews-por-mes-stacked-donostia-2013-2016.png",width = 800,height = 600)
ggplot(res2,aes(x = month, y = count,fill = fct_rev(factor(year)))) + 
  scale_fill_manual(values = c("#6baed6", "#4292c6", "#2171b5", "#084594")) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title=element_blank()
  ) +
  scale_x_continuous(breaks=seq(1,12,1))+
  geom_bar(stat="identity") +
  labs(title = "Reviews por mes en Airbnb en Donostia - San Sebastián. 2013-2016",
       subtitle = "",
       x = "Mes",
       y = "Número de reviews",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")  +
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5),size=3,color="#FFFFFF")
dev.off()

# Madrid
ggplot(res2,aes(x = month, y = count,fill = year)) + 
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_colour_brewer(palette = "Blues") +
  scale_x_continuous(breaks=seq(1,12,1))+
  geom_bar(stat="identity") +
  labs(title = "Reviews per month in Airbnb in Madrid. 2011-2017",
       subtitle = "",
       x = "Month",
       y = "Number of reviews",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")  +
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5),size=3,color="#FFFFFF")


# ------ Acumulation of reviews: date vs listing_id. Full period analyzed ------
#  See gaps in the listings, if listings is not factorized you'll see gaps related to difference in number of listing id
png(filename="images/airbnb/reviews/airbnb-reviews-puntos-donostia-2011-2017_1.png",width = 900,height = 600)
ggplot(reviews[reviews$listing_id %in% unique(listings$id),], aes(datex,factor(listing_id))) +
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
labs(title = "Reviews por fecha y número de id de alojamiento Airbnb. Donostia 2011-2017 (abril)",
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
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(datex,factor(listing_id)), alpha=1,size=0.0001) +
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
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(datex,factor(listing_id)), alpha=1,size=0.0001) +
  labs(title = "Reviews per date and listing. Donostia 2011-2017 (abril)",
       subtitle = "Every line is one listing. Look at the line gaps: listings without reviews.",
       x = "Review date",
       y = "listing id",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# ------ Number of reviews per month. Histogram. Full period --------
# ggplot(reviews[ reviews$year==2015,], aes(datex, ..count..)) +
# Donostia
png(filename="images/airbnb/reviews/airbnb-reviews-mes-2011-2017.png",width = 900,height = 600)
ggplot(reviews, aes(datex, ..count..)) +
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
png(filename="images/airbnb/reviews/airbnb-reviews-mes-2011-2017_faceted-month.png",width = 900,height = 600)
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
       subtitle = "2011-2017",
       x = "Año",
       y = "Número de reviews por mes año a año",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")+
  facet_wrap(~ month)
dev.off()

png(filename="images/airbnb/reviews/airbnb-reviews-mes-2011-2017_faceted.png",width = 900,height = 600)
ggplot(reviews, aes(month)) +
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


# Madrid
ggplot(reviews[!reviews$year==2010 & !reviews$year==2011 & !reviews$year==2018,], aes(datex, ..count..)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_histogram(binwidth = 30.41, colour="white") + #  bins = 72
  # labs(title = "Reviews per month. Madrid 2012-2017",
  #      subtitle = "Using 6 different review files from InsideAirbnb (years 2015, 2017 and 2018)",
  #      x = "Date",
  #      y = "Number of reviews by month",
  #      caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") +
  labs(title = "'Reviews' de Airbnb por mes en Madrid 2012-2017",
       subtitle = "Usando 6 diferentes archivos de reviews InsideAirbnb (3 de 2015, 2 de 2017 y 1 de 2018)",
       x = "Año",
       y = "Número de reviews por mes",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") +
  geom_hline(yintercept = seq(0,30000,by=5000),colour = "#999999",size=0.5,alpha=0.1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_text(aes(x = as.Date("2012-01-01"), y = 6000, label = "5.000"), color = "#999999", size=3) +
  geom_text(aes(x = as.Date("2012-01-01"), y = 11000, label = "10.000"), color = "#999999", size=3) +
  geom_text(aes(x = as.Date("2012-01-01"), y = 16000, label = "15.000"), color = "#999999", size=3) +
  geom_text(aes(x = as.Date("2012-01-01"), y = 21000, label = "20.000"), color = "#999999", size=3) +
  geom_text(aes(x = as.Date("2012-01-01"), y = 26000, label = "25.000"), color = "#999999", size=3) 



# ------ Number of listings reviewd per month/year. Histogram. Full period --------
reviews$year <- as.factor(reviews$year)
reviews$month <- as.factor(reviews$month)

reviews <- merge(reviews,listings[,c("id","room_type","city")], by.x = "listing_id", by.y = "id", all.x=TRUE)


# get only reviews in Donostia
reviews <- reviews[reviews$listing_id %in% listings$id,]

# get only reviews in Bilbao
# reviews <- reviews[reviews$city == "Bilbao",]
# reviews <- reviews[!is.na(reviews$city),]


# Creates results data frame
results <- data.frame(matrix(ncol = 5,nrow = 84  ))
# names(results)  <- c("date","listings","entire_home","entire_home2","private_room","private_room2","shared_room","shared_room2","not_listed")
names(results)  <- c("date","listings","entire_home","private_room","shared_room")

k <- 0
# for ( i in as.list(levels(reviews$year))) {
for ( i in 2011:2017 ) {
  for ( j in 1:12) {
    paste(print(i),print(j),sep = "-")
    print(length(unique(reviews[reviews$year==i & reviews$month==j,]$listing_id)))
    print( as.Date( paste(i,j,1,sep = "-") ) )
    results$date[k] <- as.Date(paste(i,j,1,sep = "-"),origin="1970-01-01")
    results$listings[k] <- length(unique(reviews[reviews$year==i & reviews$month==j,]$listing_id))
    results$entire_home[k] <- length(unique(reviews[reviews$year==i & reviews$month==j & reviews$room_type == "Entire home/apt",]$listing_id))
    # results$entire_home2[k] <- length(unique(reviews[reviews$year==i & reviews$month==j &  (reviews$listing_id %in% listings[listings$room_type == "Entire home/apt",]$id),]$listing_id))
    results$private_room[k] <- length(unique(reviews[reviews$year==i & reviews$month==j & reviews$room_type == "Private room",]$listing_id))
    # results$private_room2[k] <- length(unique(reviews[reviews$year==i & reviews$month==j & (reviews$listing_id %in% listings[listings$room_type == "Private room",]$id),]$listing_id))
    results$shared_room[k] <- length(unique(reviews[reviews$year==i & reviews$month==j & reviews$room_type == "Shared room",]$listing_id))
    # results$shared_room2[k] <- length(unique(reviews[reviews$year==i & reviews$month==j &  (reviews$listing_id %in% listings[listings$room_type == "Shared room",]$id),]$listing_id))
    # results$not_listed[k] <- length(unique(reviews[reviews$year==i & reviews$month==j &  !(reviews$listing_id %in% listings$id),]$listing_id))
    k <- k+1
  }
}

results$date <- as.Date(as.numeric(results$date),origin="1970-01-01")

png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-donostia-with-review-mes-2011-2017.png",width = 900,height = 600)
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
       subtitle = "2011-2017 (abril)",
       x = "Fecha",
       y = "Número de listing con review por mes",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2017") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()


library(lubridate) #to extract month in following plot
png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-donostia-with-review-mes-2011-2017_marzo-marcado.png",width = 900,height = 600)
ggplot(results[results$date < "2017-04-01" , ], aes(date, listings,fill = month(date) == 3 )) +
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
       subtitle = "2011-2017 (abril)",
       x = "Fecha",                 
       y = "Número de listing con review por mes",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2017") +
  guides(fill=guide_legend(title="Marzo")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()

# -------------- Room type classification
library(reshape2)
# Convert data to long-form with 'melt' from the reshape2 package.
results_melt <- melt(results, id.vars=c("date"),
             measure.vars=c("entire_home", "private_room","shared_room"))

# Remove dates without data
results_melt <- results_melt[results_melt$date < "2017-04-01" , ]

png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-donostia-with-review-mes-2011-2017_rooom-type_bar.png",width = 900,height = 600)
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
       subtitle = "2011-2017 (abril)",
       x = "Fecha",
       y = "Número de listing con review por mes por tipo de alojamiento",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2017") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()

png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-donostia-with-review-mes-2011-2017_rooom-type_line.png",width = 900,height = 600)
ggplot(results_melt, aes(x=date, y=value, color=variable)) +
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
       subtitle = "2011-2017 (abril)",
       x = "Fecha",
       y = "Número de listing con review por mes por tipo de alojamiento",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2017") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()

# Active listings. Calculate active listings if 30% of users leave review ----------

results_melt$value_calculated <- round(results_melt$value / 0.3, 0)

png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-donostia-with-review-mes-2011-2017_rooom-type_line_calculated.png",width = 900,height = 600)
ggplot(results_melt,aes(x=date, color=variable)) +
  geom_line(aes(y=value))  +
  geom_line(aes(y=value_calculated),linetype = 2  )  +
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
       subtitle = "2011-2017 (abril). Línea de puntos: cálculo de listings si 30% dejan reviews.",
       x = "Fecha",
       y = "Número de listing con review por mes por tipo de alojamiento",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2017") +
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
ggplot(NULL,aes(x=date, color=variable)) +
  # geom_line(data = results_melt,aes(y=value))  +
  geom_line(data = results_melt,aes(y=value_calculated,linetype=variable))  +
  geom_line(data = airdna_melt,aes(y=value)  )  +
  scale_linetype_manual(values=c(2, 2, 2),guide=FALSE)+
  scale_color_manual(values = c("#FFAA55","#FFAA55","#44FF55","#44FF55","#AAAADD","#AAAA55")) +
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
       subtitle = "2011-2017 (abril). Línea de puntos: cálculo de listings si 30% dejan reviews.",
       x = "Fecha",
       y = "Número de listing con review por mes por tipo de alojamiento",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2017 y Airdna.") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# ------ Acumulation of reviews: date vs listing_id. Full period analyzed ------
ggplot() +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(datex,factor(listing_id)), alpha=1,size=0.0001) +
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
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(datex,factor(listing_id)), 
             alpha=1,size=0.0001) +
  geom_point(data=reviews[reviews$listing_id %in% listings_top,],aes(datex,factor(listing_id)),
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
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id) & reviews$datex > "2016-04-01",],aes(datex,factor(listing_id)), alpha=0.3,size=0.0001) +
  # geom_point(data=reviews[reviews$listing_id %in% listings_top,],aes(datex,factor(listing_id)), alpha=1,size=0.005,color="#FF0000") +
  labs(title = "Reviews per date and listing. Donostia 2011-2017 (abril)",
       subtitle = "Every line is one listing. In red: the top 4 hosts (users with more listings) manage 155 ads.",
       x = "Review date",
       y = "listing id",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") 

# ------ Acumulation of reviews: date vs listing_id.Select per date -------------
ggplot(data = reviews, aes(x=datex, y=listing_id, fill=month)) + 
  geom_tile()

# ------ Others. TODO -------------
ggplot(data = reviews, aes(x=datex, y=factor(listing_id))) + 
  stat_density(aes(fill = ..density..), geom = "raster", position = "identity")
