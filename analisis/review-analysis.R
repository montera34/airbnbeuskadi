library(gsubfn)
library(tidyverse) 

reviews <- read.delim("data/original/reviews_euskadi.csv",sep = ",")
listings  <- read.delim("data/listings_donostia_simple.csv",sep = ",")

# extract year,month, day, hour
reviews$year <- as.numeric(strapplyc( as.character(reviews$date), "([0-9]*).*", simplify = TRUE))
reviews$month <- as.numeric(strapplyc( as.character(reviews$date), "[0-9]*-([0-9]*)-[0-9]*", simplify = TRUE))
reviews$day <- as.numeric(strapplyc( as.character(reviews$date), ".*[0-9]*-[0-9]*-([0-9]*)", simplify = TRUE))
# Create date field
reviews$datex <- as.Date( paste(reviews$day,"/",reviews$month,"/",reviews$year,sep = "" ), "%d/%m/%Y")

reviews$fix <- "fix"
row.names(reviews)

plot(reviews$datex)

ggplot(reviews, aes(datex,fix)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.012) +
  labs(title = "Reviews per date. Euskadi",
       subtitle = "",
       x = "Review date",
       y = "",
       caption = "Efecto Airbnb. Data: InsideAirbnb") 

ggplot(reviews[reviews$year==2016,], aes(month,year)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.05) +
  labs(title = "Reviews per date. 2016. Euskadi",
       subtitle = "",
       x = "Review date",
       y = "year",
       caption = "Efecto Airbnb. Data: InsideAirbnb") 

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

ggplot(reviews[reviews$listing_id %in% unique(listings$id),], aes(datex,fix)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.02) +
  labs(title = "Reviews per date. Donostia",
       subtitle = "",
       x = "Review date",
       y = "",
       caption = "Efecto Airbnb. Data: InsideAirbnb")  +
  scale_x_date(date_minor_breaks = "1 year", date_labels = "%Y")

ggplot(reviews[reviews$listing_id %in% unique(listings$id),], aes(month,fix)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.1) +
  labs(title = "Airbnb reviews per date. 2016. Donostia - San Sebastián",
       subtitle = "",
       x = "Review date",
       y = "",
       caption = "Efecto Airbnb. Data: InsideAirbnb")+
  scale_x_continuous(breaks=seq(0,12,1))+
  facet_wrap(~ year)

ggplot(reviews, aes(month,listing_id)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_point(aes(),position = "jitter",alpha=0.009) +
  labs(title = "Reviews per date. Euskadi",
       # subtitle = "",
       # x = "Review date",
       # y = "year",
       caption = "Efecto Airbnb. Data: InsideAirbnb") 


qplot(reviews$month, geom="histogram") 


ggplot(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2017,],
       aes(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2017,]$month)) + 
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
  labs(title = "Reviews per month. Donostia - San Sebastián. 2011-2016",
       subtitle = "",
       x = "Mes",
       y = "Number of reviews",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb") 


  table(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2017 & !reviews$year==2011& !reviews$year==2012,]$month)


res2 <- reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2017 & !reviews$year==2011& !reviews$year==2012,] %>% 
  group_by(month,year) %>% 
  summarise(count=n())

ggplot(res2,aes(x = month, y = count,fill = year)) + 
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_x_continuous(breaks=seq(1,12,1))+
  geom_bar(stat="identity") +
  labs(title = "Reviews per month in Airbnb in Donostia - San Sebastián. 2013-2016",
       subtitle = "",
       x = "Month",
       y = "Number of reviews",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")  +
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5),size=3,color="#FFFFFF")

# ------- Seen gaps in the listings---------------------
ggplot(reviews[reviews$listing_id %in% unique(listings$id),], aes(datex,listing_id)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.03) +
  labs(title = "Reviews per date and listing. Donostia 2011-2017 (abril)",
       subtitle = "Every line is one listing. Look at the line gaps: listings without reviews.",
       x = "Review date",
       y = "listing id",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") 



ggplot() +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(datex,listing_id), alpha=1,size=0.0001) +
  labs(title = "Reviews per date and listing. Donostia 2011-2017 (abril)",
       subtitle = "Every line is one listing. Look at the line gaps: listings without reviews.",
       x = "Review date",
       y = "listing id",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") 


ggplot() +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(datex,factor(listing_id)), alpha=1,size=0.0001) +
  labs(title = "Reviews per date and listing. Donostia 2011-2017 (abril)",
       subtitle = "Every line is one listing. Look at the line gaps: listings without reviews.",
       x = "Review date",
       y = "listing id",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") 

# Which listings are owned by superhosts:
list2 <- listings %>% 
  group_by(host_id) %>% 
  summarise(count=n()) %>% arrange(-count)

listings$host_id %in% list2$host_id[1:30]

listings_top <- listings[listings$host_id==15707672 |listings$host_id==98419892 |listings$host_id== 3952766 |listings$host_id== 5254782,]$id



ggplot() +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(datex,factor(listing_id)), alpha=0.3,size=0.0001) +
  geom_point(data=reviews[reviews$listing_id %in% listings_top,],aes(datex,factor(listing_id)), alpha=1,size=0.005,color="#FF0000") +
  labs(title = "Reviews per date and listing. Donostia 2011-2017 (abril)",
       subtitle = "Every line is one listing. In red: the top 4 hosts (users with more listings) manage 155 ads.",
       x = "Review date",
       y = "listing id",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") 




ggplot(reviews[reviews$listing_id %in% unique(list2$id),], aes(datex,listing_id)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.03) +
  labs(title = "Reviews per date and listing. Donostia 2011-2017 (abril)",
       subtitle = "Every line is one listing. Fijaros en las filas vacías: anuncios sin reviews.",
       x = "Review date",
       y = "listing id",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") 




ggplot(data = reviews, aes(x=datex, y=listing_id, fill=month)) + 
  geom_tile()

ggplot(mtcars, aes(mpg, factor(cyl)))

ggplot(data = reviews, aes(x=datex, y=factor(listing_id))) + 
  stat_density(aes(fill = ..density..), geom = "raster", position = "identity")
