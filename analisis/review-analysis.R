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

plot(reviews$datex)

ggplot(reviews, aes(datex,fix)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
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
  geom_histogram(breaks=seq(0, 12, by = 1), 
                 # col="red", 
                 # fill="green", 
                 alpha = .9) + 
  scale_x_continuous(breaks=seq(1,12,1))+
  labs(title = "Reviews per month. Donostia - San Sebastián. 2011-2016",
       subtitle = "",
       x = "Mes",
       y = "Number of reviews",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb") 


table(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2017,]$month)


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
  geom_bar(stat="identity") +
  labs(title = "Reviews per month in Airbnb in Donostia - San Sebastián. 2012-2016",
       subtitle = "",
       x = "Mes",
       y = "Number of reviews",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")  +
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5),size=3,color="#FFFFFF")

