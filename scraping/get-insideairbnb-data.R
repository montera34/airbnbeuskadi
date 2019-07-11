dates <- c("2019-06-30","2019-05-29","2019-04-29", "2019-03-30","2019-02-19","2019-01-30",
           "2018-12-22","2018-11-26", "2018-10-20","2018-09-27","2018-08-28","2018-07-31","2018-04-21","2017-03-00")
dates <- c("2019-06-30","2019-05-29","2019-04-29", "2019-03-30","2019-02-19","2019-01-30",
           "2018-12-22")
# the data at "2017-03-00" are not in the website
files_data <- c("listings.csv.gz", "calendar.csv.gz", "reviews.csv.gz")
files_vis <- c("listings.csv","reviews.csv")


for (i in 1:length(dates)) {
# for (i in 2:3) {
  dir.create(paste("data/original/euskadi/insideairbnb/",dates[i],sep=""))
  dir.create(paste("data/original/euskadi/insideairbnb/",dates[i],"/data",sep=""))
  dir.create(paste("data/original/euskadi/insideairbnb/",dates[i],"/visualizations/",sep=""))
  for (j in 1:length(files_vis)) {
  download.file(paste("http://data.insideairbnb.com/spain/pv/euskadi/",dates[i],"/visualisations/",files_vis[j],sep=""),
              destfile=paste("data/original/euskadi/insideairbnb/",dates[i],"/visualizations/",files_vis[j],sep=""),
                             method = "wget")
  }
  for (k in 1:length(files_data)) {
  download.file(paste("http://data.insideairbnb.com/spain/pv/euskadi/",dates[i],"/data/",files_data[k],sep=""),
                destfile=paste("data/original/euskadi/insideairbnb/",dates[i],"/data/",files_data[k],sep=""),
                method = "wget")
  }
}


