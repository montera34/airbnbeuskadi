import csv
import urllib2
from bs4 import BeautifulSoup
import time
from datetime import datetime
import re
 
infile = "../data/original/180604_listings-airbnb-donostia_datahippo.csv" # nombre del archivo con los ids de los listings, uno por linea
today = datetime.now().date().strftime("%Y%m%d")
outputpath = "../data/output/180604_listings-airbnb-donostia_datahippo_with-last-review-180911b.csv"
hdr = {'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11',
       'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
       'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
       'Accept-Encoding': 'none',
       'Accept-Language': 'en-US,en;q=0.8', #cambia idioma 'Accept-Language': 'es-ES,es;q=0.8' 	
       'Connection': 'keep-alive'}
 
count=0
with open(outputpath, "w") as outfile:
    
    writer = csv.writer(outfile)
    writer.writerow(['id','url','longitude','latitude','found','revised','host-id','room_type','bedrooms','capacity','reviews','min_nights','price','reviews_'+today,'lastreview_'+today,'exists_'+today])
    with open(infile, "r") as f:
        csvf = csv.DictReader(f, delimiter=',')
        for row in csvf:

            # uncomment the following two lines
            # to debug with first 5 rows of input file
            if count == 5:
                break

            count +=1
            time.sleep(3)
            print ""
            print count
            url = row['url'].replace("airbnb.es","airbnb.com",1)
            print url
          
            try:
                # Get URL
                response = urllib2.Request(url, headers=hdr)
                pagedata = urllib2.urlopen(response)
                if pagedata.geturl() == url:
                    exists = 1
                    print "URL found! :)"
                    html = pagedata.read()
                    # Get page code
                    soup = BeautifulSoup(html, "html.parser")

                    try:
                        nreviewsRaw = soup.select("#reviews ._1xu9tpch")[0].get_text().encode('utf-8').strip()
                        nreviewsArray = re.match('\d{1,}',nreviewsRaw)
                        nreviews = nreviewsArray.group()
                        #nreviews = nreviews.translate(None, ' Reviews').encode('utf-8').strip()

                        lastreviews = soup.select("#reviews ._17oldnte")
                        lrdates = []
                        for lr in lastreviews:
                            lrstr = lr.get_text().encode('utf-8').strip()
                            lrdate = datetime.strptime(lrstr,'%B %Y')
                            lrdates.append(lrdate)

                        lastreview = max(lrdates)
                        print "Reviews data found"

                    except:
                        nreviews = 0
                        lastreview = 0
                        print "No reviews for this listing"

                else:
                    exists = 0
                    nreviews = 0
                    lastreview = 0
                    print "URL not found :("

            except:
                exists = 0
                nreviews = 0
                lastreview = 0
                print "URL not found :("

            writer.writerow([row['id'],url,row['longitude'],row['latitude'],row['found'],row['revised'],row['host-id'],row['room_type'],row['bedrooms'],row['capacity'],row['reviews'],row['min_nights'],row['price'],nreviews,lastreview,exists])
            print "Data saved."

            pagedata.close()
