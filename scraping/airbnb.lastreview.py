import csv
import urllib2
from bs4 import BeautifulSoup
 
inputids = "input.list" # nombre del archivo con los ids de los listings, uno por línea

outputpath = "airbnb.lastreview.csv"
hdr = {'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11',
       'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
       'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
       'Accept-Encoding': 'none',
       #'Accept-Language': 'en-US,en;q=0.8', #cambia idioma 'Accept-Language': 'es-ES,es;q=0.8' 	
       'Accept-Language': 'es-ES,es;q=0.8',
       'Connection': 'keep-alive'}
 
count=0
with open(outputpath, "w") as outfile:
 
  writer = csv.writer(outfile)
  with open(inputids, "r") as f:
      for counter in f:
          counter = counter.strip()
      #for counter in [8482514,1405167]:
          count +=1
          print count
          url = "https://www.airbnb.es/rooms/"+str(counter)
          print url
          
          try:
              print "try1"
              # Get URL
              response = urllib2.Request(url, headers=hdr)
              pagedata = urllib2.urlopen(response)
              html = pagedata.read()
              # Get links
              soup = BeautifulSoup(html, "html.parser")
          except:
              print "except1"
              
          try:
              print "try2"
              nreviews = soup.select("#reviews ._1vbkutid")[0].get_text().encode('utf-8').strip()
              #nreviews = nreviews.translate(None, ' Reviews').encode('utf-8').strip()
              lastreview = soup.select("#reviews ._hylizj6")[0].get_text().encode('utf-8').strip()
              print counter
              print nreviews
              print url
              print lastreview
              writer.writerow([counter,nreviews,lastreview])

          except:
              print "except2"
     
