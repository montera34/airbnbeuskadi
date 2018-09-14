#!/bin/sh

# clean donostia vut dataset from
# https://www.donostia.eus/ataria/es/web/hirigintza/censo-de-viviendas-turisticas

today=`date +%Y%m%d`
ifile="../data/original/vut-donostia/censo-viviendas-turisticas-donostia-20180914.js"
ofile="../data/output/vut-donostia/censo-viviendas-turisticas-donostia-20180914.csv"

## TRANSFORM JS INTO CSV
sed '
s/^ *//g
1,/^var c = / d
/^var e = /,$ d
/^var .* = {/ d
/^name: / d
/^popupTitle: / d
/^markersOpacity: / d
/^extra_attributes: / d
/^cluster: / d
/^}/ d
/COREProxy/ d
/^icon: / s/^.*\/\([^\/]*\).png",$/\1,/
/^layerName: / s/^[^"]*"\([^"]*\)",$/"\1",/
/^popupContent: / s/^.*descripcionCabecera..\([^<]*\).*$/"\1",/
/^x: / s/^x: "\([0-9]*\.[0-9]*\)",$/\1,/
/^y: / s/^y: "\([0-9]*\.[0-9]*\)"$/\1/ ' < $ifile | sed '
/,$/ {
N
N
N
N
s/\(.*\)\n\(.*\)\n\(.*\)\n\(.*\)\n\(.*\)/\1\2\3\4\5/
}' | sed '
1 i\estado,tipo,dirección,x,y,longitude,latitude' > $ofile


# GEOCODING
#country="Spain"
#state="Gipuzkoa"
#city="San Sebastián"
#email=""

#php ea.donostia.vut.latlon.php "$country" "$state" "$city" $ofile "$email"
