Efecto de Airbnb
===

Más información sobre los materiales producidos durante los talleres en la [Wiki]https://wiki.montera34.com/airbnb).

## Taller abril 2017

Este repositorio alojaba inicialmente toda la información utilizada en el taller [Maps&Data: El efecto Airbnb en datos](https://montera34.com/project/efecto-airbnb-donostia/) realizado en Hirikilabs de Donosti durante los días 3, 4 y 5 de abril de 2017. 

## Efecto Airbnb. Nodo data commons. Summerlab'17. Julio 2017

Con la ampliación de la investigación sobre Airbnb en [Efecto Airbnb](https://lab.montera34.com/airbnb) que tuvo lugar durante el Summerlab'17 en Donosti los das 13 y 14 de julio de 2017, este repositorio alojó también el contenido de diferentes proyectos de captura y análisis de datos.

## Taller Efecto Airbnb. Abril 2018

Ha ido albergando la información relacionada con los diferentes talleres sobre el mismo tema.

# Estructura de contenidos por carpetas

    prepare-segmented-files.R   Script de R para generar los archivos .csv por ciudades y simplificados
    
    analisis/     
      |_ airbnb-analisis.R Script que analiza habitaciones vs Vivv comleta, anuncios por barop. ratio anuncios por barrio-viv, nº plazas, ratio plazas/habitantes
      |_ extra-info-airbnb.R script to export listings id to scrap information
      |_ gestores-vs-usuarios.R Este script clasifica los listings según su último y primer review, según el tipo de usuario que los posee 
      |_ hosts-analysis.Rmd genera informe de hosts en un municipio    
      |_ idealista-analysis.R análisis de datos Idealista   
      |_ points-in-polygons.R Script que inserta en puntos (listings) el lugar donde están ubicados (barrios y uniades menores)
      |_ preanalisis-airbnb.Rmd Genera informe a partir de datos de InsideAirbnb.com
      |_ preanalisis-efecto-airbnb-donostia.Rmd Genera informe a partir de datos de InsideAirbnb.com (desactualizado)
      |_ last-review-analisis.R análisis last review para activos
      |_ listings_activos_filter.R Este script filtra una base de datos de Airbnb para seleccionar los listings activos Se basa en datos de datahippo.org y de scraping posterior para obener fecha de last review
      |_ listings_activos.R Análisis de Airbnb en Donostia: anuncios activos en base a datos de datahippo.org y scraping para last review. Incluye generación de visualizaciones para ver y calcular el número de anuncios activos
      |_ review-analysis.R Este script analiza los reviews de una localización para estudiar estacionalidad y evolución de uso
      |_ vut-donostia.R Análisis de Viviendas de Uso Turístico (VUT) en Donostia
      |
      |_ montera34/              Scripts de análisis de Montera34
         |_airbnb-...-region.Rmd Archivo Rmd para generar informe automatizado en .html en Rstudio con Knitr sobre una region
         |_airbnb-...-ciudad.Rmd Archivo Rmd para generar informe automatizado en .html en Rstudio con Knitr sobre un municipio
         |_*.html                Informes de ciudades generados por airbnb-informe-ciudad.Rmd
         |_*.R                   Diferentes scripts de análisis, preparación de datos y gráficos
      |_ terraferida/            Scripts de análisis de datos en R de Terraferida


    data/                       Versiones limpias de las bases de datos de la carpeta original del proyecto
      |
      |_ original/              Datos sin tratar, tal como los encontramos en las fuentes
      |_ output/                Bases de datos modificadas y remezcladas

    documentos/                 Informes y documentos sobre la situación de los apartamentos turísticos en Donosti
    
    images/                     Imágenes de gráficos generadas

    presentaciones/             Presentaciones usadas en el taller
      |_ 170403_visual...       Presentación y sesión teórica: algo de teoría sobre la visualización de datos
      |_ listing_donos...       Base de datos de alojamientos en Airbnb de Donosti en PDF
      |_ presentacion-fai...    Presentación Fairbnb Sito Veracruz
      
    scraping/                   Scripts para scrapear datos de Airbnb
    
# Procesado de datos

Este es el procesado de los datos (puntos de coordenadas de anuncios de Airbnb) para calcular la ubicación de los puntos en contornos (barrios o unidades menores).

1. Partimos del archivo de datos fuente descargado de datahippo: 180423_listings-airbnb-donostia_datahippo.csv
2. Calculamos la ubicación por barrio y unidad menor con el script: analisis/points-in-polygons.R. Que genera el archivo 180423_listings-airbnb-donostia_datahippo_barrio-umenor.csv
