Efecto de Airbnb
===

## Taller abril 2017

Este repositorio era alojaba inicialmente toda la información utilizada en el taller [Maps&Data: El efecto Airbnb en datos](https://montera34.com/project/efecto-airbnb-donostia/) realizado en Hirikilabs de Donosti durante los días 3, 4 y 5 de abril de 2017.

Más información sobre los materiales producidos durante el taller en la [Wiki](https://github.com/montera34/airbnbeuskadi/wiki).

## Efecto Airbnb. Nodo data commons. Summerlab'17. Julio 2017

Con la ampliación de la investigación sobre Airbnb en [Efecto Airbnb](https://lab.montera34.com/airbnb) que tendrá lugar durante el Summerlab'17 en Donosti los das 13 y 14 de julio, este repositorio alojará el contenido de diferentes proyectos de captura y análisis de datos.


## Estructura de contenidos por carpetas

    prepare-segmented-files.R   Script de R para generar los archivos .csv por ciudades y simplificados
    airbnb-informe-region.Rmd   Archivo Rmd para generar informe automatizado en .html en Rstudio con Knitr sobre una ciudad
    *.html                      Informes de ciudades generados por airbnb-informe-region.Rmd

    data/                       Versiones limpias de las bases de datos de la carpeta original/
      |
      |_ original/              Datos sin tratar, tal como los encontramos en las fuentes
      |_ output/                Bases de datos modificadas y remezcladas

    documentos/                 Informes y documentos sobre la situación de los apartamentos turísticos en Donosti

    presentaciones/             Presentaciones usadas en el taller
      |_ 170403_visual...       Presentación y sesión teórica: algo de teoría sobre la visualización de datos
      |_ listing_donos...       Base de datos de alojamientos en Airbnb de Donosti en PDF
      |_ presentacion-fai...    Presentación Fairbnb Sito Veracruz
