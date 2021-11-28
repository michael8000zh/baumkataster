# Masterarbeit Baumkataster ZHAW
# In diesem Script werden die LV95 Koordinaten auf WGS84 umgerechnet.

# Michael Hilti (michael.hilti@gmail.com)
# 2021-11-23, 19.53 Uhr

# -*- coding: utf-8 -*-

# Das Baumkataster von Grün Stadt Zürich verwendet das in der Schweiz gebräuchliche Koordinatensystem
# LV95, um die genauen Standorte der Bäume zu bezeichnen. Leaflet benötigt jedoch Koordinaten
# im international gebräuchlichen WGS84 Dezimalkoordinatensystem. Dieses Script nimmt die
# Umrechnung vor. Als Grundlage wird eine leicht veränderte Version des Scripts Valentin Minder
# verwendet. Die Anpassung stellt die Umrechnung von LV95 sicher, während das Script von Minder
# auf LV03 ausgerichtet ist.
# Koordinaten in ihrer jeweiligen Form:
# LV95: 2 686 027.5, 1 252 531.4
# LV03: 686 027.5, 252 531.4
# WGS84: long 8.578453446, lat: 47.408977778

# Source: https://github.com/ValentinMinder/Swisstopo-WGS84-LV03/blob/master/scripts/r/WGS84_CH1903.R

# Alle vorhandenen Objekte im Workspace löschen
rm(list = objects(pattern = ".*"))

# Laden der benötigten Libraries
library(tidyverse)

# Festlegen der Anzahl verwendeten Nachkomastellen
options(digits = 9)

# Festlegen des In- und Output Files
input_file <- "data/baumkataster_clean.csv"
output_file <- "data/baumkataster_clean_wgs84.csv"

# Einlesen baumkataster_clean.csv, speichern als kataster
kataster <- read_delim(input_file, delim = ",", col_names = TRUE)

# Definieren der Umrechnungsfunktion nach Minder
CH.to.WGS.lat <- function (y, x){
  
  ## Converts military to civil and  to unit = 1000km
  ## Auxiliary values (% Bern)
  ## Hier wird von y 2600000 anstatt 600000 sowie 1200000 anstatt 200000 abgezogen. 
  y_aux <- (y - 2600000)/1000000
  x_aux <- (x - 1200000)/1000000
  
  ## Process lat
  lat <- {16.9023892 +
      3.238272 * x_aux -
      0.270978 * (y_aux^2) -
      0.002528 * (x_aux^2) -
      0.0447   * (y_aux^2) * x_aux -
      0.0140   * (x_aux^3)}
  
  ## Unit 10000" to 1 " and converts seconds to degrees (dec)
  lat <- lat * 100/36
  
  return(lat)  
}

# Ausführen der Funktion und Umrechen der Koordination von LV95 zu WGS84
kataster$lat <- CH.to.WGS.lat(kataster$che_y, kataster$che_x)

## Convert CH y/x to WGS long
CH.to.WGS.lng <- function (y, x){
  
  ## Converts military to civil and  to unit = 1000km
  ## Auxiliary values (% Bern)
  ## Hier wird von y 2600000 anstatt 600000 sowie 1200000 anstatt 200000 abgezogen. 
    y_aux <- (y - 2600000)/1000000
  x_aux <- (x - 1200000)/1000000
  
  ## Process long
  lng <- {2.6779094 +
      4.728982 * y_aux +
      0.791484 * y_aux * x_aux +
      0.1306   * y_aux * (x_aux^2) -
      0.0436   * (y_aux^3)}
  
  ## Unit 10000" to 1 " and converts seconds to degrees (dec)
  lng <- lng * 100/36
  
  return(lng)
}

# Ausführen der Funktion und Umrechen der Koordination von LV95 zu WGS84
kataster$lng <- CH.to.WGS.lng(kataster$che_y, kataster$che_x)

# Erfolgskontrolle
view(kataster)

# Abspoeichern des Datensatzes als CSV-Datei
write.csv(kataster, output_file, row.names = TRUE)
