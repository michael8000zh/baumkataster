# Masterarbeit Baumkataster ZHAW
# Dieses Script bereinigt die Daten vpon Grün Stadt Zürich. Es werden zwei Komplexitätsreduktionen
# durchgeführt, die Koordinaten von LV95 zu WGS84 umgerechnet und die nicht mehr benötigten
# Spalten entfernt.

# Michael Hilti (michael.hilti@gmail.com)
# 2021-12-21, 23.35 Uhr

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
input_file <- "data/gsz.baumkataster_baumstandorte.csv"
output_file <- "data/kataster.csv"

# Einlesen gsz.baumkataster_baumstandorte.csv, speichern als kataster
kataster <- read_delim(input_file, delim = ",", col_names = TRUE)

# Die Obstbäume weisen eine sehr genau aufzeichnung der Sorte auf. Diese erschwert die Anaylse.
# Aus 'Apfel-Obstgehölz 'Rafzubin' (Rubinette)' wird 'Apfel-Obstgehölz' gemacht.
# gleiches Verfahren mit: Aprikose-Obstgehölz, Birne-Obstgehölz, Esskastanie-Obstgehölz, Pflaume-Obstgehölz
# Pfirsich-Obstgehölz, Quitte-Obstgehölz, Sauer-Kirsche-Obstgehölz, Süss-Kirsche-Obstgehölz, Walnuss-Obstgehölz
# Zwetschge-Obstgehölz

# neue Spalte auf Basis von 'baumnamedeu'
kataster$baumnamedeu_kompakt = kataster$baumnamedeu

# mutate strings der Baumarten um Komplexität zu reduzieren.
kataster$baumnamedeu_kompakt <- sub("Apfel-Obstgehölz.*", "Apfel-Obstgehölz", kataster$baumnamedeu_kompakt)
kataster$baumnamedeu_kompakt <- sub("Aprikose-Obstgehölz.*", "Aprikose-Obstgehölz", kataster$baumnamedeu_kompakt)
kataster$baumnamedeu_kompakt <- sub("Birne-Obstgehölz.*", "Birne-Obstgehölz", kataster$baumnamedeu_kompakt)
kataster$baumnamedeu_kompakt <- sub("Esskastanie-Obstgehölz.*", "Esskastanie-Obstgehölz", kataster$baumnamedeu_kompakt)
kataster$baumnamedeu_kompakt <- sub("Pflaume-Obstgehölz.*", "Pflaume-Obstgehölz", kataster$baumnamedeu_kompakt)
kataster$baumnamedeu_kompakt <- sub("Pfirsich-Obstgehölz.*", "Pfirsich-Obstgehölz", kataster$baumnamedeu_kompakt)
kataster$baumnamedeu_kompakt <- sub("Quitte-Obstgehölz.*", "Quitte-Obstgehölz", kataster$baumnamedeu_kompakt)
kataster$baumnamedeu_kompakt <- sub("Sauer-Kirsche-Obstgehölz.*", "Sauer-Kirsche-Obstgehölz", kataster$baumnamedeu_kompakt)
kataster$baumnamedeu_kompakt <- sub("Süss-Kirsche-Obstgehölz.*", "Süss-Kirsche-Obstgehölz", kataster$baumnamedeu_kompakt)
kataster$baumnamedeu_kompakt <- sub("Walnuss-Obstgehölz.*", "Walnuss-Obstgehölz", kataster$baumnamedeu_kompakt)
kataster$baumnamedeu_kompakt <- sub("Zwetschge-Obstgehölz.*", "Zwetschge-Obstgehölz", kataster$baumnamedeu_kompakt)

# Erfolgskontrolle: von 1196 auf 566 Baumarten.
zahl_baumnamedeu <- table(kataster$baumnamedeu)
dim(zahl_baumnamedeu)
zahl_baumnamedeu_kompakt <- table(kataster$baumnamedeu_kompakt)
dim(zahl_baumnamedeu_kompakt)

# Um die allergietreibenden Bäume visualisieren zu können, werden z.B Unterarten der Esche wie "Strassen-Esche" auf "Esche" reduziert. 
# Dies wird für die folgenden Bäume durchgeführt:
# Birke, Buche, Eiche, Erle, Esche, Espe, Hasel, Kastanie, Kiefer, Linde, Platane, Pappel, Ulme, Weide

# neue Spalte "baumname_allergie" auf Basis von 'baumnamedeu_kompakt'
kataster$baumname_allergie = kataster$baumnamedeu_kompakt

# mutate strings der Baumarten um Komplexität zu reduzieren.
kataster$baumname_allergie <- gsub(".*Birke.*|.*birke*", "Birke", kataster$baumname_allergie)
kataster$baumname_allergie <- gsub(".*Buche.*|.*buche.*", "Buche", kataster$baumname_allergie)
kataster$baumname_allergie <- gsub(".*Eiche.*|.*eiche*", "Eiche", kataster$baumname_allergie)
kataster$baumname_allergie <- gsub(".*Erle.*|.*erle*", "Erle", kataster$baumname_allergie)
kataster$baumname_allergie <- gsub(".*Esche.*|.*esche*", "Esche", kataster$baumname_allergie)
kataster$baumname_allergie <- gsub(".*Espe.*|.*espe.*", "Espe", kataster$baumname_allergie)
kataster$baumname_allergie <- gsub(".*Hasel.*|.*hasel.*", "Hasel", kataster$baumname_allergie)
kataster$baumname_allergie <- gsub(".*Kastanie.*|.*kastanie.*", "Kastanie", kataster$baumname_allergie)
kataster$baumname_allergie <- gsub(".*Kiefer.*|.*kiefer.*", "Kiefer", kataster$baumname_allergie)
kataster$baumname_allergie <- gsub(".*Linde.*|.*linde.*", "Linde", kataster$baumname_allergie)
kataster$baumname_allergie <- gsub(".*Platane.*|.*platane.*", "Platane", kataster$baumname_allergie)
kataster$baumname_allergie <- gsub(".*Pappel.*|.*pappel.*", "Pappel", kataster$baumname_allergie)
kataster$baumname_allergie <- gsub(".*Ulme.*|.*ulme.*", "Ulme", kataster$baumname_allergie)
kataster$baumname_allergie <- gsub(".*Weide.*|.*weide.*", "Weide", kataster$baumname_allergie)

#Erfolgskontrolle: von 566 auf 365 verschiedene Baumarten.
dim(zahl_baumnamedeu_kompakt)
zahl_baumname_allergie <- table(kataster$baumname_allergie)
dim(zahl_baumname_allergie)

# Vorbereitung der Umrechnung der Koodinaten LV95 in WGS84

# Neue Spalte auf Basis von 'geometry'
kataster$geometry_temp <- kataster$geometry

# Entfernen von 'POINT (' und ')'
kataster$geometry_temp <- sub("POINT \\(", "", kataster$geometry_temp)
kataster$geometry_temp <- sub("\\)", "", kataster$geometry_temp)

# Split 'geometry_temp' on whitespace into 'che_y' and 'che_x'
kataster <- separate(kataster, geometry_temp, into = c("che_y", "che_x"), sep = " ")
kataster$che_x <- as.numeric(kataster$che_x)
kataster$che_y <- as.numeric(kataster$che_y)

# Erfolgskontrolle: Kommastellen bleiben erhalten.
print(head(kataster$che_x, 10))
print(head(kataster$che_y, 10))

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
print(head(kataster, 10))



# Auswahl der weiterhin benötigten Spalten
kataster <- kataster %>% 
  select(baumnummer, quartier, baumnamedeu, baumtyptext, pflanzjahr, kronendurchmesser, baumnamedeu_kompakt, baumname_allergie, lat, lng)


# Abspoeichern des Datensatzes als CSV-Datei
write.csv(kataster, output_file, row.names = TRUE)
