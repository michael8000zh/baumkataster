# Masterarbeit Baumkataster ZHAW
# Michael Hilti (michael.hilti@gmail.com)
# 2021-11-22, 09.22 Uhr

# Einlesen der Daten und Data Cleaning

# Festlegen des Working directories
setwd("/Users/michael/Documents/Data Science/Masterarbeit_ZHAW/")

# Laden der benötigten Libraries
library(tidyverse)

# Einlesen gsz.baumkataster_baumstandorte.csv, speichern als kataster
kataster <- read_delim("gsz.baumkataster_baumstandorte.csv", delim = ",", col_names = TRUE)

# Faktorisiereng der Spalten kategorie, quartier, status, baumtyp, genauigkeit, baumartlat, baumgattunglat,
# baumnamedeu, baumnamelat 
kataster$kategorie <- as.factor(kataster$kategorie)
kataster$quartier <- as.factor(kataster$quartier)
kataster$status <- as.factor(kataster$status)
kataster$baumtyp <- as.factor(kataster$baumtyp)
kataster$genauigkeit <- as.factor(kataster$genauigkeit)
kataster$baumartlat <- as.factor(kataster$baumartlat)
kataster$baumgattunglat <- as.factor(kataster$baumgattunglat)
kataster$baumnamedeu <- as.factor(kataster$baumnamedeu)
kataster$baumnamelat <- as.factor(kataster$baumnamelat)

# Nummerischen Auswertung des Datensatzes
table(kataster$kategorie)
table(kataster$quartier)
table(kataster$status)
table(kataster$baumtyp)
table(kataster$genauigkeit)
table(kataster$baumartlat)
table(kataster$baumgattunglat)
table(kataster$baumnamedeu)
table(kataster$baumnamelat)

hist(kataster$pflanzjahr)
hist(kataster$kronendurchmesser)

# Eruieren der NA-Werte pro Variable
summary(kataster)

sum(is.na(kataster$baumartlat))
sum(is.na(kataster$baumnamelat))
sum(is.na(kataster$status))
sum(is.na(kataster$baumtyp))
sum(is.na(kataster$pflanzjahr))
sum(is.na(kataster$kronendurchmesser))

# Eruieren Anzahl Kategorien baumgattunglat: 123
zahl_baumgattunglat <- table(kataster$baumgattunglat)
dim(zahl_baumgattunglat)

# Eruieren Anzahl Kategorien baumnamedeu: 1196
zahl_baumnamedeu <- table(kataster$baumnamedeu)
dim(zahl_baumnamedeu)

# Eruieren Anzahl Kategorien baumnamelat: 1492
zahl_baumnamelat <- table(kataster$baumnamelat)
dim(zahl_baumnamelat)

# Eruieren Anzahl Kategorien baumartlat: 379
zahl_baumartlat <- table(kataster$baumartlat)
dim(zahl_baumartlat)

# Kein unmittelbar schlüssiges Ergebnis, welche Kategoriesierung am Besten
# geeignet ist für die weitere Analyse.

# var baumgattunglat
zahl_baumgattunglat.df <- as.data.frame(zahl_baumgattunglat)
zahl_baumgattunglat.df <- zahl_baumgattunglat.df[order(-zahl_baumgattunglat.df$Freq),]
print(zahl_baumgattunglat.df)

# var baumnamedeu
zahl_baumanmedeu.df <- as.data.frame(zahl_baumnamedeu)
zahl_baumanmedeu.df <- zahl_baumanmedeu.df[order(-zahl_baumanmedeu.df$Freq),]
print(zahl_baumanmedeu.df)

# var baumnamelat
zahl_baumnamelat.df <- as.data.frame(zahl_baumnamelat)
zahl_baumnamelat.df <- zahl_baumnamelat.df[order(-zahl_baumnamelat.df$Freq),]
print(zahl_baumnamelat.df)

# var baumartlat
zahl_baumartlat.df <- as.data.frame(zahl_baumartlat)
zahl_baumartlat.df <- zahl_baumartlat.df[order(-zahl_baumartlat.df$Freq),]
print(zahl_baumartlat.df)

# Die Kategorie Baumnamedeu sieht vielversprechend aus. Sie ist auch für nicht Botaniker verständlich.
# Die Obstbäume weisen eine sehr genau aufzeichnung der Sorte auf. Diese erschwert die Anaylse.
# Aus 'Apfel-Obstgehölz 'Rafzubin' (Rubinette)' wird 'Apfel-Obstgehölz' gemacht.
# gleiches Verfahren mit: Aprikose-Obstgehölz, Birne-Obstgehölz, Esskastanie-Obstgehölz, Pflaume-Obstgehölz
# Pfirsich-Obstgehölz, Quitte-Obstgehölz, Sauer-Kirsche-Obstgehölz, Süss-Kirsche-Obstgehölz, Walnuss-Obstgehölz
# Zwetschge-Obstgehölz

# neue Spalte auf Basis von 'baumnamedeu'
kataster.df <- as.data.frame(kataster)
kataster.df$baumnamedeu_kompakt = kataster.df$baumnamedeu

# mutate string
kataster.df$baumnamedeu_kompakt <- sub("Apfel-Obstgehölz.*", "Apfel-Obstgehölz", kataster.df$baumnamedeu_kompakt)
kataster.df$baumnamedeu_kompakt <- sub("Aprikose-Obstgehölz.*", "Aprikose-Obstgehölz", kataster.df$baumnamedeu_kompakt)
kataster.df$baumnamedeu_kompakt <- sub("Birne-Obstgehölz.*", "Birne-Obstgehölz", kataster.df$baumnamedeu_kompakt)
kataster.df$baumnamedeu_kompakt <- sub("Esskastanie-Obstgehölz.*", "Esskastanie-Obstgehölz", kataster.df$baumnamedeu_kompakt)
kataster.df$baumnamedeu_kompakt <- sub("Pflaume-Obstgehölz.*", "Pflaume-Obstgehölz", kataster.df$baumnamedeu_kompakt)
kataster.df$baumnamedeu_kompakt <- sub("Pfirsich-Obstgehölz.*", "Pfirsich-Obstgehölz", kataster.df$baumnamedeu_kompakt)
kataster.df$baumnamedeu_kompakt <- sub("Quitte-Obstgehölz.*", "Quitte-Obstgehölz", kataster.df$baumnamedeu_kompakt)
kataster.df$baumnamedeu_kompakt <- sub("Sauer-Kirsche-Obstgehölz.*", "Sauer-Kirsche-Obstgehölz", kataster.df$baumnamedeu_kompakt)
kataster.df$baumnamedeu_kompakt <- sub("Süss-Kirsche-Obstgehölz.*", "Süss-Kirsche-Obstgehölz", kataster.df$baumnamedeu_kompakt)
kataster.df$baumnamedeu_kompakt <- sub("Walnuss-Obstgehölz.*", "Walnuss-Obstgehölz", kataster.df$baumnamedeu_kompakt)
kataster.df$baumnamedeu_kompakt <- sub("Zwetschge-Obstgehölz.*", "Zwetschge-Obstgehölz", kataster.df$baumnamedeu_kompakt)

# Erfolgskontrolle
zahl_baumnamedeu_kompakt <- table(kataster.df$baumnamedeu_kompakt)
dim(zahl_baumnamedeu_kompakt)
dim(zahl_baumnamedeu)
# Neu Anzahl der Werte in baumnamedeu_kompakt reduziert von 1196 auf 566.

# Für die Weiterverwendung der Koordinaten in Leaflet, müssen die Schweizer Y- und X-Koordinaten in separaten Spalten aufgeteilt werden. 
# POINT (2686027.5 1251531.4)

# Neue Spalte auf Basis von 'geometry'
kataster.df$geometry_temp <- kataster.df$geometry

# Entfernen von 'POINT (' und ')'
kataster.df$geometry_temp <- sub("POINT \\(", "", kataster.df$geometry_temp)
kataster.df$geometry_temp <- sub("\\)", "", kataster.df$geometry_temp)
kataster.df$geometry_temp

# Split 'geometry_temp' on whitespace into 'che_y' and 'che_x'
kataster.df <- separate(kataster.df, geometry_temp, into = c("che_y", "che_x"), sep = " ")
print(head(kataster.df$che_x, 20))
print(head(kataster.df$che_y, 20))

## Hier sind die Kommanstellen (Zeilte 133) noch vorhanden... in LV95 nicht mehr. Was läuft falsch?

# Umrechnung von LV95 Koordinatensystem auf WGS84 Dezimalsystem erfolgt im Script baumkataster_02_
# Source: https://github.com/ValentinMinder/Swisstopo-WGS84-LV03/blob/master/scripts/r/WGS84_CH1903.R

# Abspoeichern des Datensatzes als CSV-Datei
write.csv(kataster.df, "/Users/michael/Documents/Data Science/Masterarbeit_ZHAW/data_01.csv", row.names = TRUE)
