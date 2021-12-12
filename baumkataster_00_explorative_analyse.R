# Masterarbeit Baumkataster ZHAW
# In diesem Script werden die Daten von Grün Stadt Zürich eingelesen und
# grob nummerisch augewertet, um die notwendigen Variablen zu bestimmen.

# Michael Hilti (michael.hilti@gmail.com)
# 2021-12-12, 13.58 Uhr

# Alle vorhandenen Objekte im Workspace löschen
rm(list = objects(pattern = ".*"))

# Laden der benötigten Libraries
library(tidyverse)

# Festlegen der Anzahl verwendeten Nachkomastellen
options(digits = 9)

# Festlegen des In- und Output Files
input_file <- "data/gsz.baumkataster_baumstandorte.csv"

# Einlesen gsz.baumkataster_baumstandorte.csv, speichern als kataster
kataster <- read_delim(input_file, delim = ",", col_names = TRUE)

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
# Aus dieser Kategorie werden zwei neue Spalten gebaut: 
# "Baumnamedeu_kompakt" mit der Komplexitätsreduktion der Obstgehölz und "baumnamedeu_allergie" mit
# der Komplexitätsreduktion der allergietreibenden Bäume.
