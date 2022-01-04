# Masterarbeit Baumkataster ZHAW
# In diesem Script werden die Daten von Grün Stadt Zürich eingelesen und
# explorativ ausgewertet, um die notwendigen Variablen zu bestimmen.

# Michael Hilti (michael.hilti@gmail.com)
# 2021-12-30, 18.46 Uhr

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
sum(is.na(kataster$quartier))
sum(is.na(kataster$strasse))
sum(is.na(kataster$baumnamedeu))
sum(is.na(kataster$baumgattunglat))
sum(is.na(kataster$baumartlat))
sum(is.na(kataster$baumnamelat))
sum(is.na(kataster$status))
sum(is.na(kataster$baumtyp))
sum(is.na(kataster$pflanzjahr))
sum(is.na(kataster$kronendurchmesser))
sum(is.na(kataster$genauigkeit))
sum(is.na(kataster$geometry))

# Graphische Auswertung der Baumverteilung nach Quartier
data.plot1 <- kataster %>%
  group_by(quartier) %>%
  summarize(n = n())

# Aufbau des Barplots
plot1 <- ggplot(data = data.plot1, aes(y = reorder(quartier, n), x = n)) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  geom_text(aes(label = n), hjust = -0.3, size = 3.0, color = "black") + 
  theme_minimal() + 
  theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Verteilung Anzahl Bäume nach Quartier",
       subtitle = paste("Total", sum(data.plot1$n)))

# Ausgabe des Barplots
print(plot1)

# Graphische Auswertung der Baumverteilung nach Status
data.plot2 <- kataster %>%
  group_by(status) %>%
  summarize(n = n())

# Aufbau des Barplots
plot2 <- ggplot(data = data.plot2, aes(y = reorder(status, n), x = n)) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  geom_text(aes(label = n), hjust = -0.3, size = 3.0, color = "black") + 
  theme_minimal() + 
  theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Verteilung Anzahl Bäume nach Status",
       subtitle = paste("Total", sum(data.plot1$n)))

# Ausgabe des Barplots
print(plot2)

# Graphische Auswertung der Baumverteilung nach Baumtyptext
data.plot3 <- kataster %>%
  group_by(baumtyptext) %>%
  summarize(n = n())

# Aufbau des Barplots
plot3 <- ggplot(data = data.plot3, na.rm = TRUE, aes(y = reorder(baumtyptext, n), x = n)) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  geom_text(aes(label = n), hjust = -0.3, size = 3.0, color = "black") + 
  theme_minimal() + 
  theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Verteilung Anzahl Bäume nach Baumtyp",
       subtitle = paste("Total", sum(data.plot1$n)))

# Ausgabe des Barplots
print(plot3)

# Graphische Auswertung der Baumverteilung nach Kronendurchmesser
data.plot4 <- kataster %>%
  group_by(kronendurchmesser) %>%
  summarize(n = n())

# Aufbau des Barplots
plot4 <- ggplot(data = data.plot5, na.rm = TRUE, aes(y = reorder(kronendurchmesser, n), x = n)) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  geom_text(aes(label = n), hjust = -0.3, size = 3.0, color = "black") + 
  theme_minimal() + 
  theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Verteilung Anzahl Bäume nach Kronendurchmesser",
       subtitle = paste("Total", sum(data.plot1$n)))

# Ausgabe des Barplots
print(plot4)

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

# Die Kategorie Baumnamedeu sieht vielversprechend aus. Sie ist auch für Nicht-Botaniker verständlich.
# Aus dieser Kategorie werden zwei neue Spalten gebaut: 
# "Baumnamedeu_kompakt" mit der Komplexitätsreduktion der Obstgehölz und "baumnamedeu_allergie" mit
# der Komplexitätsreduktion der allergietreibenden Bäume.

