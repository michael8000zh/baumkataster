# Masterarbeit Baumkataster ZHAW
# Dieses Script berechnet die Fläche pro Quartier aufgrund der Angaben von Open
# Data Stadt Zürich. Die Flächen werden in der Auswertung zur Berechnung der 
# Allergiebaumdichte nach Quartier verwendet.

# Datenquelle: https://data.stadt-zuerich.ch/dataset/geo_statistische_quartiere
# Statistische Quartiere der Stadt Zürich, Stadt Zürich Open Data.

# Michael Hilti (michael.hilti@gmail.com)
# 2021-12-21, 16.54 Uhr

# Alle vorhandenen Objekte im Workspace löschen
rm(list = objects(pattern = ".*"))

# Laden der benötigten Libraries
library(sf)
library(dplyr)

# Einlesen des Shape Files
shape <- st_read("data/input/quartiere_shape/stzh.adm_statistische_quartiere_map.shp")

# Berechnen der Fläche der Quartiere
shape$area_sqm <- st_area(shape$geometry)

# Schreiben des Shape File als CSV
st_write(shape, "data/flaeche.csv")

# Einlesen kataster.csv, speichern als kataster
flaeche <- read_delim("data/flaeche.csv", delim = ",", col_names = TRUE)

# Auswahl der weiterhin benötigten Spalten
flaeche <- flaeche %>% 
  dplyr::select(qname, kname, area_sqm)

# Umbenennen der Spalte qname zu Quartier
flaeche <- flaeche %>%
  rename(quartier = qname)

# Abspoeichern des Datensatzes als CSV-Datei
write.csv(flaeche, "data/flaeche.csv", row.names = TRUE)
