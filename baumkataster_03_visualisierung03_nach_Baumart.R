# Masterarbeit Baumkataster ZHAW
# In diesem Script werden die Prototypen der Visualisierungen erstellt.

# Michael Hilti (michael.hilti@gmail.com)
# 2021-11-27, 23.46 Uhr

# -*- coding: utf-8 -*-

# Visualisierung 03 dient als Prototyp für die Erstellung einer Density-Map Auswertung 
# einer Baumart über die ganze Stadt Zürich

# Alle vorhandenen Objekte im Workspace löschen
rm(list = objects(pattern = ".*"))

# Festlegen der benötigten Libraries
packages = c("tidyverse", "leaflet", "RColorBrewer", "leaflet.extras")

## Laden oder installieren der benötigen LibrariesNow load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Festlegen der Anzahl verwendeten Nachkomastellen
options(digits = 9)

# Festlegen des In- und Output Files
input_file <- "data/baumkataster_clean_wgs84.csv"
# output_file <- "data/baumkataster_clean_wgs84.csv"

# Einlesen baumkataster_clean_wgs84.csv, speichern als kataster
kataster <- read_delim(input_file, delim = ",", col_names = TRUE)


# Filtern  des Katasters nach den zehn meistvorkommenden Bäumen in der ganzen Stadt
top10_auswahl <- kataster %>%
  add_count(baumnamedeu_kompakt) %>%
  filter(dense_rank(-n) < 11)

#Erfolgskontrolle
table(top10_auswahl$baumnamedeu_kompakt)

# Auswahl des gewünschten Baums. Zur Verfügung stehen: 
# "Apfel-Obstgehölz", "Berg-Ahorn, Wald-Ahorn", "Eibe, Gewöhnliche Eibe", 
# "Feld-Ahorn, Hecken-Ahorn", "Gemeine Esche", "Gemeine Hain- oder Weissbuche",
# "Gewöhnliche Kiefer, Wald-Kiefer, Föhre", "Platane (occidentalis x orientalis)", 
# "Sand-Birke, Weiss-Birke", "Spitz-Ahorn"

select_baum <- "Apfel-Obstgehölz"

# Aufbau des gefilterten Datensets
top10 <- kataster %>%
  filter(baumnamedeu_kompakt == select_baum)

# Aufbau der Karte
m <- leaflet() %>% addTiles()

# Aufbauen der Farbpalette für 10 Farben
mypalette <- brewer.pal(10, "Paired")
pal <- colorFactor(mypalette, domain = top10$baumnamedeu_kompakt)

# Übergabe neues Datenset für Graphische Auswertung
leaflet(data = top10) %>% addTiles() %>%
  addHeatmap(lng = ~lng,
                   lat = ~lat,
                   intensity = ~baumnamedeu_kompakt,
                   max = 10,
                   radius = 8,
                   blur = 10) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~baumnamedeu_kompakt,
            title = paste("Verteilung der Art ", select_baum, "in der ganzen Stadt"),
            opacity = 1)
