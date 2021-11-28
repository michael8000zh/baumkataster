# Masterarbeit Baumkataster ZHAW
# In diesem Script werden die Prototypen der Visualisierungen erstellt.

# Michael Hilti (michael.hilti@gmail.com)
# 2021-11-28, 18.30 Uhr

# -*- coding: utf-8 -*-

# Visualisierung 02 dient als Prototyp für die graphische Auswertung der top-10 Baumarten
# in der ganzen Stadt Zürich. 

# Alle vorhandenen Objekte im Workspace löschen
rm(list = objects(pattern = ".*"))

# Festlegen der benötigten Libraries
packages = c("tidyverse", "leaflet", "RColorBrewer", "ggplot2")

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
top10 <- kataster %>%
  add_count(baumnamedeu_kompakt) %>%
  filter(dense_rank(-n) < 11)

#Erfolgskontrolle
table(top10$baumnamedeu_kompakt)

# Aufbauen der Farbpalette für 10 Farben
mypalette <- brewer.pal(10, "Paired")
pal <- colorFactor(mypalette, domain = top10$baumnamedeu_kompakt)

# Übergabe neues Datenset für Graphische Auswertung
m <- leaflet(data = top10) %>% addTiles() %>%
  addCircleMarkers(lng = ~lng,
                   lat = ~lat,
                   popup = ~as.character(baumnamedeu_kompakt),
                   radius = 4,
                   stroke = FALSE,
                   fillOpacity = 0.6,
                   color = ~pal(baumnamedeu_kompakt)) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~baumnamedeu_kompakt,
            title = "Top 10 Baumarten ganze Stadt",
            opacity = 1)
# Ausgeben der Karte
print(m)

# Darstellung der Baumarten in der Stadt Zürich nach Quartier
# Ausfiltern der Top 5 Bäume, aufsummieren nach Quartier
top5_alle_quartiere <- kataster %>%
  add_count(baumnamedeu_kompakt) %>%
  filter(dense_rank(-n) < 6) %>%
  group_by(quartier, baumnamedeu_kompakt) %>%
  summarize(n = n())

# Prozentuale Verteilung der top 5 Baumarten der Stadt Zürich nach Quartier
g <- ggplot(top5_alle_quartiere, aes(fill=baumnamedeu_kompakt, y = n, x = quartier)) +
  geom_bar(position = "fill", stat = "identity", width = 0.6) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "Prozentuale Verteilung der top 5 Baumarten pro Quartier", fill = "Baumname Deutsch")

# Ausgeben der Graphik
print(g)
