# Masterarbeit Baumkataster ZHAW
# In diesem Script werden die Prototypen der Visualisierungen erstellt.

# Michael Hilti (michael.hilti@gmail.com)
# 2021-12-03, 21.07 Uhr

# -*- coding: utf-8 -*-

# Visualisierung 01 dient als Prototyp für die graphische Auswertung der Top 10
# der in einem Quartier vorhandenen Bäume.

# Alle vorhandenen Objekte im Workspace löschen
rm(list = objects(pattern = ".*"))

# Festlegen der benötigten Libraries
packages = c("tidyverse", "leaflet", "RColorBrewer")

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
input_file <- "data/baumkataster_leicht.csv"

# Einlesen baumkataster_clean_wgs84.csv, speichern als kataster
kataster <- read_delim(input_file, delim = ",", col_names = TRUE)



# Filtern nach gewünschtem Quartiernamen. Zur Auswahl stehen:
# Schwamendingen-Mitte, Albisrieden, Hirzenbach, Altstetten. Hard, Fluntern, Friesenberg, Wipkingen, Seebach, Unterstrass,
# Oberstrass, Oerlikon, Saatlen, Weinegg, Wollishofen, Hottingen, City, Enge, Alt-Wiedikon, Leimbach, Seefeld, Witikon,
# Hirslanden, Sihlfeld, Höngg, Escher Wyss, Mühlebach, Langstrasse, Werd, Hochschulen, Lindenhof, Gewerbeschule, Rathaus,
# Affoltern.

quartier_name <- "Langstrasse"



# Filtern  des Katasters nach den zehn meistvorkommenden Bäumen in Hard
top10 <- kataster %>%
  filter(quartier == quartier_name) %>%
  add_count(baumnamedeu_kompakt) %>%
  filter(dense_rank(-n) < 11)

#Erfolgskontrolle
table(top10$baumnamedeu_kompakt)

# Aufbauen der Farbpalette für 10 Farben
pal <- colorFactor(brewer.pal(10, "Set3"), domain = top10$baumnamedeu_kompakt)

# Übergabe neues Datenset für Graphische Auswertung
m <- leaflet(data = top10) %>% 
  addTiles() %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE)) %>%
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
            title = paste("Top 10 Baumarten ", quartier_name),
            opacity = 1)

# Ausgabe der Karte
print(m)

# Ausfiltern der Top 10 Bäume im ausgewählten Quartier
top10_quartier <- kataster %>%
  add_count(baumnamedeu_kompakt) %>%
  filter(dense_rank(-n) < 11) %>%
  filter(quartier == quartier_name) %>%
  group_by(quartier, baumnamedeu_kompakt) %>%
  summarize(n = n())

# Graphische Verteilung der Top 10 Baumarten der Stadt Zürich im ausgewählten Quartier
g <- ggplot(top10_quartier,
            aes(fill=baumnamedeu_kompakt,
                y = n,
                x = quartier)) +
  geom_bar(position = "stack",
           stat = "identity",
           width = 0.6) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = n), 
            size = 3,
            position = position_stack(vjust = 0.5)) +
  labs(title =paste("Anzahl vorhandene Bäume in", quartier_name), 
       fill = "Baumname Deutsch",
       subtitle = paste("n =", sum(top10_quartier$n)))

# Ausgabe der Graphik
print(g)
