# Masterarbeit Baumkataster
# 2021-11-21, 10.02 Uhr

# Filtern und Visualisierung

# Festlegen des Working directories
setwd("/Users/michael/Documents/Data Science/Masterarbeit_ZHAW/")

#Installieren der benötigten Packages
# install.packages("tidyverse")
# install.packages("leaflet")

# Laden der benötigten Libraries
library(tidyverse)
library(leaflet)

# Einlesen gsz.baumkataster_baumstandorte.csv, speichern als kataster
kataster <- read_delim("data.csv", delim = ",", col_names = TRUE)

# Filtern der Einträge nach Quartier Höngg, Summieren der Baumarten
baum_hoengg <- kataster %>% filter(quartier == 'Höngg') %>%
  count(baumnamedeu_kompakt, sort = TRUE) %>%
  filter(n > 5) %>%
  filter(baumnamedeu_kompakt == 'Spitz-Ahorn')
view(baum_hoengg)

# Filtern der Einträge nach Quartier Höngg
filter_karte <- kataster %>%
  filter(quartier == 'Höngg')
view(filter_karte)

# Nummerische Auswertung der unterschiedlichen Bäume (baumnamedeu_kompakt)

# Filtern der top-10 Baumarten in Quartier Höngg

# Übergabe neues Datenset für Graphische Auswertung: 47.40231355095928, 8.49904495236159
m <- leaflet() %>% setView(lat = 47.4023135509592, lng = 8.49904495236159, zoom = 16)
m %>% addTiles()

leaflet(data = filter_karte[1:5,]) %>% addTiles() %>%
addMarkers(lng = ~lat, lat = ~long, popup = ~as.character(baumnamedeu_kompakt))

