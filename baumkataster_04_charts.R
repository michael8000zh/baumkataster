# Masterarbeit Baumkataster ZHAW
# In diesem Scrip werden die Prototypen der Chartauswertungen zu den Bäumen gebaut..

# Michael Hilti (michael.hilti@gmail.com)
# 2021-12-14, 18.50 Uhr

# -*- coding: utf-8 -*-

# Laden der benötigten Packages
library(shiny)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(leaflet)

# install.packages("ggplot2")
# install.packages("RColorBrewer")

# Alle vorhandenen Objekte im Workspace löschen
# rm(list = objects(pattern = ".*"))

# Festlegen des Input Files
input_file <- "data/kataster.csv"

# Einlesen kataster.csv, speichern als kataster
kataster <- read_delim(input_file, delim = ",", col_names = TRUE)

# Festlegen der auszuwählenden, allergietreibenden Bäume
allergiebäume <- c("Birke", "Buche", "Eiche", "Erle", "Esche", "Espe", "Hasel", "Kastanie",
                   "Kiefer", "Linde", "Platane", "Pappel", "Ulme", "Weide")

quartiere <- c("Affoltern", "Albisrieden", "Alt-Wiedikon", "Altstetten", "ausserhalb Stadtgebiet",
               "City", "Enge", "Escher Wyss", "Fluntern", "Friesenberg", "Gewerbeschule", "Hard",
               "Hirslanden", "Hirzenbach", "Hochschulen", "Höngg", "Hottingen", "Langstrasse",
               "Leimbach", "Lindenhof", "Mühlebach", "Oberstrass", "Oerlikon", "Rathaus",
               "Saatlen", "Schwammendingen-Mitte", "Seebach", "Seefeld", "Sihlfeld", "Unterstrasse",
               "Weinegg", "Werd", "Wipkingen", "Witikon", "Wollishofen")

# Auswertung der häufigsten Bäume nach Quartier
pal <- colorFactor(brewer.pal(10, "Set3"), kataster$baumnamedeu_kompakt)

top10_quartier <- kataster %>% 
  add_count(baumnamedeu_kompakt) %>%
  filter(dense_rank(-n) < 11) %>%
  group_by(quartier, baumnamedeu_kompakt) %>%
  summarize(n = n())

plot <- ggplot(top10_quartier, aes(fill = baumnamedeu_kompakt, y = n, x = quartier)) +
  geom_bar(position = "fill", stat = "identity", width = 0.6) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank(),
        axis.title.y = element_blank(), panel.grid.major = element_blank()) + 
  labs(title = "Prozentuale Verteilung der 10 häufigsten Bäume nach Quartier", fill = "Baumname")
  
print(plot)

# Auswertung der allergietreibenden Bäume nach Quartier
