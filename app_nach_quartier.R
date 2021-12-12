# Masterarbeit Baumkataster ZHAW
# In dieser Shiny App werden die häufigsten Baumarten pro Quartier dargestellt.

# Michael Hilti (michael.hilti@gmail.com)
# 2021-12-12, 12.16 Uhr

# -*- coding: utf-8 -*-

# Laden der benötigten Packages
library(shiny)
library(tidyverse)
library(leaflet)
library(RColorBrewer)

# Alle vorhandenen Objekte im Workspace löschen
rm(list = objects(pattern = ".*"))

# Festlegen des Input Files
input_file <- "shiny/kataster.csv"

# Einlesen baumkataster_leicht.csv, speichern als kataster
kataster <- read_delim(input_file, delim = ",", col_names = TRUE)

# Festlegen der auszuwählenden Quartiere der Stadt Zürich
list <- list("Affoltern" = "Affoltern",
             "Albisrieden" = "Albisrieden",
             "Alt-Wiedikon" = "Alt-Wiedikon",
             "Altstetten" = "Altstetten",
             "ausserhalb Stadtgebiet" = "ausserhalb Stadtgebiet",
             "City" = "City",
             "Enge" = "Enge",
             "Escher Wyss" = "Escher Wyss",
             "Fluntern" = "Fluntern",
             "Friesenberg" = "Friesenberg",
             "Gewerbeschule" = "Gewerbeschule",
             "Hard" = "Hard",
             "Hirslanden" = "Hirslanden",
             "Hirzenbach" = "Hirzenbach",
             "Hochschulen" = "Hochschulen",
             "Höngg" = "Höngg",
             "Hottingen" = "Hottingen",
             "Langstrasse" = "Langstrasse",
             "Leimbach" = "Leimbach",
             "Lindenhof" = "Lindenhof",
             "Mühlebach" = "Mühlebach",
             "Oberstrass" = "Oberstrass",
             "Oerlikon" = "Oerlikon",
             "Rathaus" = "Rathaus",
             "Saatlen" = "Saatlen",
             "Schwamendingen-Mitte" = "Schwamendingen-Mitte",
             "Seebach" = "Seebach",
             "Seefeld" = "Seefeld",
             "Sihlfeld" = "Sihlfeld",
             "Unterstrass" = "Unterstass",
             "Weinegg" = "Weinegg",
             "Werd" = "Werd",
             "Wipkingen" = "Wipkingen",
             "Witikon" = "Witikon",
             "Wollishofen" = "Wollishofen")

# Definieren der UI der Applikation
ui <- bootstrapPage(
  #titlePanel("Graphische Auswertung Baumkataster der Stadt Zürich"),
  #tags$b("Visualisierung der häufigsten Bäume nach Quartier"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput(inputId = "quartierInput",
                            label = "Quartier auswählen:",
                            choices = list,
                            selected = 1))
  
  
  # # Generieren des Seitennavigationsfensters  
  # sidebarLayout(
  #   sidebarPanel(
  #     selectInput(inputId = "quartierInput",
  #                 label = "Quartier auswählen:",
  #                 choices = list,
  #                 selected = 1)
  #   ),
  #   # Generien der Karte
  #   mainPanel(
  #     leafletOutput("map", height = 800, width = 800)
  #  )
  #)
)
  
# Definieren der Serverseite der Shiny App
server <- function(input, output) {
  
  # Erstellen der Filtervariable top10quartier
  top10quartier <- reactiveVal()
  
  observeEvent(input$quartierInput, {
    top10quartier(
      kataster %>% 
        filter(quartier == input$quartierInput) %>%
        add_count(baumnamedeu_kompakt) %>%
        filter(dense_rank(-n) <11)
    )
  
  })
  
  # Erstellen der Variable für die Einfärbung der Legenden und Bäume.
  farb <- reactiveVal()

  observeEvent(input$quartierInput, {
    farb(
    colorFactor(
      brewer.pal(10, "Set3"),
      domain = top10quartier()$baumnamedeu_kompakt)
    )

  })
  
  # Erstellen der Karte mit Leaflet, Hinzufügen der Marker, Legende und Masstab
  output$map <- renderLeaflet({
    
    leaflet(data = top10quartier()) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
  
      addCircleMarkers(lng = ~lng,
                       lat = ~lat,
                       popup = paste0("Baumnummer: ", top10quartier()$baumnummer, "<br>",
                                      "Baumename: ", top10quartier()$baumnamedeu_kompakt, "<br>",
                                      "Pflanzjahr: ", top10quartier()$pflanzjahr, "<br>",
                                      "Krondendurchmesser [m]: ", top10quartier()$kronendurchmesser, "<br>"),
                       radius = 4,
                       stroke = FALSE,
                       fillOpacity = 0.8,
                       color = ~farb()(top10quartier()$baumnamedeu_kompakt)) %>%
      addLegend("bottomright", 
               pal = farb(),
               values = top10quartier()$baumnamedeu_kompakt,
               title = paste("10 häufigste Baumarte in ", input$quartierInput),
               opacity = 1) %>%
      addScaleBar(
        position = c("topleft"),
        options = scaleBarOptions(maxWidth = 200,
                                  metric = TRUE,
                                  imperial = FALSE,
                                  updateWhenIdle = TRUE)
      )
  })
}

shinyApp(ui, server)

