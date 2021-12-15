# Masterarbeit Baumkataster ZHAW
# In dieser Shiny App werden die allergietreibenden Baumarten als Density-Map dargestellt.

# Michael Hilti (michael.hilti@gmail.com)
# 2021-12-14, 18.50 Uhr

# -*- coding: utf-8 -*-

# Laden der benötigten Packages
library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)

# Alle vorhandenen Objekte im Workspace löschen
rm(list = objects(pattern = ".*"))

# Festlegen des Input Files
input_file <- "kataster.csv"

# Einlesen kataster.csv, speichern als kataster
kataster <- read_delim(input_file, delim = ",", col_names = TRUE)

# Festlegen der auszuwählenden, allergietreibenden Bäume
list <- list("Birke" = "Birke", 
             "Buche" = "Buche", 
             "Eiche" = "Eiche", 
             "Erle" = "Erle", 
             "Esche" = "Esche", 
             "Espe" = "Espe", 
             "Hasel" = "Hasel",
             "Kastanie" = "Kastanie", 
             "Kiefer" = "Kiefer", 
             "Linde" = "Linde", 
             "Platane" = "Platane", 
             "Pappel" = "Pappel", 
             "Ulme" = "Ulme", 
             "Weide" = "Weide")

# Definieren der UI der Applikation
ui <- bootstrapPage(
  #titlePanel("Graphische Auswertung Baumkataster der Stadt Zürich"),
  #tags$b("Visualisierung der häufigsten Bäume nach Quartier"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput(inputId = "bauminput",
                            label = "Baumart auswählen:",
                            choices = list,
                            selected = 1))
  )


# Definieren der Serverseite der Shiny App
server <- function(input, output) {
  
  # Erstellen der Filtervariable top10quartier
  baumart <- reactiveVal()
  
  observeEvent(input$bauminput, {
    baumart(
      kataster %>% 
        filter(baumname_allergie == input$bauminput)
    )
  })
  
  
  # Erstellen der Karte mit Leaflet, Hinzufügen der Marker, Legende und Masstab
  output$map <- renderLeaflet({
    leaflet(data = baumart()) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addHeatmap(lng = ~lng,
                 lat = ~lat,
                 intensity = ~baumname_allergie,
                 max = 0.05,
                 minOpacity = 10,
                 # intensity =,
                 # s
                 radius = 13,
                 blur = 24) %>%
      addScaleBar(
        position = c("topleft"),
        options = scaleBarOptions(maxWidth = 200,
                                  metric = TRUE,
                                  imperial = FALSE,
                                  updateWhenIdle = TRUE)
      )
  })
}
# Ausführen der App
shinyApp(ui, server)
