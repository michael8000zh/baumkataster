# Masterarbeit Baumkataster ZHAW
# In diesem Scrip werden das Basisgerüst des ShinyApp-Dashboards gebaut

# Michael Hilti (michael.hilti@gmail.com)
# 2021-12-21, 23.30 Uhr

# Installiere benötigte Packages
# install.packages("shinydashboard")
# install.packages("shiny")

# Benötigte Libraries laden
library(shinydashboard)
library(shiny)
library(graphics)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(leaflet)
library(leaflet.extras)

# Festlegen des Input Files Kataster
input_file <- "kataster.csv"

# Einlesen kataster.csv, speichern als kataster
kataster <- read_delim(input_file, delim = ",", col_names = TRUE, show_col_types = FALSE)

# Festlegen des Input Files Flaeche
input_file <- "flaeche.csv"

# Einlesen kataster.csv, speichern als kataster
flaeche <- read_delim(input_file, delim = ",", col_names = TRUE, show_col_types = FALSE)

# Festlegen der Quartiere der Stadt Zürich
quartierliste <- c("Affoltern", "Albisrieden", "Alt-Wiedikon", "Altstetten", "ausserhalb Stadtgebiet", "City",
                   "Enge", "Escher Wyss", "Fluntern", "Friesenberg", "Gewerbeschule", "Hard", "Hirslanden",
                   "Hirzenbach", "Hochschulen", "Höngg", "Hottingen", "Langstrasse", "Leimbach", "Lindenhof",
                   "Mühlebach", "Oberstrass", "Oerlikon", "Rathaus", "Saatlen", "Schwamendingen-Mitte",
                   "Seebach", "Seefeld", "Sihlfeld", "Unterstrasse", "Weinegg", "Werd", "Wipkingen",
                   "Witikon", "Wollishofen")

# Festlegen der allergietreibenden Bäume
baumliste <- c("Birke", "Buche", "Eiche", "Erle", "Esche", "Espe", "Hasel", "Kastanie", "Kiefer", "Linde",
               "Platane", "Pappel", "Ulme", "Weide")

# Definieren der UI Seite der App 
ui <- dashboardPage(
  dashboardHeader(title = "Baumkataster"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Einführung", tabName = "einfuehrung", icon = icon("fas fa-file-alt")),
      menuItem("Verteilung Baumsorten", tabName = "verteilung_normal", icon = icon("th")),
      menuItem("Verteilung Allergiebaumsorten", tabName = "verteilung_allergie", icon = icon("th")),
      menuItem("Heatmap Allergiebaumsorten", tabName = "allergie", icon = icon("th")),
      menuItem("Charts", tabName = "charts", icon = icon("bar-chart-o"), startExpanded = TRUE,
        menuSubItem("Verteilung Baumsorten", tabName = "chart_verteilung"),
        menuSubItem("Verteilung Allergiebaumsorten", tabName = "chart_allergie_verteilung"),
        menuSubItem("Einzelne Allergiebaumsorten", tabName = "chart_allergie_einzeln"),
        menuSubItem("Allergiebaumdichte", tabName = "charte_dichte")
      )
    )
  ),

  
  ## Body Content
  dashboardBody(
    tabItems(
      #tab: Einführung
      tabItem(tabName = "einfuehrung",
              h4("Graphische Auswertung des Baumkatasters der Stadt Zürich"),
              h4("Beschreibung der Arbeit, Link auf Github Repository, Link auf Daten der Stadt")
      ),
      #tab: Widget Verteilung zehn häufigste normale Bäume pro Quartier
      tabItem(tabName = "verteilung_normal",
              h4("Geographische Verteilung der zehn häufigsten Baumsorten pro Quartier"),
              selectInput(inputId = "quartierInput",
                          label = "Quartier auswählen:",
                          choices = quartierliste,
                          selected = 1),
              bootstrapPage(
                tags$style(type = "text/css", "#map {height: calc(100vh - 200px) !important;}"),
                leafletOutput("map_verteilung", height = 800)
              )
      ),

      #tab: Widget Verteilung zehn häufigste allergietreibende Bäume pro Quartier
      tabItem(tabName = "verteilung_allergie",
              h4("Geographische Verteilung der zehn häufigsten, allergietreibende Baumsorten pro Quartier"),
              selectInput(inputId = "quartierInput_allergie",
                          label = "Quartier auswählen:",
                          choices = quartierliste,
                          selected = 1),
              bootstrapPage(
                tags$style(type = "text/css", "#map {height: calc(100vh - 200px) !important;}"),
                leafletOutput("map_verteilung_allergie", height = 800)
              )
      ),
      
                    
      #tab: Widget Allergie
      tabItem(tabName = "allergie",
              h4("Heat-Map der allergietreibenden Baumsorten über ganze Stadt"),
              selectInput(inputId = "bauminput",
                          label = "Baumart auswählen:",
                          choices = baumliste,
                          selected = 1),
              bootstrapPage(
                tags$style(type = "text/css", "#map {height: calc(100vh - 200px) !important;}"),
                leafletOutput("map_allergie", height = 800)
              )
      ),
      
      #tab: Widget Charts Verteilung n häufigste Bäume
      tabItem(tabName = "chart_verteilung",
              h4("Verteilung der n-häufigsten Baumsorten pro Quartier"),
              selectInput(inputId = "chart_quartier",
                          label = "Quartier auswählen:",
                          choices = quartierliste,
                          selected = 1),
              numericInput(inputId = "chart_anz_baeume",
                           label = "Anzahl Bäume:",
                           min = 1, max = 20, value = 10),
              bootstrapPage(
                tags$style(type = "text/css", "#map {height: calc(100vh - 200px) !important;}"),
                plotOutput("plot1")
              )
      ),
      
      #tab: Widget Charts Verteilung Allergiebäume
      tabItem(tabName = "chart_allergie_verteilung",
              h4("Verteilung der Allergiebaumsorten nach Quartier"),
              selectInput(inputId = "chart_allergie_verteilung",
                          label = "Quartier auswählen:",
                          choices = quartierliste,
                          selected = 1),
              bootstrapPage(
                tags$style(type = "text/css", "#map {height: calc(100vh - 200px) !important;}"),
                plotOutput("plot2")
              )
      ),
      
      #tab: Widget Charts Verteilung einzelner Allergiebäume
      tabItem(tabName = "chart_allergie_einzeln",
              h4("Verteilung einzelner Allergiebaumsorten pro Quartier"),
              selectInput(inputId = "chart_allergie_einzeln",
                          label = "Sorte auswählen:",
                          choices = baumliste,
                          selected = 1),
              bootstrapPage(
                tags$style(type = "text/css", "#map {height: calc(100vh - 200px) !important;}"),
                plotOutput("plot3")
              )
      ),
      
      #tab: Widget Charts Dichte der Allergiebäume nach Quartiergrösse
      tabItem(tabName = "charte_dichte",
              h4("Verteilung Allergiebaumdichte im Vergleich zur Quartierfläche"),
              bootstrapPage(
                tags$style(type = "text/css", "#map {height: calc(100vh - 200px) !important;}"),
                plotOutput("plot4")
              )
              
      )
    )
  )
)


# Definieren der Serverseite der App

server <- function(input, output) {

### Erstellen der serverseitigen Funktionen für Widget Verteilung zehn häufigste normale Bäume pro Quartier   
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

# Erstellen der Karte für das Widget Verteilung
output$map_verteilung <- renderLeaflet({
  
  leaflet(data = top10quartier(), options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE),
                     group = "Basiskarte grau") %>%
    addWMSTiles(
      baseUrl = "https://www.ogd.stadt-zuerich.ch/wms/geoportal/Basiskarte_Zuerich_Raster_Grau?SERVICE=WMS&REQUEST=GetCapabilities&VERSION=1.3.0",
      layers = "Basiskarte Zürich Raster Grau",
      options = WMSTileOptions(format = "image/png", transparent = TRUE),
      group = "Basiskarte Stadt Zürich") %>%
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
              title = paste("Häufigste Baumsorten in ", input$quartierInput),
              opacity = 1) %>%
    addLayersControl(
      baseGroups = c("Basiskarte grau", "Basiskarte Stadt Zürich"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    addScaleBar(
      position = c("bottomleft"),
      options = scaleBarOptions(maxWidth = 200,
                                metric = TRUE,
                                imperial = FALSE,
                                updateWhenIdle = TRUE)
    )
})

### Erstellen der serverseitigen Funktionen für Widget Verteilung der zehn häufigsten
# Allergietreiber pro Quartier   
# Erstellen der Filtervariable top10quartier
top10quartier_allergie <- reactiveVal()

observeEvent(input$quartierInput_allergie, {
  top10quartier_allergie(
    kataster %>% 
      filter(baumname_allergie %in% baumliste) %>%
      filter(quartier == input$quartierInput_allergie) %>%
      add_count(baumname_allergie) %>%
      filter(dense_rank(-n) <11)
  )
})  

# Erstellen der Variable für die Einfärbung der Legenden und Bäume.
farb_allergie <- reactiveVal()

observeEvent(input$quartierInput_allergie, {
  farb_allergie(
    colorFactor(
      brewer.pal(10, "Set3"),
      domain = top10quartier_allergie()$baumname_allergie)
  )
})

# Erstellen der Karte für das Widget Verteilung
output$map_verteilung_allergie <- renderLeaflet({
  
  leaflet(data = top10quartier_allergie(), options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE),
                     group = "Basiskarte grau") %>%
    addWMSTiles(
      baseUrl = "https://www.ogd.stadt-zuerich.ch/wms/geoportal/Basiskarte_Zuerich_Raster_Grau?SERVICE=WMS&REQUEST=GetCapabilities&VERSION=1.3.0",
      layers = "Basiskarte Zürich Raster Grau",
      options = WMSTileOptions(format = "image/png", transparent = TRUE),
      group = "Basiskarte Stadt Zürich") %>%
    addWMSTiles(
      baseUrl = "https://www.ogd.stadt-zuerich.ch/wms/geoportal/Orthofoto_2020_Kanton_Zuerich___Sommer___inkl._Infrarot___1m?SERVICE=WMS&REQUEST=GetCapabilities&VERSION=1.3.0",
      layers = "Orthofoto 2020 Kanton Zürich - Sommer - inkl. Infrarot - 1m",
      options = WMSTileOptions(format = "image/png", transparent = TRUE),
      group = "Orthofoto") %>%
    addCircleMarkers(lng = ~lng,
                     lat = ~lat,
                     popup = paste0("Baumnummer: ", top10quartier_allergie()$baumnummer, "<br>",
                                    "Baumename: ", top10quartier_allergie()$baumname_allergie, "<br>",
                                    "Pflanzjahr: ", top10quartier_allergie()$pflanzjahr, "<br>",
                                    "Krondendurchmesser [m]: ", top10quartier_allergie()$kronendurchmesser, "<br>"),
                     radius = 4,
                     stroke = FALSE,
                     fillOpacity = 0.8,
                     color = ~farb_allergie()(top10quartier_allergie()$baumname_allergie)) %>%
    addLegend("bottomright", 
              pal = farb_allergie(),
              values = top10quartier_allergie()$baumname_allergie,
              title = paste("Häufigste Allergietreiber in ", input$quartierInput_allergie),
              opacity = 1) %>%
    addLayersControl(
      baseGroups = c("Basiskarte grau", "Basiskarte Stadt Zürich", "Orthofoto"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    addScaleBar(
      position = c("bottomleft"),
      options = scaleBarOptions(maxWidth = 200,
                                metric = TRUE,
                                imperial = FALSE,
                                updateWhenIdle = TRUE)
    )
})


### Erstellen der serverseitigen Funktionen für Widget Allergie   
# Erstellen der Filtervariable top10quartier
baumart <- reactiveVal()

observeEvent(input$bauminput, {
  baumart(
    kataster %>% 
      filter(baumname_allergie == input$bauminput)
  )
})

# Erstellen der Karte mit Leaflet, Hinzufügen der Marker und Massstab
output$map_allergie <- renderLeaflet({
  leaflet(data = baumart(), options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE),
                     group = "Basiskarte grau") %>%
    addWMSTiles(
      baseUrl = "https://www.ogd.stadt-zuerich.ch/wms/geoportal/Basiskarte_Zuerich_Raster_Grau?SERVICE=WMS&REQUEST=GetCapabilities&VERSION=1.3.0",
      layers = "Basiskarte Zürich Raster Grau",
      options = WMSTileOptions(format = "image/png", transparent = TRUE),
      group = "Basiskarte Stadt Zürich") %>%
    addHeatmap(lng = ~lng,
               lat = ~lat,
               intensity = ~baumname_allergie,
               max = 0.05,
               minOpacity = 10,
               radius = 13,
               blur = 24) %>%
    addLayersControl(
      baseGroups = c("Basiskarte grau", "Basiskarte Stadt Zürich"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    addScaleBar(
      position = c("bottomleft"),
      options = scaleBarOptions(maxWidth = 200,
                                metric = TRUE,
                                imperial = FALSE,
                                updateWhenIdle = TRUE)
    )
})

### Erstellen der serverseitigen Funktion des Widgets Charts: Verteilung der n-häufigsten Bäume nach Quartier
# Aufbau Hilfsfilter Anzahlbäume nach Quartier
total_pro_quartier <- reactiveVal()

observeEvent(input$chart_quartier, {
  total_pro_quartier(
    kataster %>%
      filter(quartier == input$chart_quartier) %>%
      summarize(n = n())
  )
  
})

# Aufbau Filter n häufigste Sorten pro Quartier  
anzahl_nach_quartier <- reactiveVal() 

observeEvent(input$chart_quartier, {
  anzahl_nach_quartier(
    kataster %>%
      filter(quartier == input$chart_quartier) %>%
      add_count(baumnamedeu_kompakt) %>%
      filter(dense_rank(-n) <= input$chart_anz_baeume) %>%
      group_by(baumnamedeu_kompakt) %>%
      summarize(n = n())
  )
})

# Ausgabe des Plots 1: n-häufigste Sorten pro Quartier  
output$plot1 <- renderPlot({
  ggplot(data = anzahl_nach_quartier(), aes(y = reorder(baumnamedeu_kompakt, n), x = n)) +
    geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
    geom_text(aes(label = n), hjust = 1.6, size = 3.0, color = "white") +
    theme_minimal() +
    theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
    labs(title = paste("Verteilung der", input$chart_anz_baeume, "häufigsten Bäume im Quartier", input$chart_quartier),
         subtitle = paste("Auswertung über", sum(anzahl_nach_quartier()$n), "von", total_pro_quartier(), "im Quartier stehenden Bäumen"))
})

### Erstellen der serverseitigen Funktion des Widgets Charts: Verteilung der allergietreibenden Bäume nach Quartier
# Aufbau Datenframe der Allergiebäume, durchgezählt
allergiebaeume_nach_quartier <- reactiveVal()

observeEvent(input$chart_allergie_verteilung, {
  allergiebaeume_nach_quartier(
    kataster %>%
      filter(baumname_allergie %in% baumliste) %>%
      filter(quartier == input$chart_allergie_verteilung) %>%
      add_count(baumname_allergie) %>%
      group_by(baumname_allergie) %>%
      summarise(n = n())
      
    )
  
})

# Hilfsvariable aufbauen: Anzahl Bäume total im Quartier
totalbaeume_nach_quartier <- reactiveVal()

observeEvent(input$chart_allergie_verteilung, {
  totalbaeume_nach_quartier(
    kataster %>%
      filter(quartier == input$chart_allergie_verteilung) %>%
      summarize(n = n())
  )
})
  
# Ausgabe des Plots 2: Anzahl Allergiebäume pro Quartier  
output$plot2 <- renderPlot({
  ggplot(data = allergiebaeume_nach_quartier(), aes(y = reorder(baumname_allergie, n), x = n)) +
    geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
    geom_text(aes(label = n), hjust = 1.6, size = 3.0, color = "white") +
    theme_minimal() + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
    labs(title = paste("Verteilung der Allergiebaumsorten im Quartier", input$chart_allergie_verteilung),
         subtitle = paste("Auswertung über", sum(allergiebaeume_nach_quartier()$n), "von", totalbaeume_nach_quartier(), "im Quartier stehenden Bäumen"))
})

### Erstellen der serverseitigen Funktion des Widgets Charts: Verteilung einzelner allergietreibenden Sorten nach Quartier
# Aufbau Datenframe der Allergiebäume, durchgezählt
allergiebaum_pro_quartier <- reactiveVal()

observeEvent(input$chart_allergie_einzeln, {
  allergiebaum_pro_quartier(
    kataster %>%
      filter(baumname_allergie == input$chart_allergie_einzeln) %>%
      add_count(baumname_allergie) %>%
      group_by(quartier) %>%
      summarise(n = n())
    
  )
  
})

# Hilfsvariable aufbauen: Anzahl Bäume total im Quartier
totalallergiebaum <- reactiveVal()

observeEvent(input$chart_allergie_einzeln, {
  totalallergiebaum(
    kataster %>%
      filter(baumname_allergie == input$chart_allergie_einzeln) %>%
      summarize(n = n())
  )
})

# Ausgabe des Plots 3: Anzahl Allergiebäume pro Quartier  
output$plot3 <- renderPlot({
  ggplot(data = allergiebaum_pro_quartier(), aes(y = reorder(quartier, n), x = n)) +
    geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
    geom_text(aes(label = n), hjust = 1.6, size = 3.0, color = "white") +
    theme_minimal() + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
    labs(title = paste("Verteilung der Bäume der Sorte", input$chart_allergie_einzeln, "pro Quartier"),
         subtitle = paste("Auswertung über", sum(allergiebaum_pro_quartier()$n), "Bäume"))
})

### Erstellen der serverseitigen Funktion des Widgets Charts: Berechnung und Darstellung der Allergiebaumdichte
# Erstellen Hilfsvariable alle_allergiebaeume_pro_quartier 
alle_allergiebaeume_pro_quartier <- kataster %>%
  filter(baumname_allergie %in% baumliste) %>%
  group_by(quartier) %>%
  summarize(anz_allergiebaeume = n())

# Zusammenführen von flaeche und alle alle_allergiebaeume_pro_quartier
kombiniert <- merge(alle_allergiebaeume_pro_quartier, flaeche, by = 'quartier')

# Berechnen des Allergiebaumkoeffizient: Anzahl Allergiebäume pro Quadratmeterfläche des Quartiers)
allergiebaumkoeff_nach_quartier <- kombiniert %>%
  mutate(allergieb_pro_m = round(anz_allergiebaeume / area_sqm * 1000, digits = 3))

# Ausgabe des Plots 4: Allergiebaumdichte pro Quartier
output$plot4 <- renderPlot({
  ggplot(data = allergiebaumkoeff_nach_quartier, aes(y = reorder(quartier, allergieb_pro_m), x = allergieb_pro_m)) +
    geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
    geom_text(aes(label = allergieb_pro_m), hjust = 1.6, size = 3.0, color = "white") +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    labs(title = "Verteilung der Allergiebaumdichte pro Quadratmeter Quartierfläche",
         subtitle = paste("Auswertung über", sum(alle_allergiebaeume_pro_quartier$anz_allergiebaeume), "von", dim(kataster), "Bäumen"))
})

}

# Ausführen der App
shinyApp(ui, server)

