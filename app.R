# Load Libraries
library(shiny)
library(tidyverse)
library(leaflet)
library(RColorBrewer)

# Alle vorhandenen Objekte im Workspace löschen
rm(list = objects(pattern = ".*"))

# Festlegen des In- und Output Files
input_file <- "shiny/baumkataster_leicht.csv"

# Einlesen baumkataster_clean_wgs84.csv, speichern als kataster
kataster <- read_delim(input_file, delim = ",", col_names = TRUE)

# quartier_name <- "Langstrasse"

# Filtern  des Katasters nach den zehn meistvorkommenden Bäumen in Hard
# top10 <- kataster %>%
#    filter(quartier == "Langstrasse") %>%
#    add_count(baumnamedeu_kompakt) %>%
#    filter(dense_rank(-n) < 11)


list <- as.data.frame(c("Höngg" = "Höngg",
          "Langstrasse" = "Langstrasse",
          "Hard" = "Hard"))

# Aufbauen der Farbpalette für 10 Farben
# mypalette <- brewer.pal(10, "Paired")
# pal <- colorFactor(mypalette, domain = top10$baumnamedeu_kompakt)

ui <- fluidPage(
  titlePanel("Auswertung der Top 10 Bäume"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "quartierInput", label = "Quartier:",
                  choices = list,
                  selected = 2)
    ),

    mainPanel(
      leafletOutput("map")
    )
  )
)
  

server <- function(input, output) {
  input <- reactive({input$quartierInput})
  
  top10quartier <- function(input) {
    reactive({
      kataster %>% filter(quartier == input) %>%
                                    add_count(baumnamedeu_kompakt) %>%
                                    filter(dense_rank(-n) <11)
    })
  }
  
  # mypalette <- brewer.pal(10, "Paired")
  # pal <- reactive(colorFactor(mypalette, domain = top10quartier$baumnamedeu_kompakt))

  output$map <- renderLeaflet({
    leaflet(data = top10quartier()) %>% addProviderTiles(providers$Stamen.TonerLite,
                                               options = providerTileOptions(noWrap = TRUE)
                                               ) %>%
      addCircleMarkers(lng = ~lng,
                       lat = ~lat,
                       popup = ~as.character(top10quartier()$baumnamedeu_kompakt),
                       radius = 4,
                       stroke = FALSE,
                       fillOpacity = 0.6)
  })#,
                       # color = colorFactor(brewer.pal(10, "Paired"), domain = top10quartier$baumnamedeu_kompakt))  # %>%
      # addLegend("bottomright",
      #           pal = pal,
      #           values = top10quartier$baumnamedeu_kompakt,
      #           title = paste("Top 10 Baumarten ", input$quartierInput),
      #           opacity = 1)
  
}

shinyApp(ui, server)

