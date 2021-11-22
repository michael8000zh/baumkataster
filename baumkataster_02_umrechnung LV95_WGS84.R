# Masterarbeit Baumkataster ZHAW
# Michael Hilti (michael.hilti@gmail.com)
# 2021-11-22, 09.22 Uhr

# Festlegen des Working directories
setwd("/Users/michael/Documents/Data Science/Masterarbeit_ZHAW/")

# laden der verwendeten Libraries
library(tidyverse)

# Einlesen der Daten aus baumkataster_01_datacleaning.r
kataster <- read_delim("data_01.csv", delim = ",", col_names = TRUE)
kataster <- head(kataster, 50)

# LV95: POINT (y = 2686027.5 x = 1251531.4)
# WGS84: long : 8.578453446, lat: 47.408977778
CH.to.WGS.lat <- function (y, x){
  
  ## Converts military to civil and  to unit = 1000km
  ## Auxiliary values (% Bern)
  y_aux <- (y - 2600000)/1000000
  x_aux <- (x - 1200000)/1000000
  
  ## Process lat
  lat <- {16.9023892 +
      3.238272 * x_aux -
      0.270978 * (y_aux^2) -
      0.002528 * (x_aux^2) -
      0.0447   * (y_aux^2) * x_aux -
      0.0140   * (x_aux^3)}
  
  ## Unit 10000" to 1 " and converts seconds to degrees (dec)
  lat <- lat * 100/36
  
  return(lat)  
}

kataster$lat <- sapply(kataster$che_y, CH.to.WGS.lat, kataster$che_x)

# Erfolgskontrolle
view(kataster)


## Convert CH y/x to WGS long
CH.to.WGS.lng <- function (y, x){
  
  ## Converts military to civil and  to unit = 1000km
  ## Auxiliary values (% Bern)
  y_aux <- (y - 2600000)/1000000
  x_aux <- (x - 1200000)/1000000
  
  ## Process long
  lng <- {2.6779094 +
      4.728982 * y_aux +
      0.791484 * y_aux * x_aux +
      0.1306   * y_aux * (x_aux^2) -
      0.0436   * (y_aux^3)}
  
  ## Unit 10000" to 1 " and converts seconds to degrees (dec)
  lng <- lng * 100/36
  
  return(lng)
}

kataster$lng <- sapply(kataster$che_y, CH.to.WGS.lng, kataster$che_x)

# Erfolgskontrolle
view(kataster)

# Abspeichern CSV 
write.csv(kataster, "/Users/michael/Documents/Data Science/Masterarbeit_ZHAW/data_02.csv", row.names = TRUE)
