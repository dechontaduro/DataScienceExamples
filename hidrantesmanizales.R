getData <- function(variableName, url, filename, ...){
  if(!exists(variableName)){
    if (!file.exists(filename)) { 
      download.file(url, filename)
    }
    read.csv(filename, ...)
  }
}

geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}

setwd("C:\\Users\\juanc\\Dropbox\\DataScience\\pruebas\\hidrantes")
#rm(dataHidra)
url <- "https://www.datos.gov.co/resource/ygcd-j498.csv"
dataHidra <- getData("dataHidra", url, "hidrantesmanizales.csv")

str(dataHidra)

geo <- apply (dataHidra, 1, 
       function(x) {
         geocodeAdddress(paste(x[2], ', Manizales, Colombia'))}) 

dataHidra$lat <- sapply(geo, function(x) x[2])
dataHidra$lon <- sapply(geo, function(x) x[1])
View(dataHidra)

library(leaflet)
library(dplyr)

leaflet(data = dataHidra) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~direccion, 
             clusterOptions = markerClusterOptions())