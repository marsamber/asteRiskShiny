library(shiny)
library(ggmap)
library(asteRisk)
library(asteRiskData)
library(shinycssloaders)
library(leaflet)
library(leaflet.extras2)
library(htmltools)
library(shinyTime)

source('ui.R', local=TRUE)
source('server.R')

shinyApp(ui = ui, server = server)
