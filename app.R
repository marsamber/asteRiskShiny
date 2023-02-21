library(shiny)
library(asteRisk)
library(asteRiskData)
library(shinycssloaders)
library(leaflet)
library(leaflet.extras2)
library(htmltools)
library(shinyTime)
library(randomcoloR)

source('ui.R', local=TRUE)
source('server.R')

shinyApp(ui = ui, server = server)
