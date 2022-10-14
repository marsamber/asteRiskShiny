#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggmap)
library(googleway)
library(asteRisk)
library(asteRiskData)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("AsteRisk"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3("Elige dos satélites para mostrar:"),
      uiOutput("select_satelite"),
      uiOutput("select_satelite2"),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel("First method", plotOutput("mapOutput")),
      tabPanel("Second method (HPOP)", p("A implementar"))
    ))
  ))

# Define server logic required to draw a histogram

server <- function(input, output) {
  getSatelites <- function() {
    test_TLEs <-
      readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
    lista = list()
    for (i in 1:length(test_TLEs[])) {
      key <- test_TLEs[[i]]$objectName
      value <- i
      lista[[key]] <- value
    }
    return(lista)
  }
  
  output$select_satelite <- renderUI({
    selectInput("satelite", p("azul"), choices = getSatelites())
  })
  
  output$select_satelite2 <- renderUI({
    selectInput("satelite2", p("rojo"), choices = getSatelites())
  })
  
  geodeticMatrix <- function(sat) {
    targetTimes <- seq(0, 720, by = 10)
    
    results_position_matrix <-
      matrix(nrow = length(targetTimes), ncol = 3)
    results_velocity_matrix <-
      matrix(nrow = length(targetTimes), ncol = 3)
    
    for (i in 1:length(targetTimes)) {
      new_result <-
        sgdp4(
          n0 = sat$meanMotion * ((2 * pi) / (1440)),
          e0 = sat$eccentricity,
          i0 = sat$inclination * pi / 180,
          M0 = sat$meanAnomaly * pi / 180,
          omega0 = sat$perigeeArgument *
            pi / 180,
          OMEGA0 = sat$ascension * pi / 180,
          Bstar = sat$Bstar,
          initialDateTime = sat$dateTime,
          targetTime = targetTimes[i]
        )
      results_position_matrix[i, ] <- new_result[[1]]
      results_velocity_matrix[i, ] <- new_result[[2]]
    }
    last_sat_propagation <- new_result
    results_position_matrix = cbind(results_position_matrix, targetTimes)
    colnames(results_position_matrix) <- c("x", "y", "z", "time")
    new_dateTime <- "2006-06-25 12:33:43"
    
    ITRF_coordinates <-
      TEMEtoITRF(last_sat_propagation$position,
                 last_sat_propagation$velocity,
                 new_dateTime)
    
    # Let us now convert the previously calculated set of TEME coordinates to
    # geodetic latitude and longitude
    
    geodetic_matrix <-
      matrix(nrow = nrow(results_position_matrix),
             ncol = 3)
    
    for (i in 1:nrow(geodetic_matrix)) {
      new_dateTime <-
        as.character(as.POSIXct(sat$dateTime, tz = "UTC") + 60 *
                       targetTimes[i])
      new_geodetic <-
        TEMEtoLATLON(results_position_matrix[i, 1:3] * 1000, new_dateTime)
      geodetic_matrix[i, ] <- new_geodetic
    }
    
    colnames(geodetic_matrix) <-
      c("latitude", "longitude", "altitude")
    return(geodetic_matrix)
  }
  
  calculateGeodeticsMatrix <- function() {
    req(input$satelite)
    req(input$satelite2)
    
    test_TLEs <-
      readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
    sat <- test_TLEs[[strtoi(input$satelite)]]
    sat2 <- test_TLEs[[strtoi(input$satelite2)]]
    return(list(geodeticMatrix(sat), geodeticMatrix(sat2)))
  }
  
  output$mapOutput <- renderPlot({
    geodetics_matrix <- calculateGeodeticsMatrix()
    geoMatrix <- geodetics_matrix[1]
    geoMatrix2 <- geodetics_matrix[2]
    ggmap(get_map(c(
      left = -180,
      right = 180,
      bottom = -80,
      top = 80
    ))) + geom_segment(
      data = as.data.frame(geoMatrix),
      aes(
        x = longitude,
        y = latitude,
        xend = c(tail(longitude, n = -1), NA),
        yend = c(tail(latitude,
                      n = -1), NA)
      ),
      na.rm = TRUE,
      color = 'blue'
    ) + geom_point(
      data = as.data.frame(geoMatrix),
      aes(x = longitude, y = latitude),
      color = "blue",
      size = 0.3,
      alpha = 0.8
    ) + geom_segment(
      data = as.data.frame(geoMatrix2),
      aes(
        x = longitude,
        y = latitude,
        xend = c(tail(longitude, n = -1), NA),
        yend = c(tail(latitude,
                      n = -1), NA)
      ),
      na.rm = TRUE,
      color = "red"
    ) + geom_point(
      data = as.data.frame(geoMatrix2),
      aes(x = longitude, y = latitude),
      color = "red",
      size = 0.3,
      alpha = 0.8
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
