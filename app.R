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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("AsteRisk"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            uiOutput("select_satelite"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("mapOutput")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  getSatelites <- function(){
    test_TLEs <- readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
    lista = list()
    for(i in 1:length(test_TLEs[])){
      key <- test_TLEs[[i]]$objectName
      value <- i
      lista[[key]] <- value
    }
    return(lista)
  }
  
  output$select_satelite <- renderUI({
    selectInput("satelite", h4("Elige un satélite que mostrar"), choices=getSatelites() )
  })
  
  calculateGeodeticMatrix <- function(){
    req(input$satelite)
    test_TLEs <- readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
    molniya <- test_TLEs[[strtoi(input$satelite)]]
    targetTimes <- seq(0, 720, by = 10)
    
    results_position_matrix <- matrix(nrow = length(targetTimes), ncol = 3)
    results_velocity_matrix <- matrix(nrow = length(targetTimes), ncol = 3)
    
    for (i in 1:length(targetTimes)) {
      new_result <- sgdp4(n0 = molniya$meanMotion * ((2 * pi)/(1440)), e0 = molniya$eccentricity,
                          i0 = molniya$inclination * pi/180, M0 = molniya$meanAnomaly * pi/180, omega0 = molniya$perigeeArgument *
                            pi/180, OMEGA0 = molniya$ascension * pi/180, Bstar = molniya$Bstar, initialDateTime = molniya$dateTime,
                          targetTime = targetTimes[i])
      results_position_matrix[i, ] <- new_result[[1]]
      results_velocity_matrix[i, ] <- new_result[[2]]
    }
    last_molniya_propagation <- new_result
    results_position_matrix = cbind(results_position_matrix, targetTimes)
    colnames(results_position_matrix) <- c("x", "y", "z", "time")
    new_dateTime <- "2006-06-25 12:33:43"
    
    ITRF_coordinates <- TEMEtoITRF(last_molniya_propagation$position, last_molniya_propagation$velocity,
                                   new_dateTime)
    
    # Let us now convert the previously calculated set of TEME coordinates to
    # geodetic latitude and longitude
    
    geodetic_matrix <- matrix(nrow = nrow(results_position_matrix), ncol = 3)
    
    for (i in 1:nrow(geodetic_matrix)) {
      new_dateTime <- as.character(as.POSIXct(molniya$dateTime, tz = "UTC") + 60 *
                                     targetTimes[i])
      new_geodetic <- TEMEtoLATLON(results_position_matrix[i, 1:3] * 1000, new_dateTime)
      geodetic_matrix[i, ] <- new_geodetic
    }
    
    colnames(geodetic_matrix) <- c("latitude", "longitude", "altitude")
    return(geodetic_matrix)
  }
  
  output$mapOutput <-renderPlot({
    geodetic_matrix <- calculateGeodeticMatrix()
    ggmap(get_map(c(left = -180, right = 180, bottom = -80, top = 80))) + geom_segment(data = as.data.frame(geodetic_matrix),
                                                                                       aes(x = longitude, y = latitude, xend = c(tail(longitude, n = -1), NA), yend = c(tail(latitude,
                                                                                                                                                                             n = -1), NA)), na.rm = TRUE) + geom_point(data = as.data.frame(geodetic_matrix),
                                                                                                                                                                                                                       aes(x = longitude, y = latitude), color = "blue", size = 0.3, alpha = 0.8)
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
