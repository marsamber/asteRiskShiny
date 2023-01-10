source('AuxiliarFunctions.R', local = TRUE)

server <- function(input, output) {
  output$select_satelite <- renderUI({
    selectInput("satelite", p("azul"), choices = getSatelites())
  })
  
  output$select_satelite2 <- renderUI({
    selectInput("satelite2", p("rojo"), choices = getSatelites())
  })
  
  output$initialDateSat <- renderText({test_TLEs[[strtoi(input$satelite)]]$dateTime})
  output$initialDateSat2 <- renderText({test_TLEs[[strtoi(input$satelite2)]]$dateTime})
  
  output$firstMap <- renderLeaflet({
    req(input$satelite)
    req(input$satelite2)
    req(input$propagationTime)
    req(input$dimension)
    
    test_TLEs <-
      readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
    sat <- test_TLEs[[strtoi(input$satelite)]]
    sat <- c(sat, initialDateTime = sat$dateTime)
    sat2 <- test_TLEs[[strtoi(input$satelite2)]]
    sat2 <- c(sat2, initialDateTime = sat2$dateTime)
    
    geodetics_matrix <- calculateGeodeticMatrix(sat, sat2)
    geoMarkers <- calculateGeoMarkers(geodetics_matrix)
    geoPolylines <- calculateGeoPolylines(geoMarkers)
    
    renderMapTwoSatellites(geoMarkers, geoPolylines, input$dimension)
  })
  
  
  output$hpopOutput <- renderLeaflet({
    req(input$satelite)
    req(input$satelite2)
    req(input$dimension)
    
    test_TLEs <-
      readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
    sat <- test_TLEs[[strtoi(input$satelite)]]
    sat2 <- test_TLEs[[strtoi(input$satelite2)]]

    geodeticsMatrixHpop <- getGeodeticsMatrixHpop(sat, sat2)
    geoMarkers <- calculateGeoMarkers(geodeticsMatrixHpop)
    geoPolylines <- calculateGeoPolylines(geoMarkers)
    
    renderMapTwoSatellites(geoMarkers, geoPolylines, input$dimension)
  })
  
  output$myMap <- renderLeaflet({
    req(input$inclination)
    req(input$ascension)
    req(input$eccentricity)
    req(input$perigeeArgument)
    req(input$meanAnomaly)
    req(input$meanMotion)
    req(input$Bstar)
    req(input$propagationTimeSimulator)
    
    req(input$dimension)
    
    sat = list(
      initialDateTime = if(input$propagationTimeSimulator == "datetime") paste(input$initialDateSimulator, substring(input$initialTimeSimulator, 12, 20), sep= " ") else NULL,
      inclination = input$inclination,
      ascension = input$ascension,
      eccentricity = input$eccentricity,
      perigeeArgument = input$perigeeArgument,
      meanAnomaly = input$meanAnomaly,
      meanMotion = input$meanMotion,
      Bstar = input$Bstar
    )
    
    geodetics_matrix <- calculateGeodeticMatrix(sat)
    
    geoMarkers <- calculateGeoMarkers(geodetics_matrix)
    geoPolylines <- calculateGeoPolylines(geoMarkers)
    
    renderMapOneSatellite(geoMarkers, geoPolylines, input$dimension)
  })
}