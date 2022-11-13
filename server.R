source('AuxiliarFunctions.R', local = TRUE)

server <- function(input, output) {
  output$select_satelite <- renderUI({
    selectInput("satelite", p("azul"), choices = getSatelites())
  })
  
  output$select_satelite2 <- renderUI({
    selectInput("satelite2", p("rojo"), choices = getSatelites())
  })
  
  output$firstMap <- renderLeaflet({
    req(input$satelite)
    req(input$satelite2)
    
    test_TLEs <-
      readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
    sat <- test_TLEs[[strtoi(input$satelite)]]
    sat2 <- test_TLEs[[strtoi(input$satelite2)]]
    
    geodetics_matrix <- calculateGeodeticMatrix(sat, sat2)
    geoMarkers <- calculateGeoMarkers(geodetics_matrix)
    geoPolylines <- calculateGeoPolylines(geoMarkers)
    
    renderMapTwoSatellites(geoMarkers, geoPolylines)
  })
  
  
  output$hpopOutput <- renderLeaflet({
    req(input$satelite)
    req(input$satelite2)
    test_TLEs <-
      readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
    sat <- test_TLEs[[strtoi(input$satelite)]]
    sat2 <- test_TLEs[[strtoi(input$satelite2)]]
    
    geodeticsMatrixHpop <- getGeodeticsMatrixHpop(sat, sat2)
    geoMarkers <- calculateGeoMarkers(geodeticsMatrixHpop)
    geoPolylines <- calculateGeoPolylines(geoMarkers)
    
    renderMapTwoSatellites(geoMarkers, geoPolylines)
  })
  
  output$myMap <- renderLeaflet({
    req(input$date)
    req(input$time)
    req(input$inclination)
    req(input$ascension)
    req(input$eccentricity)
    req(input$perigeeArgument)
    req(input$meanAnomaly)
    req(input$meanMotion)
    req(input$Bstar)
    
    sat = list(
      NORADCatalogNumber = "0005",
      classificationLevel = "unclassified",
      internationalDesignator = "2058-002B",
      launchYear = 2058,
      launchNumber = "002",
      launchPiece = "B",
      dateTime = paste(input$date, substring(input$time, 12, 20), sep= " "),
      elementNumber = 475,
      inclination = input$inclination,
      ascension = input$ascension,
      eccentricity = input$eccentricity,
      perigeeArgument = input$perigeeArgument,
      meanAnomaly = input$meanAnomaly,
      meanMotion = input$meanMotion,
      meanMotionDerivative = 4.6e-07,
      meanMotionSecondDerivative = 0,
      Bstar = input$Bstar,
      ephemerisType = "Distributed data (SGP4/SDP4)",
      epochRevolutionNumber = 41366,
      objectName = "Example"
    )
    
    geodetics_matrix <- calculateGeodeticMatrix(sat)
    geoMarkers <- calculateGeoMarkers(geodetics_matrix)
    geoPolylines <- calculateGeoPolylines(geoMarkers)
    
    renderMapOneSatellite(geoMarkers, geoPolylines)
  })
}