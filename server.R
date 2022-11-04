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
    req(input$NORADCatalogNumber)
    req(input$classificationLevel)
    req(input$internationalDesignator)
    req(input$launchYear)
    req(input$launchNumber)
    req(input$launchPiece)
    req(input$date)
    req(input$time)
    req(input$elementNumber)
    req(input$inclination)
    req(input$ascension)
    req(input$eccentricity)
    req(input$perigeeArgument)
    req(input$meanAnomaly)
    req(input$meanMotion)
    req(input$meanMotionDerivative)
    req(input$meanMotionSecondDerivative)
    req(input$Bstar)
    req(input$ephemerisType)
    req(input$epochRevolutionNumber)
    req(input$objectName)
    
    sat = list(
      NORADCatalogNumber = input$NORADCatalogNumber,
      classificationLevel = input$classificationLevel,
      internationalDesignator = input$internationalDesignator,
      launchYear = input$launchYear,
      launchNumber = input$launchNumber,
      launchPiece = input$launchPiece,
      dateTime = paste(input$date, substring(input$time, 12, 20), sep= " "),
      elementNumber = input$elementNumber,
      inclination = input$inclination,
      ascension = input$ascension,
      eccentricity = input$eccentricity,
      perigeeArgument = input$perigeeArgument,
      meanAnomaly = input$meanAnomaly,
      meanMotion = input$meanMotion,
      meanMotionDerivative = input$meanMotionDerivative,
      meanMotionSecondDerivative = input$meanMotionSecondDerivative,
      Bstar = input$Bstar,
      ephemerisType = input$ephemerisType,
      epochRevolutionNumber = input$epochRevolutionNumber,
      objectName = input$objectName
    )
    
    geodetics_matrix <- calculateGeodeticMatrix(sat)
    geoMarkers <- calculateGeoMarkers(geodetics_matrix)
    geoPolylines <- calculateGeoPolylines(geoMarkers)
    
    renderMapOneSatellite(geoMarkers, geoPolylines)
  })
}