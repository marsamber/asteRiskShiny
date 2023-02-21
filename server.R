source('AuxiliarFunctions.R', local = TRUE)

server <- function(input, output, session) {
  test_TLEs <-
    readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
  
  observe({
    if (!is.null(input$satelite) && !is.null(input$satelite2)) {
      initialDateTimeSat = test_TLEs[[strtoi(input$satelite)]]$dateTime
      initialDateTimeSat2 = test_TLEs[[strtoi(input$satelite2)]]$dateTime
      
      initialDateSat = substr(initialDateTimeSat, 1, 10)
      initialDateSat2 = substr(initialDateTimeSat2, 1, 10)
      
      updateDateInput(
        session,
        "targetDateSat",
        value = as.Date(initialDateSat) + 1,
        min = initialDateSat
      )
      updateDateInput(
        session,
        "targetDateSat2",
        value = as.Date(initialDateSat2) + 1,
        min = initialDateSat2
      )
      updateTimeInput(session, "targetTimeSat", value = Sys.time())
      updateTimeInput(session, "targetTimeSat2", value = Sys.time())
    }
    updateDateInput(session, "initialDateSimulator", value = Sys.Date())
    updateTimeInput(session, "initialTimeSimulator", value = Sys.time())
    updateDateInput(
      session,
      "targetDateSimulator",
      value = Sys.Date(),
      min = input$initialDateSimulator
    )
    updateTimeInput(session, "targetTimeSimulator", value = Sys.time() + 1)
  })
  
  output$select_satelite <- renderUI({
    selectInput("satelite", p("Satélite 1"), choices = getSatelites())
  })
  
  output$select_satelite2 <- renderUI({
    selectInput("satelite2", p("Satélite 2"), choices = getSatelites())
  })
  
  output$initialDateSat <-
    renderText({
      req(input$satelite)
      test_TLEs[[strtoi(input$satelite)]]$dateTime
    })
  output$initialDateSat2 <-
    renderText({
      req(input$satelite2)
      test_TLEs[[strtoi(input$satelite2)]]$dateTime
    })
  
  output$firstMap <- renderLeaflet({
    req(input$satelite)
    req(input$satelite2)
    req(input$propagationTime)
    req(input$dimension)
    
    sats = list()
    
    if(!is.null(input$TLEFile)){
      file <- input$TLEFile
      sats <- readTLE(filename = file$datapath)
    } else {
      sat <- test_TLEs[[strtoi(input$satelite)]]
      sat <- c(sat, initialDateTime = sat$dateTime)
      sat2 <- test_TLEs[[strtoi(input$satelite2)]]
      sat2 <- c(sat2, initialDateTime = sat2$dateTime)
      
      sats[[1]] = sat
      sats[[2]] = sat2
    }
    
    
    if (input$propagationTime == "datetime") {
      req(input$targetDateSat)
      req(input$targetDateSat2)
      req(input$targetTimeSat)
      req(input$targetTimeSat2)
      
      targetTimeSat <- substring(input$targetTimeSat, 12, 19)
      targetTimeSat <-
        if (targetTimeSat == '')
          substring(sat$dateTime, 12, 19)
      else
        targetTimeSat
      
      targetTimeSat2 <- substring(input$targetTimeSat2, 12, 19)
      targetTimeSat2 <-
        if (targetTimeSat2 == '')
          substring(sat2$dateTime, 12, 19)
      else
        targetTimeSat2
      
      initialDate1 = sat$initialDateTime
      initialDate2 = sat2$initialDateTime
      
      
      targetDate1 = paste(input$targetDateSat,
                          targetTimeSat,
                          sep = " ")
      targetDate2 = paste(input$targetDateSat2,
                          targetTimeSat2,
                          sep = " ")
      
      
      geodetics_matrix <-
        calculateGeodeticMatrix(
          sat,
          sat2,
          targetDate1 = targetDate1,
          targetDate2 = targetDate2
        )
    } else if (input$propagationTime == "minutes") {
      req(input$propagationTimeSat)
      req(input$propagationTimeSat2)
      geodetics_matrix <-
        calculateGeodeticMatrix(
          sat,
          sat2,
          min1 = input$propagationTimeSat,
          min2 = input$propagationTimeSat2
        )
    }
    
    geoMarkers <- calculateGeoMarkers(geodetics_matrix)
    geoPolylines <- calculateGeoPolylines(geoMarkers)
    
    renderMapSatellites(geoMarkers, geoPolylines, input$dimension, list(sat$NORADcatalogNumber, sat2$NORADcatalogNumber))
  })
  
  
  output$hpopOutput <- renderLeaflet({
    req(input$satelite)
    req(input$satelite2)
    req(input$dimension)
    
    sat <- test_TLEs[[strtoi(input$satelite)]]
    sat2 <- test_TLEs[[strtoi(input$satelite2)]]
    
    geodeticsMatrixHpop <- getGeodeticsMatrixHpop(sat, sat2)
    geoMarkers <- calculateGeoMarkers(geodeticsMatrixHpop)
    geoPolylines <- calculateGeoPolylines(geoMarkers)
    
    renderMapSatellites(geoMarkers, geoPolylines, input$dimension, list(sat$NORADcatalogNumber, sat2$NORADcatalogNumber))
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
      initialDateTime = paste(
          input$initialDateSimulator,
          substring(input$initialTimeSimulator, 12, 20),
          sep = " "
        ),
      inclination = input$inclination,
      ascension = input$ascension,
      eccentricity = input$eccentricity,
      perigeeArgument = input$perigeeArgument,
      meanAnomaly = input$meanAnomaly,
      meanMotion = input$meanMotion,
      Bstar = input$Bstar
    )
    
    if (input$propagationTimeSimulator == "datetime") {
      req(input$targetDateSimulator)
      req(input$targetTimeSimulator)
      
      targetTimeSat <- substring(input$targetTimeSimulator, 12, 19)
      targetTimeSat <-
        if (targetTimeSat == '')
          substring(sat$initialDateTime, 12, 19)
      else
        targetTimeSat
      
      geodetics_matrix <-
        calculateGeodeticMatrix(
          sat,
          targetDate1 = paste(input$targetDateSimulator,
                              targetTimeSat,
                              sep = " ")
        )
    } else if (input$propagationTimeSimulator == "minutes") {
      req(input$propagationTimeSatSimulator)
      geodetics_matrix <-
        calculateGeodeticMatrix(
          sat,
          min1 = input$propagationTimeSatSimulator
        )
    }
    
    geoMarkers <- calculateGeoMarkers(geodetics_matrix)
    geoPolylines <- calculateGeoPolylines(geoMarkers)
    
    renderMapSatellites(geoMarkers, geoPolylines, input$dimension)
  })
}