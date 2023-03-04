source('AuxiliarFunctions.R', local = TRUE)

server <- function(input, output, session) {
  test_TLEs <-
    readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
  
  getMoreRecentDate <- function(){
    req(input$TLEFile)
    
    file <- input$TLEFile
    sats <- readTLE(filename = file$datapath)
    dates = NULL
    
    if (is.null(names(sats))){
      for(i in 1:length(sats)){
        date <- sats[[i]]$dateTime
        date = as.POSIXct(date, tz="UTC")
        dates = c(dates, date)
      }
      
      dates = sort(dates, decreasing = TRUE)
      dateString = format(as.POSIXct(dates[1], origin="1970-01-01", tz="UTC"))
    } else {
      dateString = sats$dateTime
    }
    
    return(dateString)
  }
  
  observe({
    if (!is.null(input$satelite)) {
      if(is.null(input$TLEFile)) {
        initialDateTimeSat = test_TLEs[[strtoi(input$satelite)]]$dateTime
        initialDateSat = substr(initialDateTimeSat, 1, 10)
        
        updateDateInput(
          session,
          "targetDateSat",
          value = as.Date(initialDateSat) + 1,
          min = initialDateSat
        )
        updateTimeInput(session, "targetTimeSat", value = Sys.time())
      } else {
        initialDateTimeSat = getMoreRecentDate()
        initialDateSat = substr(initialDateTimeSat, 1, 10)
        updateDateInput(
          session,
          "targetDateSat",
          value = as.Date(initialDateSat) + 1,
          min = initialDateSat
        )
        updateTimeInput(session, "targetTimeSat", value = Sys.time())
      }
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
    selectInput("satelite", p("Satélite"), choices = getSatelites())
  })
  
  output$initialDateSat <-
    renderText({
      req(input$satelite)
      if(!is.null(input$TLEFile)){
        getMoreRecentDate()
      } else {
        test_TLEs[[strtoi(input$satelite)]]$dateTime
      }
    })
  
  output$firstMap <- renderLeaflet({
    req(input$satelite)
    req(input$propagationTime)
    req(input$dimension)
    
    sats = list()
    
    if(!is.null(input$TLEFile)){
      file <- input$TLEFile
      TLESats <- readTLE(filename = file$datapath)
      if(!is.null(names(TLESats))){
        sats[[1]] = TLESats
      } else {
        sats <- TLESats 
      }
      for(i in 1:length(sats)){
        sats[[i]] = c(sats[[i]], initialDateTime = sats[[i]]$dateTime)
      }
    } else {
      sat <- test_TLEs[[strtoi(input$satelite)]]
      sat <- c(sat, initialDateTime = sat$dateTime)
      sats[[1]] = sat
    }
    
    if (input$propagationTime == "datetime") {
      req(input$targetDateSat)
      req(input$targetTimeSat)
      
      targetTimeSat <- substring(input$targetTimeSat, 12, 19)
      targetTimeSat <-
        if (targetTimeSat == '')
          substring(sats[[1]]$dateTime, 12, 19)
      else
        targetTimeSat
      
      targetDate = paste(input$targetDateSat,
                          targetTimeSat,
                          sep = " ")
      geodetics_matrix <-
        calculateGeodeticMatrix(
          sats,
          targetDate = targetDate
        )
    } else if (input$propagationTime == "minutes") {
      req(input$propagationTimeSat)
      geodetics_matrix <-
        calculateGeodeticMatrix(
          sats,
          min = input$propagationTimeSat
        )
    }
    
    geoMarkers <- calculateGeoMarkers(geodetics_matrix)
    geoPolylines <- calculateGeoPolylines(geoMarkers)
    
    
    names = NULL
    for(i in 1:length(sats)){
      names = c(names, sats[[i]]$NORADcatalogNumber)
    }
    
    renderMapSatellites(geoMarkers, geoPolylines, input$dimension, names)
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