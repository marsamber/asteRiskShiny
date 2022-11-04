getSatelites <- function() {
  test_TLEs <-
    readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
  lista <- list()
  for (i in 1:length(test_TLEs[])) {
    key <- test_TLEs[[i]]$objectName
    value <- i
    lista[[key]] <- value
  }
  return(lista)
}

calculateResults <- function(sat) {
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
  results_position_matrix <-
    cbind(results_position_matrix, targetTimes)
  colnames(results_position_matrix) <- c("x", "y", "z", "time")
  new_dateTime <- "2006-06-25 12:33:43"
  
  ITRF_coordinates <-
    TEMEtoITRF(last_sat_propagation$position,
               last_sat_propagation$velocity,
               new_dateTime)
  
  return(list(
    results_position_matrix,
    results_velocity_matrix,
    targetTimes
  ))
}

calculateGeodeticMatrix <- function(sat, sat2 = NULL) {
  results_position_matrix <- calculateResults(sat)[[1]]
  results_velocity_matrix <- calculateResults(sat)[[2]]
  targetTimes <- calculateResults(sat)[[3]]
  
  if (!is.null(sat2)) {
    results_position_matrix2 <- calculateResults(sat2)[[1]]
    results_velocity_matrix2 <- calculateResults(sat2)[[2]]
    targetTimes2 <- calculateResults(sat2)[[3]]
    
    return(list(
      getGeodeticMatrix(
        results_position_matrix,
        results_velocity_matrix,
        sat,
        targetTimes
      ),
      getGeodeticMatrix(
        results_position_matrix2,
        results_velocity_matrix2,
        sat2,
        targetTimes2
      )
    ))
  }
  
  return(list(
    getGeodeticMatrix(
      results_position_matrix,
      results_velocity_matrix,
      sat,
      targetTimes
    )
  ))
  
}

getGeodeticMatrix <-
  function(results_position_matrix,
           results_velocity_matrix,
           sat,
           targetTimes) {
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

calculateGeodeticMatrixHpop <-
  function(sat,
           results_position_matrix,
           results_velocity_matrix) {
    satMass <- 1600
    satCrossSection <- 15
    satCd <- 2.2
    satCr <- 1.2
    
    GCRF_coordinates <-
      TEMEtoGCRF(results_position_matrix[1, 1:3] * 1000,
                 results_velocity_matrix[1,
                                         1:3] * 1000,
                 sat$dateTime)
    
    initialPosition <- GCRF_coordinates$position
    initialVelocity <- GCRF_coordinates$velocity
    
    # Let´s use the HPOP to calculate the position each 2 minutes during a period
    # of 3 hours
    
    targetTimes <- seq(0, 10800, by = 120)
    
    hpop_results <-
      hpop(
        initialPosition,
        initialVelocity,
        sat$dateTime,
        targetTimes,
        satMass,
        satCrossSection,
        satCrossSection,
        satCr,
        satCd
      )
    
    # Now we can calculate and plot the corresponding geodetic coordinates
    
    geodetic_matrix_hpop <-
      matrix(nrow = nrow(hpop_results), ncol = 3)
    
    for (i in 1:nrow(geodetic_matrix_hpop)) {
      new_dateTime <-
        as.character(as.POSIXct(sat$dateTime, tz = "UTC") + targetTimes[i])
      new_geodetic <-
        GCRFtoLATLON(as.numeric(hpop_results[i, 2:4]), new_dateTime)
      geodetic_matrix_hpop[i, ] <- new_geodetic
    }
    
    colnames(geodetic_matrix_hpop) <-
      c("latitude", "longitude", "altitude")
    
    return(geodetic_matrix_hpop)
  }

getGeodeticsMatrixHpop <- function(sat, sat2) {
  results_position_matrix <- calculateResults(sat)[[1]]
  results_velocity_matrix <- calculateResults(sat)[[2]]
  results_position_matrix2 <- calculateResults(sat2)[[1]]
  results_velocity_matrix2 <- calculateResults(sat2)[[2]]
  
  geodetics_matrix_hpop <-
    calculateGeodeticMatrixHpop(sat, results_position_matrix, results_velocity_matrix)
  geodetics_matrix_hpop2 <-
    calculateGeodeticMatrixHpop(sat2, results_position_matrix2, results_velocity_matrix2)
  
  return(list(geodetics_matrix_hpop, geodetics_matrix_hpop2))
}

calculateGeoMarkers <- function(geodetics_matrix) {
  geoMatrix <- geodetics_matrix[1]
  
  geoMarkers <- as.data.frame(geoMatrix[[1]])
  geoMarkers <- cbind(
    geoMarkers,
    PopUp = paste(
      "Longitud:",
      geoMarkers[, 1],
      "Latitud:",
      geoMarkers[, 2],
      "Altitud:",
      geoMarkers[, 3]
    )
  )
  if (length(geodetics_matrix) == 2) {
    geoMatrix2 <- geodetics_matrix[2]
    
    geoMarkers2 <- as.data.frame(geoMatrix2[[1]])
    geoMarkers2 <- cbind(
      geoMarkers2,
      PopUp = paste(
        "Longitud:",
        geoMarkers[, 1],
        "Latitud:",
        geoMarkers[, 2],
        "Altitud:",
        geoMarkers[, 3]
      )
    )
    
    return(list(geoMarkers, geoMarkers2))
  }
  
  return(list(geoMarkers))
  
}

calculateGeoPolylines <- function(geoMarkers) {
  geoPolylines <-
    as.data.frame(geoMarkers[1])[c("longitude", "latitude")]
  geoPolylines <- matrix(unlist(geoPolylines), ncol = 2)
  
  if (length(geoMarkers) == 2) {
    geoPolylines2 <-
      as.data.frame(geoMarkers[2])[c("longitude", "latitude")]
    geoPolylines2 <- matrix(unlist(geoPolylines2), ncol = 2)
    
    return(list(geoPolylines, geoPolylines2))
  }
  
  return(list(geoPolylines))
  
}

renderMapTwoSatellites <- function(geoMarkers, geoPolylines) {
  myTransparentIcon <-
    makeIcon(
      iconUrl = "www/transparentIcon.png",
      iconWidth = 24,
      iconHeight = 24,
      iconAnchorX = 12,
      iconAnchorY = 12
    )
  
  leaflet() %>%
    setView(0, 0, 0.5) %>%
    addTiles() %>%
    addMarkers(
      data = geoMarkers[[1]],
      ~ longitude,
      ~ latitude,
      label =  ~ htmlEscape(PopUp),
      icon = myTransparentIcon
    ) %>%
    addArrowhead(data = geoPolylines[[1]],
                 color = "blue",
                 weight = 3) %>%
    addMarkers(
      data = geoMarkers[[2]],
      ~ longitude,
      ~ latitude,
      label = ~ htmlEscape(PopUp),
      icon = myTransparentIcon
    ) %>%
    addArrowhead(data = geoPolylines[[2]],
                 color = "red",
                 weight = 3)
}

renderMapOneSatellite <- function(geoMarkers, geoPolylines) {
  myTransparentIcon <-
    makeIcon(
      iconUrl = "www/transparentIcon.png",
      iconWidth = 24,
      iconHeight = 24,
      iconAnchorX = 12,
      iconAnchorY = 12
    )
  
  leaflet() %>%
    setView(0, 0, 0.5) %>%
    addTiles() %>%
    addMarkers(
      data = geoMarkers[[1]],
      ~ longitude,
      ~ latitude,
      label =  ~ htmlEscape(PopUp),
      icon = myTransparentIcon
    ) %>%
    addArrowhead(data = geoPolylines[[1]],
                 color = "blue",
                 weight = 3)
}
