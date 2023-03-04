getSatelites <- function() {
  test_TLEs <-
    readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
  lista <- list()
  for (i in 1:length(test_TLEs[])) {
    key <- test_TLEs[[i]]$NORADcatalogNumber
    value <- i
    lista[[key]] <- value
  }
  return(lista)
}

calculateResults <- function(sat,
                             targetDate = NULL,
                             min = NULL) {
  initialTime = if (!is.null(targetDate))
    sat$initialDateTime
  else
    NULL
  
  targetTimes = NULL
  if (!is.null(targetDate)) {
    if (!is.null(initialTime)) {
      diff_dates = difftime(targetDate, initialTime, units = "mins")
      if (diff_dates > 0)
      {
        step = diff_dates / 200
        targetTimesDate = seq(as.POSIXct(initialTime, tz = "UTC"),
                              as.POSIXct(targetDate, tz = "UTC"),
                              step)
        for (i in 1:length(targetTimesDate)) {
          targetTimes[i] = substr(as.character(targetTimesDate[i]), 1, 19)
        }
      }
      
    }
  } else {
    step = min / 200
    targetTimes = seq(0, min, step)
  }
  
  targetTime = if (!is.null(targetDate))
    targetDate
  else
    min
  
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
        initialDateTime = initialTime,
        targetTime = targetTimes[i]
      )
    results_position_matrix[i,] <-
      as.numeric(new_result$position[[1]])
    results_velocity_matrix[i,] <-
      as.numeric(new_result$velocity[[2]])
    
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

calculateGeodeticMatrix <- function(sat,
                                    sat2 = NULL,
                                    targetDate1 = NULL,
                                    targetDate2 = NULL,
                                    min1 = NULL,
                                    min2 = NULL) {
  resultsSat <- calculateResults(sat, targetDate1, min1)
  results_position_matrix <- resultsSat[[1]]
  results_velocity_matrix <- resultsSat[[2]]
  targetTimes <- resultsSat[[3]]
  
  if (!is.null(sat2)) {
    resultsSat2 <- calculateResults(sat2, targetDate2, min2)
    results_position_matrix2 <- resultsSat2[[1]]
    results_velocity_matrix2 <- resultsSat2[[2]]
    targetTimes2 <- resultsSat2[[3]]
    
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
        if (typeof(targetTimes[i]) != 'character')
          as.character(as.POSIXct(sat$initialDateTime, tz = "UTC") + 60 *
                         targetTimes[i])
      else
        targetTimes[i]
      
      new_geodetic <-
        TEMEtoLATLON(as.numeric(results_position_matrix[i, 1:3]) * 1000, new_dateTime)
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
      "Latitud:",
      geoMarkers[, 1],
      "Longitud:",
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
        "Latitud:",
        geoMarkers2[, 1],
        "Longitud:",
        geoMarkers2[, 2],
        "Altitud:",
        geoMarkers2[, 3]
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

renderMapSatellites <-
  function(geoMarkers,
           geoPolylines,
           dimension,
           names = NULL) {
    myTransparentIcon <-
      makeIcon(
        iconUrl = "www/transparentIcon.png",
        iconWidth = 24,
        iconHeight = 24,
        iconAnchorX = 12,
        iconAnchorY = 12
      )
    
    zoom = 0.5
    if (dimension[1] > 1000) {
      zoom = 2
    }
    
    map = leaflet() %>%
      setView(0, 0, zoom) %>%
      addTiles(options = tileOptions(noWrap = TRUE))
    
    colors = NULL
    labels = NULL
    for (i in 1:length(geoMarkers)) {
      geoMarkersi = as.data.frame(geoMarkers[[i]])
      geoPolylinesi = geoPolylines[[i]]
      color = if (i %% 2 == 0)
        randomColor(luminosity = 'dark', hue = 'orange')
      else
        randomColor(luminosity = 'dark', hue = 'pink')
      if (nrow(geoMarkers[[i]]) != 1) {
        #flechas
        some_rows = seq_len(nrow(geoPolylinesi)) %% 4
        geoPolylinesiWithoutArrow = geoPolylinesi[some_rows == 1,]
        geoPolylinesiWithArrow = geoPolylinesi[some_rows == 0,]
        
        colorDegr <- colorRampPalette(c(color, "white"))
        
        geoPoly <- geoMarkersi %>%
          mutate(nextLat = lead(latitude),
                 nextLng = lead(longitude),
                 color = colorDegr(dim(geoMarkersi)[1])
                 )
        
        map = map %>%
          addMarkers(
            data = geoMarkersi,
            ~ longitude,
            ~ latitude,
            label = ~ htmlEscape(PopUp),
            icon = myTransparentIcon
          )  
        for (j in 1:nrow(geoPoly)){
          map = map %>%
            addPolylines(data = geoPoly,
                         lng = as.numeric(geoPoly[j, c('longitude', 'nextLng')]),
                         lat = as.numeric(geoPoly[j, c('latitude', 'nextLat')]),
                         color = as.character(geoPoly[j, c('color')]),
                         weight = 3)
        }
          # %>% addPolylines(data = geoPolylinesiWithoutArrow,
          #              color = color,
          #              weight = 3) %>%
          # addArrowhead(data = geoPolylinesiWithArrow,
          #              color = color,
          #              weight = 3) %>%
          map = map %>% addPopups(
            as.numeric(geoMarkersi$longitude[1]),
            as.numeric(geoMarkersi$latitude[1]),
            if (!is.null(names))
              names[i]
            else
              "Satélite",
            options = popupOptions()
          )
      } else {
        map = map %>%
          addAwesomeMarkers(
            data = geoMarkersi,
            ~ longitude,
            ~ latitude,
            label = ~ htmlEscape(PopUp),
            icon = makeAwesomeIcon(
              icon = "star",
              iconColor = color,
              markerColor = 'white',
              library = "fa"
            )
          ) %>%
          addPolylines(data = geoPolylinesi,
                       color = color,
                       weight = 3)
      }
      
      colors = c(colors, color)
      labels = c(labels, if (!is.null(names))
        names[i]
        else
          "Satélite")
    }
    
    map = map %>%
      addLegend(
        "bottomright",
        colors = colors,
        labels = labels,
        title = "Satélites",
        opacity = 1
      )
    
    return(map)
  }
