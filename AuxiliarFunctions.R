get_satelites <- function() {
  test_TLEs <-
    readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
  lista <- list()
  for (i in seq_along(test_TLEs[])) {
    key <- test_TLEs[[i]]$NORADcatalogNumber
    value <- i
    lista[[key]] <- value
  }
  return(lista)
}

calculate_results <- function(sat,
                              target_date = NULL,
                              min = NULL) {
  initial_time <- if (!is.null(target_date)) {
    sat$initialDateTime
  } else {
    NULL
  }

  target_times <- NULL
  if (!is.null(target_date)) {
    if (!is.null(initial_time)) {
      diff_dates <- difftime(target_date, initial_time, units = "mins")
      if (diff_dates > 0) {
        step <- diff_dates / 200
        target_times_date <- seq(
          as.POSIXct(initial_time, tz = "UTC"),
          as.POSIXct(target_date, tz = "UTC"),
          by = step
        )
        for (i in seq_along(target_times_date)) {
          target_times[i] <- substr(as.character(target_times_date[i]), 1, 19)
        }
      }
    }
  } else {
    step <- min / 200
    target_times <- seq(0, min, by = step)
  }

  results_position_matrix <-
    matrix(nrow = length(target_times), ncol = 3)
  results_velocity_matrix <-
    matrix(nrow = length(target_times), ncol = 3)

  for (i in seq_along(target_times)) {
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
        initialDateTime = sat$initialDateTime,
        targetTime = target_times[i]
      )
    results_position_matrix[i, ] <-
      as.numeric(new_result$position[[1]])
    results_velocity_matrix[i, ] <-
      as.numeric(new_result$velocity[[2]])
  }
  last_sat_propagation <- new_result
  results_position_matrix <-
    cbind(results_position_matrix, target_times)
  colnames(results_position_matrix) <- c("x", "y", "z", "time")
  new_datetime <- "2006-06-25 12:33:43"

  # ITRF_coordinates <-
  #   TEMEtoITRF(
  #     last_sat_propagation$position,
  #     last_sat_propagation$velocity,
  #     new_datetime
  #   )

  return(list(
    results_position_matrix,
    results_velocity_matrix,
    target_times
  ))
}

calculate_geodetic_matrix_and_position_two_weeks <- function(sats,
                                      target_date = NULL,
                                      min = NULL) {
  geodetic_matrixs <- list()
  indexes_more_two_weeks <- list()
  for (i in seq_along(sats)) {
    results_sat <- calculate_results(sats[[i]], target_date, min)
    results_position_matrix <- results_sat[[1]]
    results_velocity_matrix <- results_sat[[2]]
    target_times <- results_sat[[3]]

    geodetic_matrix <- get_geodetic_matrix(
      results_position_matrix,
      results_velocity_matrix,
      sats[[i]],
      target_times
    )

    index <- -1
    positions <- data.frame(results_position_matrix)
    positions$time <- as.POSIXct(positions$time, tz = "UTC")
    datetime_limit <- head(positions$time, n = 1) + 14 * 86400
    if (any(positions$time > datetime_limit)) {
      index <- which.max(positions$time > datetime_limit)
    }
    indexes_more_two_weeks[[i]] <- index

    geodetic_matrixs[[i]] <- geodetic_matrix
  }

  return(list(geodetic_matrixs, indexes_more_two_weeks))
}

get_geodetic_matrix <-
  function(results_position_matrix,
           results_velocity_matrix,
           sat,
           target_times) {
    # Let us now convert the previously calculated set of TEME coordinates to
    # geodetic latitude and longitude
    geodetic_matrix <-
      matrix(
        nrow = nrow(results_position_matrix),
        ncol = 3
      )

    for (i in seq_len(nrow(geodetic_matrix))) {
      new_datetime <-
        if (typeof(target_times[i]) != "character") {
          as.character(as.POSIXct(sat$initialDateTime, tz = "UTC") + 60 *
            target_times[i])
        } else {
          target_times[i]
        }

      new_geodetic <-
        TEMEtoLATLON(
          as.numeric(results_position_matrix[i, 1:3]) * 1000,
          new_datetime
        )
      geodetic_matrix[i, ] <- new_geodetic
    }

    colnames(geodetic_matrix) <-
      c("latitude", "longitude", "altitude")

    return(geodetic_matrix)
  }

calculate_geodetic_matrix_hpop <-
  function(sat,
           results_position_matrix,
           results_velocity_matrix) {
    sat_mass <- 1600
    sat_cross_section <- 15
    sat_cd <- 2.2
    sat_cr <- 1.2

    GCRF_coordinates <-
      TEMEtoGCRF(
        results_position_matrix[1, 1:3] * 1000,
        results_velocity_matrix[
          1,
          1:3
        ] * 1000,
        sat$dateTime
      )

    initial_position <- GCRF_coordinates$position
    initial_velocity <- GCRF_coordinates$velocity

    # Let´s use the HPOP to calculate the position each 2 minutes
    # during a period of 3 hours

    target_times <- seq(0, 10800, by = 120)

    hpop_results <-
      hpop(
        initial_position,
        initial_velocity,
        sat$dateTime,
        target_times,
        sat_mass,
        sat_cross_section,
        sat_cross_section,
        sat_cr,
        sat_cd
      )

    # Now we can calculate and plot the corresponding geodetic coordinates

    geodetic_matrix_hpop <-
      matrix(nrow = nrow(hpop_results), ncol = 3)

    for (i in seq_len(nrow(geodetic_matrix_hpop))) {
      new_datetime <-
        as.character(as.POSIXct(sat$dateTime, tz = "UTC") + target_times[i])
      new_geodetic <-
        GCRFtoLATLON(as.numeric(hpop_results[i, 2:4]), new_datetime)
      geodetic_matrix_hpop[i, ] <- new_geodetic
    }

    colnames(geodetic_matrix_hpop) <-
      c("latitude", "longitude", "altitude")

    return(geodetic_matrix_hpop)
  }

get_geodetics_matrix_hpop <- function(sat, sat2) {
  results_position_matrix <- calculate_results(sat)[[1]]
  results_velocity_matrix <- calculate_results(sat)[[2]]
  results_position_matrix2 <- calculate_results(sat2)[[1]]
  results_velocity_matrix2 <- calculate_results(sat2)[[2]]

  geodetics_matrix_hpop <- calculate_geodetic_matrix_hpop(
    sat,
    results_position_matrix, results_velocity_matrix
  )
  geodetics_matrix_hpop2 <- calculate_geodetic_matrix_hpop(
    sat2,
    results_position_matrix2, results_velocity_matrix2
  )

  return(list(geodetics_matrix_hpop, geodetics_matrix_hpop2))
}

calculate_geo_markers <- function(geodetics_matrix) {
  geo_markerss <- list()

  for (i in seq_along(geodetics_matrix)) {
    geo_matrix <- geodetics_matrix[i]

    geo_markers <- as.data.frame(geo_matrix[[1]])
    # geo_markers <- cbind(
    #   geo_markers,
    # PopUp = paste(
    #   "Latitud:",
    #   geo_markers[, 1],
    #   "<br>Longitud:",
    #   geo_markers[, 2],
    #   "<br>Altitud:",
    #   geo_markers[, 3]
    # )
    # )

    geo_markerss[[i]] <- geo_markers
  }

  return(geo_markerss)
}

calculate_geo_polylines <- function(geo_markers) {
  geo_polyliness <- list()

  for (i in seq_along(geo_markers)) {
    geo_polylines <- as.data.frame(geo_markers[1])[c("longitude", "latitude")]
    geo_polylines <- matrix(unlist(geo_polylines), ncol = 2)

    geo_polyliness[[i]] <- geo_polylines
  }

  return(geo_polyliness)
}

render_map_satellites <-
  function(geo_markers,
           geo_polylines,
           dimension,
           names = NULL,
           positions_two_weeks) {
    my_transparent_icon <-
      makeIcon(
        iconUrl = "www/transparentIcon.png",
        iconWidth = 24,
        iconHeight = 24,
        iconAnchorX = 12,
        iconAnchorY = 12
      )

    zoom <- 0.5
    if (dimension[1] > 1000) {
      zoom <- 2
    }

    map <- leaflet() %>%
      setView(0, 0, zoom) %>%
      addTiles(options = tileOptions(noWrap = TRUE))

    colors <- NULL
    labels <- NULL

    for (i in seq_along(geo_markers)) {
      geo_markers_i <- as.data.frame(geo_markers[[i]])
      positions_two_weeks_i <- positions_two_weeks[[i]]
      print(positions_two_weeks_i)
      popUps <- paste("Latitud:", geo_markers_i$latitude,
      "<br>Longitud:", geo_markers_i$longitude,
      "<br>Altitud:", geo_markers_i$altitude) %>% lapply(htmltools::HTML)
      geo_polylines_i <- geo_polylines[[i]]

      color <- if (i %% 5 == 0) {
        randomColor(luminosity = "dark", hue = "orange")
      } else if (i %% 5 == 1) {
        randomColor(luminosity = "dark", hue = "pink")
      } else if (i %% 5 == 2) {
        randomColor(luminosity = "dark", hue = "blue")
      } else if (i %% 5 == 3) {
        randomColor(luminosity = "dark", hue = "green")
      } else if (i %% 5 == 4) {
        randomColor(luminosity = "dark", hue = "yellow")
      }
      if (nrow(geo_markers[[i]]) != 1) {
        # flechas
        # some_rows <- seq_len(nrow(geo_polylines_i)) %% 4
        # geo_polylines_i_without_arrow <- geo_polylines_i[some_rows == 1, ]
        # geo_polylines_i_with_arrow <- geo_polylines_i[some_rows == 0, ]

        color_degr <- colorRampPalette(c(color, "white"))

        geo_poly <- geo_markers_i %>%
          mutate(
            nextLat = lead(latitude),
            nextLng = lead(longitude),
            color = color_degr(dim(geo_markers_i)[1])
          )

        map <- map %>%
          addMarkers(
            data = geo_markers_i,
            lng = ~longitude,
            lat = ~latitude,
            label = ~popUps,
            icon = my_transparent_icon
          )

        for (j in seq_len(nrow(geo_poly))) {
          if (is.na(geo_poly[j, "nextLng"]) ||
            geo_poly[j, "longitude"] < geo_poly[j, "nextLng"]) {
            map <- map %>%
              addPolylines(
                data = geo_poly,
                lng = as.numeric(geo_poly[j, c("longitude", "nextLng")]),
                lat = as.numeric(geo_poly[j, c("latitude", "nextLat")]),
                color = if(positions_two_weeks_i != -1 && (positions_two_weeks_i <= j) ) "black"
                else as.character(geo_poly[j, c("color")]),
                weight = 4
              )
          } else {
            map <- map %>%
              addPolylines(
                data = geo_poly,
                lng = as.numeric(geo_poly[j, c("longitude", "nextLng")]),
                lat = as.numeric(geo_poly[j, c("latitude", "nextLat")]),
                color = if(positions_two_weeks_i != -1 && (positions_two_weeks_i <= j) ) "black"
                else as.character(geo_poly[j, c("color")]),
                weight = 1
              )
          }
        }
        # %>% addPolylines(data = geoPolylinesiWithoutArrow,
        #              color = color,
        #              weight = 3) %>%
        # addArrowhead(data = geoPolylinesiWithArrow,
        #              color = color,
        #              weight = 3) %>%
        map <- map %>% addPopups(
          as.numeric(geo_markers_i$longitude[1]),
          as.numeric(geo_markers_i$latitude[1]),
          if (!is.null(names)) {
            names[i]
          } else {
            "Satélite"
          },
          options = popupOptions()
        )
      } else {
        map <- map %>%
          addAwesomeMarkers(
            data = geo_markers_i,
            ~longitude,
            ~latitude,
            label = ~ htmlEscape(PopUp),
            icon = makeAwesomeIcon(
              icon = "star",
              iconColor = color,
              markerColor = "white",
              library = "fa"
            )
          ) %>%
          addPolylines(
            data = geo_polylines_i,
            color = color,
            weight = 3
          )
      }

      colors <- c(colors, color)
      labels <- c(labels, if (!is.null(names)) {
        names[i]
      } else {
        "Satélite"
      })
    }

    map <- map %>%
      addLegend(
        "bottomright",
        colors = colors,
        labels = labels,
        title = "Satélites",
        opacity = 1
      )

    return(map)
  }
