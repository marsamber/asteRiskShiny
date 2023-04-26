get_satelites <- function() {
  test_TLEs <-
    readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
  lista <- list()
  for (i in seq_along(test_TLEs[])) {
    object_name <- get_object_name(test_TLEs[[i]]$NORADcatalogNumber)
    datetime <- get_datetime(test_TLEs[[i]]$dateTime)
    key <- paste0(
      object_name, " a ", datetime,
      " (", test_TLEs[[i]]$NORADcatalogNumber, ")"
    )
    value <- i
    lista[[key]] <- value
  }
  return(lista)
}

get_object_name <- function(NORAD_catalog_number) {
  switch(NORAD_catalog_number,
    "00005" = "VANGUARD 1",
    "04632" = "TITAN 3C TRANSTAGE R/B",
    "06251" = "DELTA 1 DEB",
    "08195" = "MOLNIYA 2-14",
    "09880" = "MOLNIYA 1-36",
    "09998" = "SMS 1 AKM",
    "11801" = "SL-12 R/B(AUX MOTOR)",
    "14128" = "EUTELSAT 1-F1 (ECS 1)",
    "16925" = "SL-6 R/B(2)",
    "20413" = "SL-12 R/B",
    "21897" = "MOLNIYA 1-83",
    "22312" = "SL-6 R/B(2)",
    "22674" = "SL-6 R/B(2)",
    "23177" = "ARIANE 44L+ R/B",
    "23333" = "WIND",
    "23599" = "ARIANE 42P+3 R/B",
    "24208" = "ITALSAT 2",
    "25954" = "AMC-4 (GE-4)",
    "26900" = "INTELSAT 902",
    "26975" = "COSMOS 1024 DEB",
    "28057" = "CBERS 2",
    "28129" = "NAVSTAR 53 (USA 175)",
    "28350" = "COSMOS 2405",
    "28623" = "H-2 R/B",
    "28626" = "XM-3",
    "28872" = "MINOTAUR R/B",
    "29141" = "SL-14 DEB",
    "29238" = "SL-12 DEB",
    "88888" = "Original STR"
  )
}

get_datetime <- function(datetime) {
  datetime <- as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  datetime_format <- format(datetime, format = "%d-%m-%Y %H:%M:%S")
  return(datetime_format)
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
        num_points <- as.numeric(diff_dates) * 200 / 1440
        if (num_points > 2000) {
          num_points <- 2000
        }
        step <- diff_dates / num_points
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
    num_points <- min * 200 / 1440
    if (num_points > 4700) {
      num_points <- 4700
    } else if (num_points < 1) {
      num_points <- 1
    }
    step <- min / num_points
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
    # results_position_matrix[i, ] <-
    #   as.numeric(new_result$position[[1]])
    # results_velocity_matrix[i, ] <-
    #   as.numeric(new_result$velocity[[2]])
    results_position_matrix[i, ] <- new_result[[1]]
    results_velocity_matrix[i, ] <- new_result[[2]]
  }
  last_sat_propagation <- new_result
  results_position_matrix <-
    cbind(results_position_matrix, target_times)
  colnames(results_position_matrix) <- c("x", "y", "z", "time")

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
    if (!is.null(target_date)) {
      positions$time <- as.POSIXct(positions$time, tz = "UTC")
      datetime_limit <- head(positions$time, n = 1) + 14 * 86400
      if (any(positions$time > datetime_limit)) {
        index <- which.max(positions$time > datetime_limit)
      }
    } else {
      if (any(positions$time > 14 * 1440)) {
        index <- which.max(positions$time > 14 * 1440)
      }
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

get_geodetic_matrix_hpop <-
  function(results_position_matrix,
           results_velocity_matrix,
           sat,
           target_times) {
    sat_mass <- 1600
    sat_cross_section <- 15
    sat_cd <- 2.2
    sat_cr <- 1.2

    GCRF_coordinates <-
      TEMEtoGCRF(
        as.numeric(results_position_matrix[1, 1:3]) * 1000,
        as.numeric(results_velocity_matrix[
          1,
          1:3
        ]) * 1000,
        sat$dateTime
      )

    initial_position <- GCRF_coordinates$position
    initial_velocity <- GCRF_coordinates$velocity

    target_times <- target_times * 60

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

calculate_geodetic_matrix_and_position_two_weeks_hpop <- function(sats, min = NULL) {
  geodetic_matrixs <- list()
  indexes_more_two_weeks <- list()

  for (i in seq_along(sats)) {
    results_sat <- calculate_results(sats[[i]], min = min)
    results_position_matrix <- results_sat[[1]]
    results_velocity_matrix <- results_sat[[2]]
    target_times <- results_sat[[3]]

    geodetic_matrix <- get_geodetic_matrix_hpop(
      results_position_matrix,
      results_velocity_matrix,
      sats[[i]],
      target_times
    )

    index <- -1
    positions <- data.frame(results_position_matrix)
    if (any(positions$time > 14 * 1440)) {
      index <- which.max(positions$time > 14 * 1440)
    }
    indexes_more_two_weeks[[i]] <- index

    geodetic_matrixs[[i]] <- geodetic_matrix
  }

  return(list(geodetic_matrixs, indexes_more_two_weeks))
}

calculate_geo_markers <- function(geodetics_matrix) {
  list_of_geo_markers <- list()
  
  for (i in seq_along(geodetics_matrix)) {
    geo_matrix <- geodetics_matrix[i]
    data_frame_geo_markers <- as.data.frame(geo_matrix[[1]])
    list_of_geo_markers[[i]] <- data_frame_geo_markers
  }
  
  return(list_of_geo_markers)
}


lighten_color <- function(hex_color) {
  rgb_color <- col2rgb(hex_color)
  hsb_color <- rgb2hsv(rgb_color[1], rgb_color[2], rgb_color[3])
  hsb_color[2] <- as.numeric(hsb_color[2]) * 0.2
  hsb_color[3] <- as.numeric(hsb_color[3]) * 3
  hsb_color[hsb_color > 1] <- 1
  rgb_color <- hsv2rgb(hsb_color)
  hex_color <- rgb2hex(rgb_color)
  return(hex_color)
}

equally_spaced_lists <- function(df, z) {
  n <- nrow(df)
  if (n < z) {
    return (n-1)
  } else if ( n / z < 4) {
    z = 10
  }
  tamaño_salto <- floor((n - 1) / (z - 1))
  indices <- c(1, seq(from = tamaño_salto + 1, to = n - 1, by = tamaño_salto))
  return(indices)
}

render_map_satellites <-
  function(geo_markers,
           dimension,
           names = NULL,
           positions_two_weeks) {
   
    # Se define un icono transparente
    my_transparent_icon <- makeIcon(
      iconUrl = "www/transparentIcon.png",
      iconWidth = 24,
      iconHeight = 24,
      iconAnchorX = 12,
      iconAnchorY = 12
    )
    
    # Se define el zoom inicial del mapa
    zoom <- ifelse(dimension[1] > 1000, 2, 0.5)
    
    # Se crea el objeto del mapa
    map <- leaflet() %>%
      setView(0, 0, zoom) %>%
      addTiles(options = tileOptions(noWrap = TRUE))
    
    # Se inicializan las variables para los colores y las etiquetas
    colors <- NULL
    labels <- NULL
    divs <- ""

    for (i in seq_along(geo_markers)) {
      geo_markers_i <- as.data.frame(geo_markers[[i]])
      positions_two_weeks_i <- positions_two_weeks[[i]]
      
      popUps <- paste(
        "Latitud:", geo_markers_i$latitude,
        "<br>Longitud:", geo_markers_i$longitude,
        "<br>Altitud:", geo_markers_i$altitude
      ) %>% lapply(htmltools::HTML)

      # Se definen los colores de los marcadores
      color <- switch(i %% 5 + 1,
                      randomColor(luminosity = "dark", hue = "orange"),
                      randomColor(luminosity = "dark", hue = "pink"),
                      randomColor(luminosity = "dark", hue = "blue"),
                      randomColor(luminosity = "dark", hue = "green"),
                      randomColor(luminosity = "dark", hue = "yellow"))
      lighten_color <- lighten_color(color)
      
      if (nrow(geo_markers[[i]]) != 1) {

        color_degr <- colorRampPalette(c(color, lighten_color))

        geo_poly <- geo_markers_i %>%
          mutate(
            nextLat = lead(latitude),
            nextLng = lead(longitude),
            color = color_degr(dim(geo_markers_i)[1])
          )
        indices <- equally_spaced_lists(geo_poly, 20)

        map <- map %>%
          addMarkers(
            data = geo_markers_i,
            lng = ~longitude,
            lat = ~latitude,
            label = ~popUps,
            icon = my_transparent_icon
          )

        for (j in seq_len(nrow(geo_poly))) {
          j <- as.character(j)
          if(j %in% indices) {
            map <- map %>%
              addArrowhead(
                data = geo_poly[j, ],
                lng = c(geo_poly[j, "longitude"], geo_poly[j, "nextLng"]),
                lat = c(geo_poly[j, "latitude"], geo_poly[j, "nextLat"]),
                color = if (positions_two_weeks_i != -1 && (positions_two_weeks_i <= j)) {
                  "black"
                } else {
                  as.character(geo_poly[j, "color"])
                },
                weight = ifelse(geo_poly[j, "longitude"] < geo_poly[j, "nextLng"], 4, 1)
              )
          } else {
            map <- map %>%
            addPolylines(
              data = geo_poly[j, ],
              lng = c(geo_poly[j, "longitude"], geo_poly[j, "nextLng"]),
              lat = c(geo_poly[j, "latitude"], geo_poly[j, "nextLat"]),
              color = if (positions_two_weeks_i != -1 && (positions_two_weeks_i <= j)) {
                "black"
              } else {
                as.character(geo_poly[j, "color"])
              },
              weight = ifelse(geo_poly[j, "longitude"] < geo_poly[j, "nextLng"], 4, 1)
            ) 
          }
        }
        
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
            label = popUps,
            icon = makeAwesomeIcon(
              icon = "star",
              iconColor = color,
              markerColor = "white",
              library = "fa"
            )
          )
      }

      colors <- c(colors, color)

      label <- if (!is.null(names)) {
        names[i]
      } else {
        "Satélite"
      }
      labels <- c(labels, label)

      divs <- paste(divs, sprintf("<div><div style='background: linear-gradient(to right, %s, %s); width: 50px; height:20px; display:inline;'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</div>&nbsp;&nbsp<div style='display:inline;'>%s</div></div>", color, lighten_color, label))
    }

    if (any(positions_two_weeks != -1)) {
      divs <- paste(divs, "<div><div style='background: black; width: 50px; height:20px; display:inline;'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</div>&nbsp;&nbsp<div style='display:inline;'>Más de dos semanas</div></div>")
    }
    map <- map %>%
      addControl(html = sprintf("<div><strong>Satélites</strong>%s</div>", divs), position = "bottomright")

    return(map)
  }

render_empty_map <- function(dimension) {
  # Se define el zoom inicial del mapa
  zoom <- ifelse(dimension[1] > 1000, 2, 0.5)

  map <- leaflet() %>%
    setView(0, 0, zoom) %>%
    addTiles(options = tileOptions(noWrap = TRUE))

  return(map)
}
