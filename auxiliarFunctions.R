get_satelites <- function() {
  test_TLEs <-
    asteRisk::readTLE("www/testTLE.txt")
  lista <- list()
  for (i in seq_along(test_TLEs[])) {
    object_name <- test_TLEs[[i]]$objectName
    datetime <- get_datetime(test_TLEs[[i]]$dateTime)
    key <- paste0(
      object_name,
      " a ",
      datetime,
      " (",
      test_TLEs[[i]]$NORADcatalogNumber,
      ")"
    )
    value <- i
    lista[[key]] <- value
  }
  return(lista)
}

get_datetime <- function(datetime) {
  datetime <-
    as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  datetime_format <- format(datetime, format = "%d-%m-%Y %H:%M:%S")
  return(datetime_format)
}

calculate_results <- function(sat,
                              target_date = NULL,
                              min = NULL,
                              method = NULL) {
  initial_time <- if (!is.null(target_date)) {
    sat$initialDateTime
  } else {
    NULL
  }

  target_times <- NULL
  target_times_date <- NULL
  is_negative <- FALSE

  if (!is.null(target_date)) {
    if (!is.null(initial_time)) {
      diff_dates <- difftime(target_date, initial_time, units = "mins")
      diff_dates <- as.numeric(diff_dates)

      if (diff_dates != 0) {
        diff_dates_abs <- abs(diff_dates)
        is_negative <- diff_dates_abs != diff_dates

        num_points <- diff_dates_abs * 200 / 1440
        if (num_points > 2000) {
          num_points <- 2000
        }
        step <- diff_dates_abs / num_points
        target_times_date <- if (diff_dates < 0) {
          seq(
            as.POSIXct(target_date, tz = "UTC"),
            as.POSIXct(initial_time, tz = "UTC"),
            by = step * 60
          )
        } else {
          seq(
            as.POSIXct(initial_time, tz = "UTC"),
            as.POSIXct(target_date, tz = "UTC"),
            by = step * 60
          )
        }
      } else {
        target_times_date <- c(as.POSIXct(target_date, tz = "UTC"))
      }
      for (i in seq_along(target_times_date)) {
        target_times[i] <- substr(as.character(target_times_date[i]), 1, 19)
      }
    }
  } else {
    min_abs <- abs(min)
    is_negative <- min_abs != min

    num_points <- min_abs * 200 / 1440
    if (num_points > 4700) {
      num_points <- 4700
    } else if (num_points < 1) {
      num_points <- 1
    }
    step <- min_abs / num_points
    target_times <-
      if (min < 0) {
        seq(min, 0, by = step)
      } else {
        seq(0, min, by = step)
      }
  }

  results_position_matrix <-
    matrix(nrow = length(target_times), ncol = 3)
  results_velocity_matrix <-
    matrix(nrow = length(target_times), ncol = 3)

  results_position_matrix_GCRF <-
    matrix(nrow = length(target_times), ncol = 3)
  results_velocity_matrix_GCRF <-
    matrix(nrow = length(target_times), ncol = 3)

  orbital_elements <-
    matrix(nrow = length(target_times), ncol = 5)

  for (i in seq_along(target_times)) {
    if (is.null(method) || method == "SGDP4") {
      new_result <-
        asteRisk::sgdp4(
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
    } else if (method == "SGP4") {
      new_result <-
        asteRisk::sgp4(
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
    } else {
      new_result <-
        asteRisk::sdp4(
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
    }
    results_position_matrix[i, ] <- new_result[[1]]
    results_velocity_matrix[i, ] <- new_result[[2]]

    target_times_date_conversed <- target_times[i]
    if (!is.null(min)) {
      target_times_date_conversed <-
        as.POSIXct(sat$initialDateTime,
          tz = "UTC",
          format = "%Y-%m-%d %H:%M:%S"
        )
      +target_times[i] * 60
      target_times_date_conversed <- format(
        target_times_date_conversed,
        "%Y-%m-%d %H:%M:%S"
      )
    }
    GCRF_coordinates <-
      asteRisk::TEMEtoGCRF(
        results_position_matrix[i, ] * 1000,
        results_velocity_matrix[i, ] * 1000,
        target_times_date_conversed
      )

    results_position_matrix_GCRF[i, ] <-
      round(GCRF_coordinates$position, 3)
    results_velocity_matrix_GCRF[i, ] <-
      round(GCRF_coordinates$velocity, 3)

    all_orbital_elements <-
      asteRisk::ECItoKOE(
        results_position_matrix_GCRF[i, ],
        results_velocity_matrix_GCRF[i, ]
      )

    orbital_elements[i, 1] <-
      round(all_orbital_elements$eccentricity, 3)
    orbital_elements[i, 2] <-
      round(all_orbital_elements$inclination, 3)
    orbital_elements[i, 3] <-
      round(all_orbital_elements$meanAnomaly, 3)
    orbital_elements[i, 4] <-
      round(all_orbital_elements$argumentPerigee, 3)
    orbital_elements[i, 5] <-
      round(all_orbital_elements$longitudeAscendingNode, 3)
  }
  results_position_matrix <-
    cbind(results_position_matrix, target_times)
  colnames(results_position_matrix) <- c("x", "y", "z", "time")

  target_times_min <- target_times
  if (!is.null(target_date)) {
    initial_time <- as.POSIXct(sat$initialDateTime,
      tz = "UTC", format = "%Y-%m-%d %H:%M:%S"
    )
    target_times_min <- as.POSIXct(target_times,
      tz = "UTC", format = "%Y-%m-%d %H:%M:%S"
    )
    target_times_min <- as.numeric(difftime(target_times_min,
      initial_time,
      units = "mins"
    ))
  }

  orbital_elements <-
    cbind(orbital_elements, target_times_min)

  return(
    list(
      results_position_matrix,
      results_velocity_matrix,
      results_position_matrix_GCRF,
      results_velocity_matrix_GCRF,
      orbital_elements,
      target_times,
      is_negative
    )
  )
}

calculate_geodetic_matrix_and_position_two_weeks <- function(sats,
                                                             target_date = NULL,
                                                             min = NULL,
                                                             method = NULL) {
  geodetic_matrixs <- list()
  results_position_matrixs_GCRF <- list()
  results_velocity_matrixs_GCRF <- list()
  orbitals_elements <- list()
  indexes_more_two_weeks <- list()
  indexes_more_two_weeks_two_days <- list()
  is_negative <- list()

  for (i in seq_along(sats)) {
    results_sat <-
      calculate_results(sats[[i]], target_date, min, method)
    results_position_matrix <- results_sat[[1]]
    results_velocity_matrix <- results_sat[[2]]
    results_position_matrix_GCRF <- results_sat[[3]]
    results_velocity_matrix_GCRF <- results_sat[[4]]
    orbital_elements <- results_sat[[5]]
    target_times <- results_sat[[6]]
    is_negative_i <- results_sat[[7]]

    geodetic_matrix <- get_geodetic_matrix(
      results_position_matrix,
      results_velocity_matrix,
      sats[[i]],
      target_times
    )

    # two weeks
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

    # two weeks and two days
    index <- -1
    positions <- data.frame(results_position_matrix)
    if (!is.null(target_date)) {
      positions$time <- as.POSIXct(positions$time, tz = "UTC")
      datetime_limit <- head(positions$time, n = 1) + 16 * 86400
      if (any(positions$time > datetime_limit)) {
        index <- which.max(positions$time > datetime_limit)
      }
    } else {
      if (any(positions$time > 16 * 1440)) {
        index <- which.max(positions$time > 16 * 1440)
      }
    }
    indexes_more_two_weeks_two_days[[i]] <- index

    is_negative[[i]] <- is_negative_i

    geodetic_matrixs[[i]] <- geodetic_matrix

    results_position_matrixs_GCRF[[i]] <-
      results_position_matrix_GCRF
    results_velocity_matrixs_GCRF[[i]] <-
      results_velocity_matrix_GCRF

    orbitals_elements[[i]] <-
      orbital_elements
  }

  return(
    list(
      geodetic_matrixs,
      results_position_matrixs_GCRF,
      results_velocity_matrixs_GCRF,
      orbitals_elements,
      indexes_more_two_weeks,
      indexes_more_two_weeks_two_days,
      is_negative
    )
  )
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
        asteRisk::TEMEtoLATLON(
          as.numeric(results_position_matrix[i, 1:3]) * 1000,
          new_datetime
        )
      geodetic_matrix[i, ] <- new_geodetic
    }

    colnames(geodetic_matrix) <-
      c("latitude", "longitude", "altitude")

    return(geodetic_matrix)
  }

calculate_geodetic_matrix_and_position_two_weeks_hpop <-
  function(sats,
           target_date = NULL,
           min = NULL) {
    geodetic_matrixs <- list()
    results_position_matrixs_GCRF <- list()
    results_velocity_matrixs_GCRF <- list()
    orbitals_elements <- list()
    indexes_more_two_weeks <- list()
    indexes_more_two_weeks_two_days <- list()

    for (i in seq_along(sats)) {
      if (!is.null(target_date)) {
        min <-
          as.numeric(difftime(target_date, sats[[i]]$dateTime, units = "mins"))
      }

      if (min <= 0) {
        return()
      }

      results_sat <- calculate_results(sats[[i]], min = min)
      results_position_matrix <- results_sat[[1]]
      results_velocity_matrix <- results_sat[[2]]
      results_position_matrix_GCRF <- results_sat[[3]]
      results_velocity_matrix_GCRF <- results_sat[[4]]
      orbital_elements <- results_sat[[5]]
      target_times <- results_sat[[6]]

      geodetic_matrix <-
        get_geodetic_matrix_hpop(
          results_position_matrix,
          results_velocity_matrix,
          sats[[i]],
          target_times
        )

      # two weeks
      index <- -1
      positions <- data.frame(results_position_matrix)
      if (any(positions$time > 14 * 1440)) {
        index <- which.max(positions$time > 14 * 1440)
      }
      indexes_more_two_weeks[[i]] <- index

      # two weeks and two days
      index <- -1
      positions <- data.frame(results_position_matrix)
      if (any(positions$time > 16 * 1440)) {
        index <- which.max(positions$time > 16 * 1440)
      }
      indexes_more_two_weeks_two_days[[i]] <- index

      geodetic_matrixs[[i]] <- geodetic_matrix

      results_position_matrixs_GCRF[[i]] <-
        results_position_matrix_GCRF
      results_velocity_matrixs_GCRF[[i]] <-
        results_velocity_matrix_GCRF

      orbitals_elements[[i]] <- orbital_elements
    }

    return(
      list(
        geodetic_matrixs,
        results_position_matrixs_GCRF,
        results_velocity_matrixs_GCRF,
        orbitals_elements,
        indexes_more_two_weeks,
        indexes_more_two_weeks_two_days
      )
    )
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
      asteRisk::TEMEtoGCRF(
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
      asteRisk::hpop(
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
        asteRisk::GCRFtoLATLON(as.numeric(hpop_results[i, 2:4]), new_datetime)
      geodetic_matrix_hpop[i, ] <- new_geodetic
    }

    colnames(geodetic_matrix_hpop) <-
      c("latitude", "longitude", "altitude")

    return(geodetic_matrix_hpop)
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
  rgb_color <- colormod::hsv2rgb(hsb_color)
  hex_color <- colormod::rgb2hex(rgb_color)
  return(hex_color)
}

equally_spaced_lists <- function(df, z) {
  n <- nrow(df)
  if (n < z) {
    return(n - 1)
  } else if (n / z < 4) {
    z <- 10
  }
  step_size <- floor((n - 1) / (z - 1))
  indices <-
    c(1, seq(
      from = step_size + 1,
      to = n - 1,
      by = step_size
    ))
  return(indices)
}

render_map_satellites <-
  function(geo_markers,
           results_position_matrix_GCRF,
           results_velocity_matrix_GCRF,
           orbital_elements,
           dimension,
           names = NULL,
           positions_two_weeks = NULL,
           positions_two_weeks_two_days = NULL,
           type_colors = "palette",
           is_negative = NULL,
           method) {
    # Se define un icono transparente
    my_transparent_icon <- leaflet::makeIcon(
      iconUrl = "www/transparentIcon.png",
      iconWidth = 24,
      iconHeight = 24,
      iconAnchorX = 12,
      iconAnchorY = 12
    )

    # Se define el zoom inicial del mapa
    zoom <- ifelse(dimension[1] > 1000, 2, 0.5)

    # Se crea el objeto del mapa
    map <- leaflet::leaflet() |>
      leaflet::setView(0, 0, zoom) |>
      leaflet::addTiles(options = leaflet::tileOptions(noWrap = TRUE))

    # Se inicializan las variables para los colores y las etiquetas
    colors <- NULL
    labels <- NULL
    divs <- ""

    for (i in seq_along(geo_markers)) {
      geo_markers_i <- as.data.frame(geo_markers[[i]])
      results_position_matrix_GCRF_i <-
        results_position_matrix_GCRF[[i]]
      results_velocity_matrix_GCRF_i <-
        results_velocity_matrix_GCRF[[i]]
      orbital_elements_i <- orbital_elements[[i]]
      positions_two_weeks_i <- ifelse(
        !is.null(positions_two_weeks), positions_two_weeks[[i]], -1
      )
      positions_two_weeks_two_days_i <- ifelse(
        !is.null(positions_two_weeks_two_days),
        positions_two_weeks_two_days[[i]], -1
      )
      is_negative_i <- ifelse(
        !is.null(is_negative), is_negative[[i]], FALSE
      )

      names_i <- ""
      if (!is.null(names)) {
        names_i <- names[[i]]
      }

      pop_ups <- paste(
        "<strong style='font-size:1.1em'>",
        names_i,
        "<br>Posición en coordenadas GCRF</strong>: <br><strong>x</strong>:",
        results_position_matrix_GCRF_i[, 1],
        " m<br><strong>y</strong>:",
        results_position_matrix_GCRF_i[, 2],
        " m<br><strong>z</strong>:",
        results_position_matrix_GCRF_i[, 3],
        " m<br><strong style='font-size:1.1em'>Velocidad</strong>:
         <br><strong>x</strong>:",
        results_velocity_matrix_GCRF_i[, 1],
        " m/s<br><strong>y</strong>:",
        results_velocity_matrix_GCRF_i[, 2],
        " m/s<br><strong>z</strong>:",
        results_velocity_matrix_GCRF_i[, 3],
        " m/s <br><strong style='font-size:1.1em'>
        Elementos orbitales keplerianos</strong>:
        <br><strong>Excentricidad media (0-1)</strong>:",
        orbital_elements_i[, 1],
        "<br><strong>Inclinación media</strong>:",
        orbital_elements_i[, 2],
        " radianes<br><strong>Anomalía media</strong>:",
        orbital_elements_i[, 3],
        " radianes<br><strong>Argumento del perigeo medio</strong>:",
        orbital_elements_i[, 4],
        " radianes<br><strong>Ascensión recta media</strong>:",
        orbital_elements_i[, 5],
        " radianes"
      ) |> lapply(htmltools::HTML)

      # Se definen los colores de los marcadores
      color <-
        if (type_colors == "scale") {
          "green"
        } else {
          switch(i %% 7 + 1,
            randomcoloR::randomColor(luminosity = "dark", hue = "orange"),
            randomcoloR::randomColor(luminosity = "dark", hue = "pink"),
            randomcoloR::randomColor(luminosity = "dark", hue = "blue"),
            randomcoloR::randomColor(luminosity = "dark", hue = "green"),
            randomcoloR::randomColor(luminosity = "dark", hue = "yellow"),
            randomcoloR::randomColor(luminosity = "dark", hue = "red"),
            randomcoloR::randomColor(luminosity = "dark", hue = "purple")
          )
        }
      lighten_color <-
        if (type_colors == "scale") {
          "red"
        } else {
          lighten_color(color)
        }

      if (nrow(geo_markers[[i]]) != 1) {
        color_degr <-
          if (is_negative_i) {
            colorRampPalette(c(lighten_color, color))
          } else {
            colorRampPalette(c(color, lighten_color))
          }
        geo_poly_color <- color_degr(dim(geo_markers_i)[1])

        if (type_colors == "scale" && positions_two_weeks_i != -1) {
          geo_poly_color <- color_degr(positions_two_weeks_i)
          black_degr <-
            if (is_negative_i) {
              colorRampPalette(c("black", lighten_color))
            } else {
              colorRampPalette(c(lighten_color, "black"))
            }

          if (positions_two_weeks_two_days_i != -1) {
            geo_poly_black <-
              black_degr(positions_two_weeks_two_days_i - positions_two_weeks_i)
            geo_poly_color <- if (is_negative_i) {
              c(
                rep(
                  "black",
                  dim(geo_markers_i)[1] - positions_two_weeks_two_days_i
                ),
                geo_poly_black,
                geo_poly_color
              )
            } else {
              c(
                geo_poly_color,
                geo_poly_black,
                rep(
                  "black",
                  dim(geo_markers_i)[1] - positions_two_weeks_two_days_i
                )
              )
            }
          } else {
            geo_poly_black <-
              black_degr(dim(geo_markers_i)[1] - positions_two_weeks_i)
            geo_poly_color <-
              if (is_negative_i) {
                c(geo_poly_black, geo_poly_color)
              } else {
                c(geo_poly_color, geo_poly_black)
              }
          }
        }

        geo_poly <- geo_markers_i |>
          dplyr::mutate(
            nextLat = dplyr::lead(latitude),
            nextLng = dplyr::lead(longitude),
            color = geo_poly_color
          )
        indices <- equally_spaced_lists(geo_poly, 20)

        map <- map |>
          leaflet::addMarkers(
            data = geo_markers_i,
            lng = ~longitude,
            lat = ~latitude,
            label = ~pop_ups,
            icon = my_transparent_icon
          )

        for (j in seq_len(nrow(geo_poly))) {
          j <- as.character(j)
          if (j %in% indices) {
            map <- map |>
              leaflet.extras2::addArrowhead(
                data = geo_poly[j, ],
                lng = c(geo_poly[j, "longitude"], geo_poly[j, "nextLng"]),
                lat = c(geo_poly[j, "latitude"], geo_poly[j, "nextLat"]),
                color = as.character(geo_poly[j, "color"]),
                weight = ifelse(
                  geo_poly[j, "longitude"] < geo_poly[j, "nextLng"], 4, 1
                )
              )
          } else {
            map <- map |>
              leaflet::addPolylines(
                data = geo_poly[j, ],
                lng = c(geo_poly[j, "longitude"], geo_poly[j, "nextLng"]),
                lat = c(geo_poly[j, "latitude"], geo_poly[j, "nextLat"]),
                color = as.character(geo_poly[j, "color"]),
                weight = ifelse(
                  geo_poly[j, "longitude"] < geo_poly[j, "nextLng"], 4, 1
                )
              )
          }
        }

        map <- map |> leaflet::addPopups(
          as.numeric(geo_markers_i$longitude[1]),
          as.numeric(geo_markers_i$latitude[1]),
          if (!is.null(names)) {
            names[i]
          } else {
            "Satélite"
          },
          options = leaflet::popupOptions()
        )
      } else {
        map <- map |>
          leaflet::addAwesomeMarkers(
            data = geo_markers_i,
            ~longitude,
            ~latitude,
            label = pop_ups,
            icon = leaflet::makeAwesomeIcon(
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

      divs <-
        paste(
          divs,
          sprintf(
            "<div>
            <div style='background: linear-gradient(to right, %s, %s);
             width: 50px; height:20px; display:inline;'>
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
             </div>
             &nbsp;&nbsp<div style='display:inline;'>%s</div></div>",
            ifelse(is_negative_i, lighten_color, color),
            ifelse(is_negative_i, color, lighten_color),
            label
          )
        )
    }

    if (any(positions_two_weeks != -1 && type_colors == "scale")) {
      divs <-
        paste(
          divs,
          "<div><div style='background: black; width: 50px;
           height:20px; display:inline;'>
          &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</div>
          &nbsp;&nbsp
          <div style='display:inline;'>Más de dos semanas</div></div>"
        )
    }
    map <- map |>
      leaflet::addControl(
        html = sprintf("<div><strong>Satélites</strong>%s</div>", divs),
        position = "bottomright"
      ) |>
      leaflet::addControl(
        html = sprintf("<div><strong>%s</strong></div", method),
        position = "topright"
      )

    return(map)
  }

render_empty_map <- function(dimension) {
  # Se define el zoom inicial del mapa
  zoom <- ifelse(dimension[1] > 1000, 2, 0.5)

  map <- leaflet::leaflet() |>
    leaflet::setView(0, 0, zoom) |>
    leaflet::addTiles(options = leaflet::tileOptions(noWrap = TRUE))

  return(map)
}
