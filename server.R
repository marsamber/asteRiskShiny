source("auxiliarFunctions.R", local = TRUE)

server <- function(input, output, session) {
  proceso_en_ejecucion <- shiny::reactiveValues(estado = NULL)

  shiny::observeEvent(input$get_latest_space_data, {
    # Cambiar el estado a "En ejecución" cuando se hace clic en el botón
    proceso_en_ejecucion$estado <- "En ejecución"
    flush.console()
    asteRisk::getLatestSpaceData()
    # Cambiar el estado a "Completado" cuando el proceso ha terminado
    proceso_en_ejecucion$estado <- "Completado"
  })

  output$output <- shiny::renderPrint({
    if (is.null(proceso_en_ejecucion$estado)) {
      "Pulsa el botón para comenzar el proceso"
    } else if (proceso_en_ejecucion$estado == "En ejecución") {
      "El proceso está en ejecución..."
    } else {
      "El proceso ha terminado"
    }
  })

  test_TLEs <-
    asteRisk::readTLE("www/testTLE.txt")
  id_notification_differents <- NULL
  id_notification_file <- NULL
  id_notification_HPOP_neg <- NULL
  id_notification_error <- NULL

  read_file <- function(file) {
    if (!is.null(id_notification_error)) {
        shiny::removeNotification(id_notification_error)
      }
    id_notification_error <<- NULL

    # Leer el contenido del archivo
    file_content <- readLines(file$datapath)

    # Agregar un salto de línea al final del archivo si no existe
    if (!grepl("\\s*$", file_content[length(file_content)])) {
      file_content <- c(file_content, "")
    }

    # Crear un nuevo archivo temporal con el contenido modificado
    temp_file <- tempfile()
    writeLines(file_content, temp_file)

    # Pasar el datapath del archivo temporal a la función readTLE
    sats <- asteRisk::readTLE(filename = temp_file)

    # Eliminar el archivo temporal después de su uso
    unlink(temp_file)

    has_NA <- any(sapply(sats, function(x) any(is.na(x))))
    if (has_NA) {
      id_notification_error <<- shiny::showNotification(
        "Se produjo un error al leer el archivo TLE.
        Verifica que el formato del archivo sea correcto.",
        type = "error",
        duration = 0
      )
      return(NULL)
    }

    return(sats)
  }

  get_more_recent_date <- function() {
    shiny::req(input$TLEFile)

    file <- input$TLEFile
    sats <- read_file(file)
    dates <- NULL

    if (is.null(names(sats))) {
      for (i in seq_along(sats)) {
        date <- sats[[i]]$dateTime
        date <- as.POSIXct(date, tz = "UTC")
        dates <- c(dates, date)
      }

      dates <- sort(dates, decreasing = TRUE)
      date_string <- format(as.POSIXct(dates[1],
        origin = "1970-01-01", tz = "UTC"
      ))
    } else {
      date_string <- sats$dateTime
    }

    return(date_string)
  }

  shiny::observe({
    if (!is.null(input$satelite)) {
      initial_datetime_sat <- NULL
      initial_date_sat <- NULL
      if (input$data == "selectSats") {
        initial_datetime_sat <- test_TLEs[[strtoi(input$satelite)]]$dateTime
        initial_date_sat <- substr(initial_datetime_sat, 1, 10)

        shiny::updateDateInput(session,
          "targetDateSat",
          value = as.Date(initial_date_sat) + 1
        )
        shinyTime::updateTimeInput(session, "targetTimeSat", value = Sys.time())

        if (!is.null(id_notification_file)) {
          shiny::removeNotification(id_notification_file)
        }
        id_notification_file <<- NULL
      } else {
        if (!is.null(input$TLEFile)) {
          initial_datetime_sat <- get_more_recent_date()
          initial_date_sat <- substr(initial_datetime_sat, 1, 10)
          shiny::updateDateInput(session,
            "targetDateSat",
            value = as.Date(initial_date_sat) + 1
          )
          shinyTime::updateTimeInput(session,
            "targetTimeSat",
            value = Sys.time()
          )

          if (!is.null(id_notification_file)) {
            shiny::removeNotification(id_notification_file)
          }
          id_notification_file <<- NULL
        } else {
          if (!is.null(id_notification_file)) {
            return()
          }
          id_notification_file <<-
            shiny::showNotification(
              paste("Debe subir un fichero
                                                        para visualizar"),
              duration = 0,
              type = "warning"
            )
        }
      }

      if (input$metodos == "SGDP4") {
        shiny::updateDateInput(session,
          "targetDateSat",
          min = NA
        )
        shiny::updateNumericInput(session,
          "propagationTimeSat",
          min = -Inf
        )
      } else if (input$metodos == "HPOP") {
        shiny::updateDateInput(session,
          "targetDateSat",
          min = initial_date_sat
        )
        shiny::updateNumericInput(session,
          "propagationTimeSat",
          min = 1
        )
      }
    }
  })

  shiny::observe({
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab1",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab2",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab3",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab4",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab5",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab6",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab7",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab8",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab9",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab10",
      session = shiny::getDefaultReactiveDomain()
    )

    shiny::hideTab("tabsetpanelHPOP", "HPOPTab1",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab2",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab3",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab4",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab5",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab6",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab7",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab8",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab9",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab10",
      session = shiny::getDefaultReactiveDomain()
    )
  })

  shiny::observe({
    shinyTime::updateTimeInput(
      session, "initialTimeSimulator",
      value = Sys.time()
    )
    shinyTime::updateTimeInput(
      session, "targetTimeSimulator",
      value = Sys.time()
    )
  })

  output$select_satelite <- shiny::renderUI({
    shiny::selectInput("satelite", p("Satélite"), choices = get_satelites())
  })


  output$initialDateSat <-
    shiny::renderText({
      shiny::req(input$satelite)
      if (input$data == "selectSats") {
        test_TLEs[[strtoi(input$satelite)]]$dateTime
      } else {
        if (!is.null(input$TLEFile)) {
          get_more_recent_date()
        } else {
          "Sube tu fichero TLE"
        }
      }
    })

  shiny::observeEvent(input$metodos, {
    shiny::updateTabsetPanel(session, "metodos", selected = input$metodos)
  })

  output$SGDP4Map <- leaflet::renderLeaflet({
    if (!is.null(tabs_data())) {
      tabs_data()
    }
  })

  output$HPOPMap <- leaflet::renderLeaflet({
    if (!is.null(tabs_data())) {
      tabs_data()
    }
  })

  output$myMap <- leaflet::renderLeaflet({
    if (!is.null(map_data_simulator())) {
      map_data_simulator()
    }
  })

  render_SGDP4 <- function() {
    shiny::req(input$satelite)
    shiny::req(input$propagationTime)
    shiny::req(input$dimension)
    shiny::req(input$data)
    shiny::req(input$method)
    shiny::req(input$colors)

    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab1",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab2",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab3",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab4",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab5",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab6",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab7",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab8",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab9",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelSGDP4", "SGDP4Tab10",
      session = shiny::getDefaultReactiveDomain()
    )

    sats <- list()
    differents <- FALSE
    method <- input$method
    type_colors <- input$colors
    is_negative <- FALSE

    result_position_matrix_GCRF <- NULL
    result_velocity_matrix_GCRF <- NULL
    orbital_elements <- NULL

    if (!is.null(id_notification_HPOP_neg)) {
      shiny::removeNotification(id_notification_HPOP_neg)
    }
    id_notification_HPOP_neg <<- NULL

    if (input$data == "selectSats") {
      sat <- test_TLEs[[strtoi(input$satelite)]]
      sat <- c(sat, initialDateTime = sat$dateTime)
      sats[[1]] <- sat
    } else {
      if (!is.null(input$TLEFile)) {
        file <- input$TLEFile
        TLE_sats <- read_file(file)
        if (!is.null(names(TLE_sats))) {
          sats[[1]] <- TLE_sats
        } else {
          sats <- TLE_sats
        }
        number_NORAD <- sats[[1]]$NORADcatalogNumber
        for (i in seq_along(sats)) {
          sats[[i]] <- c(sats[[i]], initialDateTime = sats[[i]]$dateTime)
          if (number_NORAD != sats[[i]]$NORADcatalogNumber) {
            differents <- TRUE
          }
        }
      } else {
        return(render_empty_map(input$dimension))
      }
    }

    if (input$propagationTime == "datetime") {
      shiny::req(input$targetDateSat)
      shiny::req(input$targetTimeSat)

      if (!is.null(id_notification_differents)) {
        shiny::removeNotification(id_notification_differents)
      }
      id_notification_differents <<- NULL

      target_time_sat <- substring(input$targetTimeSat, 12, 19)
      target_time_sat <-
        if (target_time_sat == "") {
          substring(sats[[1]]$dateTime, 12, 19)
        } else {
          target_time_sat
        }

      target_date <- paste(input$targetDateSat,
        target_time_sat,
        sep = " "
      )
      geodetics_matrix_and_positions_two_weeks <-
        calculate_geodetic_matrix_and_position_two_weeks(sats,
          target_date = target_date,
          method = method
        )

      geodetics_matrix <-
        geodetics_matrix_and_positions_two_weeks[[1]]
      results_position_matrix_GCRF <-
        geodetics_matrix_and_positions_two_weeks[[2]]
      results_velocity_matrix_GCRF <-
        geodetics_matrix_and_positions_two_weeks[[3]]
      orbital_elements <-
        geodetics_matrix_and_positions_two_weeks[[4]]
      positions_two_weeks <-
        geodetics_matrix_and_positions_two_weeks[[5]]
      positions_two_weeks_two_days <-
        geodetics_matrix_and_positions_two_weeks[[6]]
      is_negative <-
        geodetics_matrix_and_positions_two_weeks[[7]]
    } else if (input$propagationTime == "minutes") {
      if (differents) {
        if (!is.null(id_notification_differents)) {
          return()
        }
        id_notification_differents <<-
          shiny::showNotification(
            paste(
              "No se puede propagar en minutos al ser satélites diferentes"
            ),
            duration = 0,
            type = "warning"
          )

        return(render_empty_map(input$dimension))
      }
      if (!is.null(id_notification_differents)) {
        shiny::removeNotification(id_notification_differents)
      }
      id_notification_differents <<- NULL

      shiny::req(input$propagationTimeSat)
      geodetics_matrix_and_positions_two_weeks <-
        calculate_geodetic_matrix_and_position_two_weeks(sats,
          min = input$propagationTimeSat,
          method = method
        )

      geodetics_matrix <-
        geodetics_matrix_and_positions_two_weeks[[1]]
      results_position_matrix_GCRF <-
        geodetics_matrix_and_positions_two_weeks[[2]]
      results_velocity_matrix_GCRF <-
        geodetics_matrix_and_positions_two_weeks[[3]]
      orbital_elements <-
        geodetics_matrix_and_positions_two_weeks[[4]]
      positions_two_weeks <-
        geodetics_matrix_and_positions_two_weeks[[5]]
      positions_two_weeks_two_days <-
        geodetics_matrix_and_positions_two_weeks[[6]]
      is_negative <-
        geodetics_matrix_and_positions_two_weeks[[7]]
    }

    geo_markers <- calculate_geo_markers(geodetics_matrix)

    names <- NULL
    for (i in seq_along(sats)) {
      if (input$data == "fileSats") {
        names <- c(names, sats[[i]]$objectName)
      } else {
        names <- c(names, sats[[i]]$NORADcatalogNumber)
      }
    }

    if (length(sats) > 1) {
      for (i in seq_along(sats)) {
        local({
          my_i <- i

          output[[paste0("titleSGDP4Tab", my_i)]] <- shiny::renderText({
            names[[my_i]]
          })

          shiny::showTab("tabsetpanelSGDP4", paste0("SGDP4Tab", my_i), FALSE,
            session = shiny::getDefaultReactiveDomain()
          )

          output[[paste0("SGDP4Map", my_i)]] <- leaflet::renderLeaflet({
            render_map_satellites(
              list(geo_markers[[my_i]]),
              list(results_position_matrix_GCRF[[my_i]]),
              list(results_velocity_matrix_GCRF[[my_i]]),
              list(orbital_elements[[my_i]]),
              input$dimension,
              list(names[[my_i]]),
              list(positions_two_weeks[[my_i]]),
              list(positions_two_weeks_two_days[[my_i]]),
              type_colors,
              list(is_negative[[my_i]]),
              method
            )
          })
        })
      }
    }

    return(
      render_map_satellites(
        geo_markers,
        results_position_matrix_GCRF,
        results_velocity_matrix_GCRF,
        orbital_elements,
        input$dimension,
        names,
        positions_two_weeks,
        positions_two_weeks_two_days,
        type_colors,
        is_negative,
        method
      )
    )
  }

  render_HPOP <- function() {
    shiny::req(input$satelite)
    shiny::req(input$propagationTime)
    shiny::req(input$dimension)
    shiny::req(input$data)
    shiny::req(input$colors)

    shiny::hideTab("tabsetpanelHPOP", "HPOPTab1",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab2",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab3",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab4",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab5",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab6",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab7",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab8",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab9",
      session = shiny::getDefaultReactiveDomain()
    )
    shiny::hideTab("tabsetpanelHPOP", "HPOPTab10",
      session = shiny::getDefaultReactiveDomain()
    )

    if (!is.null(id_notification_differents)) {
      shiny::removeNotification(id_notification_differents)
    }
    id_notification_differents <<- NULL

    if (!is.null(id_notification_HPOP_neg)) {
      shiny::removeNotification(id_notification_HPOP_neg)
    }
    id_notification_HPOP_neg <<- NULL

    sats <- list()
    differents <- FALSE
    type_colors <- input$colors

    if (input$data == "selectSats") {
      sat <- test_TLEs[[strtoi(input$satelite)]]
      sat <- c(sat, initialDateTime = sat$dateTime)
      sats[[1]] <- sat
    } else {
      if (!is.null(input$TLEFile)) {
        file <- input$TLEFile
        TLE_sats <- read_file(file)
        if (!is.null(names(TLE_sats))) {
          sats[[1]] <- TLE_sats
        } else {
          sats <- TLE_sats
        }
        number_NORAD <- sats[[1]]$NORADcatalogNumber
        for (i in seq_along(sats)) {
          sats[[i]] <- c(sats[[i]], initialDateTime = sats[[i]]$dateTime)
          if (number_NORAD != sats[[i]]$NORADcatalogNumber) {
            differents <- TRUE
          }
        }
      } else {
        return(render_empty_map(input$dimension))
      }
    }

    if (input$propagationTime == "datetime") {
      shiny::req(input$targetDateSat)
      shiny::req(input$targetTimeSat)

      target_time_sat <- substring(input$targetTimeSat, 12, 19)
      target_time_sat <-
        if (target_time_sat == "") {
          substring(sats[[1]]$dateTime, 12, 19)
        } else {
          target_time_sat
        }

      target_date <- paste(input$targetDateSat,
        target_time_sat,
        sep = " "
      )

      geodetics_matrix_and_positions_two_weeks_hpop <-
        calculate_geodetic_matrix_and_position_two_weeks_hpop(sats,
          target_date = target_date
        )
    } else {
      if (differents) {
        if (!is.null(id_notification_differents)) {
          return()
        }
        id_notification_differents <<-
          shiny::showNotification(
            paste(
              "No se puede propagar en minutos al ser satélites diferentes"
            ),
            duration = 0,
            type = "warning"
          )

        return(render_empty_map(input$dimension))
      }

      if (!is.null(id_notification_differents)) {
        shiny::removeNotification(id_notification_differents)
      }
      id_notification_differents <<- NULL

      shiny::req(input$propagationTimeSat)
      geodetics_matrix_and_positions_two_weeks_hpop <-
        calculate_geodetic_matrix_and_position_two_weeks_hpop(sats,
          min = input$propagationTimeSat
        )
    }

    if (is.null(geodetics_matrix_and_positions_two_weeks_hpop)) {
      if (!is.null(id_notification_HPOP_neg)) {
        return()
      }
      id_notification_HPOP_neg <<-
        shiny::showNotification(
          paste(
            "No se puede propagar hacia atrás en el tiempo para el método HPOP"
          ),
          duration = 0,
          type = "error"
        )
    }

    geodetics_matrix_hpop <-
      geodetics_matrix_and_positions_two_weeks_hpop[[1]]
    results_position_matrix_GCRF <-
      geodetics_matrix_and_positions_two_weeks_hpop[[2]]
    results_velocity_matrix_GCRF <-
      geodetics_matrix_and_positions_two_weeks_hpop[[3]]
    orbital_elements <-
      geodetics_matrix_and_positions_two_weeks_hpop[[4]]
    positions_two_weeks_hpop <-
      geodetics_matrix_and_positions_two_weeks_hpop[[5]]
    positions_two_weeks_two_days_hpop <-
      geodetics_matrix_and_positions_two_weeks_hpop[[6]]

    geo_markers <- calculate_geo_markers(geodetics_matrix_hpop)

    names <- NULL
    for (i in seq_along(sats)) {
      if (input$data == "fileSats") {
        names <- c(names, sats[[i]]$objectName)
      } else {
        names <- c(names, sats[[i]]$NORADcatalogNumber)
      }
    }

    if (length(sats) > 1) {
      for (i in seq_along(sats)) {
        local({
          my_i <- i

          output[[paste0("titleHPOPTab", my_i)]] <- shiny::renderText({
            names[[my_i]]
          })

          shiny::showTab("tabsetpanelHPOP", paste0("HPOPTab", my_i), FALSE,
            session = shiny::getDefaultReactiveDomain()
          )

          output[[paste0("HPOPMap", my_i)]] <- leaflet::renderLeaflet({
            render_map_satellites(
              list(geo_markers[[my_i]]),
              list(results_position_matrix_GCRF[[my_i]]),
              list(results_velocity_matrix_GCRF[[my_i]]),
              list(orbital_elements[[my_i]]),
              input$dimension,
              list(names[[my_i]]),
              list(positions_two_weeks_hpop[[my_i]]),
              list(positions_two_weeks_two_days_hpop[[my_i]]),
              type_colors,
              method = "HPOP"
            )
          })
        })
      }
    }

    render_map_satellites(
      geo_markers,
      results_position_matrix_GCRF,
      results_velocity_matrix_GCRF,
      orbital_elements,
      input$dimension,
      names,
      positions_two_weeks_hpop,
      positions_two_weeks_two_days_hpop,
      type_colors,
      method = "HPOP"
    )
  }

  render_simulator <- function() {
    shiny::req(input$inclination)
    shiny::req(input$ascension)
    shiny::req(input$eccentricity)
    shiny::req(input$perigeeArgument)
    shiny::req(input$meanAnomaly)
    shiny::req(input$meanMotion)
    shiny::req(input$Bstar)
    shiny::req(input$propagationTimeSimulator)
    shiny::req(input$dimension)

    geodetics_matrix <- NULL
    results_position_matrix_GCRF <- NULL
    results_velocity_matrix_GCRF <- NULL
    orbital_elements <- NULL

    if (!is.null(id_notification_differents)) {
      shiny::removeNotification(id_notification_differents)
    }
    id_notification_differents <<- NULL

    if (!is.null(id_notification_file)) {
      shiny::removeNotification(id_notification_file)
    }
    id_notification_file <<- NULL

    if (!is.null(id_notification_HPOP_neg)) {
      shiny::removeNotification(id_notification_HPOP_neg)
    }
    id_notification_HPOP_neg <<- NULL

    sat <- list(
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

    sats <- list()
    sats[[1]] <- sat

    if (input$propagationTimeSimulator == "datetime") {
      shiny::req(input$targetDateSimulator)
      shiny::req(input$targetTimeSimulator)

      target_time_sat <-
        substring(input$targetTimeSimulator, 12, 19)
      target_time_sat <-
        if (target_time_sat == "") {
          substring(sat$initialDateTime, 12, 19)
        } else {
          target_time_sat
        }

      geodetics_matrix_and_positions_two_weeks <-
        calculate_geodetic_matrix_and_position_two_weeks(sats,
          target_date = paste(input$targetDateSimulator,
            target_time_sat,
            sep = " "
          )
        )

      geodetics_matrix <-
        geodetics_matrix_and_positions_two_weeks[[1]]
      results_position_matrix_GCRF <-
        geodetics_matrix_and_positions_two_weeks[[2]]
      results_velocity_matrix_GCRF <-
        geodetics_matrix_and_positions_two_weeks[[3]]
      orbital_elements <-
        geodetics_matrix_and_positions_two_weeks[[4]]
      positions_two_weeks <-
        geodetics_matrix_and_positions_two_weeks[[5]]
    } else if (input$propagationTimeSimulator == "minutes") {
      shiny::req(input$propagationTimeSatSimulator)
      geodetics_matrix_and_positions_two_weeks <-
        calculate_geodetic_matrix_and_position_two_weeks(sats,
          min = input$propagationTimeSatSimulator
        )

      geodetics_matrix <-
        geodetics_matrix_and_positions_two_weeks[[1]]
      results_position_matrix_GCRF <-
        geodetics_matrix_and_positions_two_weeks[[2]]
      results_velocity_matrix_GCRF <-
        geodetics_matrix_and_positions_two_weeks[[3]]
      orbital_elements <-
        geodetics_matrix_and_positions_two_weeks[[4]]
      positions_two_weeks <-
        geodetics_matrix_and_positions_two_weeks[[5]]
    }
    geo_markers <- calculate_geo_markers(geodetics_matrix)

    render_map_satellites(
      geo_markers,
      results_position_matrix_GCRF,
      results_velocity_matrix_GCRF,
      orbital_elements,
      dimension = input$dimension,
      positions_two_weeks = positions_two_weeks,
      method = "Simulador"
    )
  }

  tabs_data <- shiny::eventReactive(input$generate, {
    if (input$metodos == "SGDP4") {
      render_SGDP4()
    } else if (input$metodos == "HPOP") {
      render_HPOP()
    } else if (input$metodos == "Elementos orbitales keplerianos") {
      render_plots()
    }
  })

  map_data_simulator <- shiny::eventReactive(input$generateSimulator, {
    render_simulator()
  })

  render_plots <- function() {
    shiny::req(input$satelite)
    shiny::req(input$propagationTime)
    shiny::req(input$dimension)
    shiny::req(input$data)

    sats <- list()
    differents <- FALSE

    orbital_elements <- NULL

    if (!is.null(id_notification_HPOP_neg)) {
      shiny::removeNotification(id_notification_HPOP_neg)
    }
    id_notification_HPOP_neg <<- NULL

    if (input$data == "selectSats") {
      sat <- test_TLEs[[strtoi(input$satelite)]]
      sat <- c(sat, initialDateTime = sat$dateTime)
      sats[[1]] <- sat
    } else {
      if (!is.null(input$TLEFile)) {
        file <- input$TLEFile
        TLE_sats <- read_file(file)
        if (!is.null(names(TLE_sats))) {
          sats[[1]] <- TLE_sats
        } else {
          sats <- TLE_sats
        }
        number_NORAD <- sats[[1]]$NORADcatalogNumber
        for (i in seq_along(sats)) {
          sats[[i]] <- c(sats[[i]], initialDateTime = sats[[i]]$dateTime)
          if (number_NORAD != sats[[i]]$NORADcatalogNumber) {
            differents <- TRUE
          }
        }
      } else {
        return(orbital_elements)
      }
    }

    if (input$propagationTime == "datetime") {
      shiny::req(input$targetDateSat)
      shiny::req(input$targetTimeSat)

      if (!is.null(id_notification_differents)) {
        shiny::removeNotification(id_notification_differents)
      }
      id_notification_differents <<- NULL

      target_time_sat <- substring(input$targetTimeSat, 12, 19)
      target_time_sat <-
        if (target_time_sat == "") {
          substring(sats[[1]]$dateTime, 12, 19)
        } else {
          target_time_sat
        }

      target_date <- paste(input$targetDateSat,
        target_time_sat,
        sep = " "
      )
      geodetics_matrix_and_positions_two_weeks <-
        calculate_geodetic_matrix_and_position_two_weeks(sats,
          target_date = target_date
        )

      orbital_elements <-
        geodetics_matrix_and_positions_two_weeks[[4]]
    } else if (input$propagationTime == "minutes") {
      if (differents) {
        if (!is.null(id_notification_differents)) {
          return()
        }
        id_notification_differents <<-
          shiny::showNotification(
            paste(
              "No se puede propagar en minutos al ser satélites diferentes"
            ),
            duration = 0,
            type = "warning"
          )

        return(orbital_elements)
      }
      if (!is.null(id_notification_differents)) {
        shiny::removeNotification(id_notification_differents)
      }
      id_notification_differents <<- NULL

      shiny::req(input$propagationTimeSat)
      geodetics_matrix_and_positions_two_weeks <-
        calculate_geodetic_matrix_and_position_two_weeks(sats,
          min = input$propagationTimeSat
        )
      orbital_elements <-
        geodetics_matrix_and_positions_two_weeks[[4]]
    }

    names <- NULL
    for (i in seq_along(sats)) {
      if (input$data == "fileSats") {
        names <- c(names, sats[[i]]$objectName)
      } else {
        names <- c(names, sats[[i]]$NORADcatalogNumber)
      }
    }

    return(list(orbital_elements, names))
  }

  output$plot1 <- plotly::renderPlotly({
    if (!("leaflet" %in% class(tabs_data()))) {
      orbital_elements <- tabs_data()[[1]]
      sats <- tabs_data()[[2]]

      p <- plotly::plot_ly() |>
        plotly::layout(
          legend = list(orientation = "h", y = -0.2),
          xaxis = list(title = "Tiempo (mins)"),
          yaxis = list(title = "Excentricidad (rads)"),
          title = "Excentricidad media"
        )

      for (i in seq_along(orbital_elements)) {
        x <- as.numeric(orbital_elements[[i]][, 6])
        y <- as.numeric(orbital_elements[[i]][, 1])
        data <- data.frame(x, y)
        p <- p |> plotly::add_trace(
          data = data, x = ~x, y = ~y,
          type = "scatter", mode = "lines", name = sats[[i]]
        )
      }
      p
    }
  })

  output$plot2 <- plotly::renderPlotly({
    if (!("leaflet" %in% class(tabs_data()))) {
      orbital_elements <- tabs_data()[[1]]
      sats <- tabs_data()[[2]]

      p <- plotly::plot_ly() |>
        plotly::layout(
          legend = list(orientation = "h", y = -0.2),
          xaxis = list(title = "Tiempo (mins)"),
          yaxis = list(title = "Inclinación (rads)"),
          title = "Inclinación media"
        )

      for (i in seq_along(orbital_elements)) {
        x <- as.numeric(orbital_elements[[i]][, 6])
        y <- as.numeric(orbital_elements[[i]][, 2])
        data <- data.frame(x, y)
        p <- p |> plotly::add_trace(
          data = data, x = ~x, y = ~y,
          type = "scatter", mode = "lines", name = sats[[i]]
        )
      }
      p
    }
  })

  output$plot3 <- plotly::renderPlotly({
    if (!("leaflet" %in% class(tabs_data()))) {
      orbital_elements <- tabs_data()[[1]]
      sats <- tabs_data()[[2]]

      p <- plotly::plot_ly() |>
        plotly::layout(
          legend = list(orientation = "h", y = -0.2),
          xaxis = list(title = "Tiempo (mins)"),
          yaxis = list(title = "Anomalía (rads)"),
          title = "Anomalía media"
        )

      for (i in seq_along(orbital_elements)) {
        x <- as.numeric(orbital_elements[[i]][, 6])
        y <- as.numeric(orbital_elements[[i]][, 3])
        data <- data.frame(x, y)
        p <- p |> plotly::add_trace(
          data = data, x = ~x, y = ~y,
          type = "scatter", mode = "lines", name = sats[[i]]
        )
      }
      p
    }
  })

  output$plot4 <- plotly::renderPlotly({
    if (!("leaflet" %in% class(tabs_data()))) {
      orbital_elements <- tabs_data()[[1]]
      sats <- tabs_data()[[2]]

      p <- plotly::plot_ly() |>
        plotly::layout(
          legend = list(orientation = "h", y = -0.2),
          xaxis = list(title = "Tiempo (mins)"),
          yaxis = list(title = "Argumento del perigeo (rads)"),
          title = "Argumento del perigeo medio"
        )

      for (i in seq_along(orbital_elements)) {
        x <- as.numeric(orbital_elements[[i]][, 6])
        y <- as.numeric(orbital_elements[[i]][, 4])
        data <- data.frame(x, y)
        p <- p |> plotly::add_trace(
          data = data, x = ~x, y = ~y,
          type = "scatter", mode = "lines", name = sats[[i]]
        )
      }
      p
    }
  })

  output$plot5 <- plotly::renderPlotly({
    if (!("leaflet" %in% class(tabs_data()))) {
      orbital_elements <- tabs_data()[[1]]
      sats <- tabs_data()[[2]]

      p <- plotly::plot_ly() |>
        plotly::layout(
          legend = list(orientation = "h", y = -0.2),
          xaxis = list(title = "Tiempo (mins)"),
          yaxis = list(title = "Ascención recta (rads)"),
          title = "Ascención recta media"
        )

      for (i in seq_along(orbital_elements)) {
        x <- as.numeric(orbital_elements[[i]][, 6])
        y <- as.numeric(orbital_elements[[i]][, 5])
        data <- data.frame(x, y)
        p <- p |> plotly::add_trace(
          data = data, x = ~x, y = ~y,
          type = "scatter", mode = "lines", name = sats[[i]]
        )
      }
      p
    }
  })
}
