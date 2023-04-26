source("auxiliarFunctions.R", local = TRUE)

server <- function(input, output, session) {
  # observeEvent(input$get_latest_space_data, {
  #   output$progress <- renderText("Actualizando los datos...")
  # })
  
  proceso_en_ejecucion <- reactiveValues(estado = NULL)
  
  observeEvent(input$get_latest_space_data, {
    # Cambiar el estado a "En ejecución" cuando se hace clic en el botón
    proceso_en_ejecucion$estado <- "En ejecución"
    flush.console()
    # Ejecutar el proceso de larga duración en un hilo separado
    # isolate({
      # getLatestSpaceData()
      Sys.sleep(5)
      # Cambiar el estado a "Completado" cuando el proceso ha terminado
      proceso_en_ejecucion$estado <- "Completado"
    # })
  })
  
  output$output <- renderPrint({
    if(is.null(proceso_en_ejecucion$estado)){
      "El proceso no ha comenzado todavía"
    } else if(proceso_en_ejecucion$estado == "En ejecución"){
      "El proceso está en ejecución..."
    } else {
      "El proceso ha terminado"
    }
  })

  test_TLEs <-
    readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))
  idNotificationHPOP <- NULL
  idNotificationDifferents <- NULL
  idNotificationFile <- NULL

  get_more_recent_date <- function() {
    req(input$TLEFile)

    file <- input$TLEFile
    sats <- readTLE(filename = file$datapath)
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

  observe({
    if (!is.null(input$satelite)) {
      if (input$data == "selectSats") {
        initial_datetime_sat <- test_TLEs[[strtoi(input$satelite)]]$dateTime
        initial_date_sat <- substr(initial_datetime_sat, 1, 10)

        updateDateInput(
          session,
          "targetDateSat",
          value = as.Date(initial_date_sat) + 1,
          min = initial_date_sat
        )
        updateTimeInput(session, "targetTimeSat", value = Sys.time())

        if (!is.null(idNotificationFile)) {
          removeNotification(idNotificationFile)
        }
        idNotificationFile <<- NULL
      } else {
        if (!is.null(input$TLEFile)) {
          initial_datetime_sat <- get_more_recent_date()
          initial_date_sat <- substr(initial_datetime_sat, 1, 10)
          updateDateInput(
            session,
            "targetDateSat",
            value = as.Date(initial_date_sat) + 1,
            min = initial_date_sat
          )
          updateTimeInput(session, "targetTimeSat", value = Sys.time())

          if (!is.null(idNotificationFile)) {
            removeNotification(idNotificationFile)
          }
          idNotificationFile <<- NULL
        } else {
          if (!is.null(idNotificationFile)) {
            return()
          }
          idNotificationFile <<- showNotification(paste("Debe subir un fichero
                                                        para visualizar"), duration = 0, type = "warning")
        }
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
    selectInput("satelite", p("Satélite"), choices = get_satelites())
  })


  output$initialDateSat <-
    renderText({
      req(input$satelite)
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

  output$firstMap <- renderLeaflet({
    req(input$satelite)
    req(input$propagationTime)
    req(input$dimension)
    req(input$data)

    if (!is.null(idNotificationHPOP)) {
      removeNotification(idNotificationHPOP)
    }
    idNotificationHPOP <<- NULL

    updateNumericInput(session, "propagationTimeSat", min = 0)

    sats <- list()
    differents <- FALSE

    if (input$data == "selectSats") {
      sat <- test_TLEs[[strtoi(input$satelite)]]
      sat <- c(sat, initialDateTime = sat$dateTime)
      sats[[1]] <- sat
    } else {
      if (!is.null(input$TLEFile)) {
        file <- input$TLEFile
        TLE_sats <- readTLE(filename = file$datapath)
        if (!is.null(names(TLE_sats))) {
          sats[[1]] <- TLE_sats
        } else {
          sats <- TLE_sats
        }
        numberNORAD <- sats[[1]]$NORADcatalogNumber
        for (i in seq_along(sats)) {
          sats[[i]] <- c(sats[[i]], initialDateTime = sats[[i]]$dateTime)
          if (numberNORAD != sats[[i]]$NORADcatalogNumber) {
            differents <- TRUE
          }
        }
      } else {
        return(render_empty_map(input$dimension))
      }
    }

    if (input$propagationTime == "datetime") {
      req(input$targetDateSat)
      req(input$targetTimeSat)

      if (!is.null(idNotificationDifferents)) {
        removeNotification(idNotificationDifferents)
      }
      idNotificationDifferents <<- NULL

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
        calculate_geodetic_matrix_and_position_two_weeks(
          sats,
          target_date = target_date
        )

      geodetics_matrix <- geodetics_matrix_and_positions_two_weeks[[1]]
      positions_two_weeks <- geodetics_matrix_and_positions_two_weeks[[2]]
    } else if (input$propagationTime == "minutes") {
      if (differents) {
        if (!is.null(idNotificationDifferents)) {
          return()
        }
        idNotificationDifferents <<- showNotification(paste("No se puede propagar
                                                            en minutos al ser satélites diferentes"), duration = 0, type = "warning")

        return(render_empty_map(input$dimension))
      }
      if (!is.null(idNotificationDifferents)) {
        removeNotification(idNotificationDifferents)
      }
      idNotificationDifferents <<- NULL

      req(input$propagationTimeSat)
      geodetics_matrix_and_positions_two_weeks <-
        calculate_geodetic_matrix_and_position_two_weeks(
          sats,
          min = input$propagationTimeSat
        )

      geodetics_matrix <- geodetics_matrix_and_positions_two_weeks[[1]]
      positions_two_weeks <- geodetics_matrix_and_positions_two_weeks[[2]]
    }

    geo_markers <- calculate_geo_markers(geodetics_matrix)
    # geo_polylines <- calculate_geo_polylines(geo_markers)


    names <- NULL
    for (i in seq_along(sats)) {
      names <- c(names, sats[[i]]$NORADcatalogNumber)
    }

    render_map_satellites(geo_markers, input$dimension, names, positions_two_weeks)
  })


  output$hpopOutput <- renderLeaflet({
    req(input$satelite)
    req(input$propagationTime)
    req(input$dimension)
    req(input$data)

    # disable("targetDateSat")
    # disable("targetTimeSat")

    if (!is.null(idNotificationDifferents)) {
      removeNotification(idNotificationDifferents)
    }
    idNotificationDifferents <<- NULL

    sats <- list()
    differents <- FALSE

    if (input$data == "selectSats") {
      sat <- test_TLEs[[strtoi(input$satelite)]]
      sat <- c(sat, initialDateTime = sat$dateTime)
      sats[[1]] <- sat
    } else {
      if (!is.null(input$TLEFile)) {
        file <- input$TLEFile
        TLE_sats <- readTLE(filename = file$datapath)
        if (!is.null(names(TLE_sats))) {
          sats[[1]] <- TLE_sats
        } else {
          sats <- TLE_sats
        }
        numberNORAD <- sats[[1]]$NORADcatalogNumber
        for (i in seq_along(sats)) {
          sats[[i]] <- c(sats[[i]], initialDateTime = sats[[i]]$dateTime)
          if (numberNORAD != sats[[i]]$NORADcatalogNumber) {
            differents <- TRUE
          }
        }
      } else {
        return(render_empty_map(input$dimension))
      }
    }

    if (input$propagationTime == "datetime") {
      if (!is.null(idNotificationHPOP)) {
        return()
      }
      idNotificationHPOP <<- showNotification(paste("No se puede propagar en fechas en el método
                                    HPOP"), duration = 0, type = "warning")

      return(render_empty_map(input$dimension))
    }

    if (differents) {
      if (!is.null(idNotificationDifferents)) {
        return()
      }
      idNotificationDifferents <<- showNotification(paste("No se puede propagar
                                                            en minutos al ser satélites diferentes"), duration = 0, type = "warning")

      return(render_empty_map(input$dimension))
    }

    if (!is.null(idNotificationDifferents)) {
      removeNotification(idNotificationDifferents)
    }
    idNotificationDifferents <<- NULL

    updateNumericInput(session, "propagationTimeSat", min = 1)

    if (!is.null(idNotificationHPOP)) {
      removeNotification(idNotificationHPOP)
    }
    idNotificationHPOP <<- NULL

    req(input$propagationTimeSat)
    geodetics_matrix_and_positions_two_weeks_hpop <-
      calculate_geodetic_matrix_and_position_two_weeks_hpop(
        sats,
        min = input$propagationTimeSat
      )

    geodetics_matrix_hpop <- geodetics_matrix_and_positions_two_weeks_hpop[[1]]
    positions_two_weeks_hpop <- geodetics_matrix_and_positions_two_weeks_hpop[[2]]

    geo_markers <- calculate_geo_markers(geodetics_matrix_hpop)
    # geo_polylines <- calculate_geo_polylines(geo_markers)

    names <- NULL
    for (i in seq_along(sats)) {
      names <- c(names, sats[[i]]$NORADcatalogNumber)
    }

    render_map_satellites(
      geo_markers, input$dimension,
      names, positions_two_weeks_hpop
    )
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

    if (!is.null(idNotificationHPOP)) {
      removeNotification(idNotificationHPOP)
    }
    idNotificationHPOP <<- NULL

    if (!is.null(idNotificationDifferents)) {
      removeNotification(idNotificationDifferents)
    }
    idNotificationDifferents <<- NULL

    if (!is.null(idNotificationFile)) {
      removeNotification(idNotificationFile)
    }
    idNotificationFile <<- NULL

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
      req(input$targetDateSimulator)
      req(input$targetTimeSimulator)

      target_time_sat <- substring(input$targetTimeSimulator, 12, 19)
      target_time_sat <-
        if (target_time_sat == "") {
          substring(sat$initialDateTime, 12, 19)
        } else {
          target_time_sat
        }

      geodetics_matrix_and_positions_two_weeks <-
        calculate_geodetic_matrix_and_position_two_weeks(
          sats,
          target_date = paste(input$targetDateSimulator,
            target_time_sat,
            sep = " "
          )
        )

      geodetics_matrix <- geodetics_matrix_and_positions_two_weeks[[1]]
      positions_two_weeks <- geodetics_matrix_and_positions_two_weeks[[2]]
    } else if (input$propagationTimeSimulator == "minutes") {
      req(input$propagationTimeSatSimulator)
      geodetics_matrix_and_positions_two_weeks <-
        calculate_geodetic_matrix_and_position_two_weeks(
          sats,
          min = input$propagationTimeSatSimulator
        )

      geodetics_matrix <- geodetics_matrix_and_positions_two_weeks[[1]]
      positions_two_weeks <- geodetics_matrix_and_positions_two_weeks[[2]]
    }

    geo_markers <- calculate_geo_markers(geodetics_matrix)
    # geo_polylines <- calculate_geo_polylines(geo_markers)

    render_map_satellites(geo_markers, input$dimension, positions_two_weeks = positions_two_weeks)
  })
}
