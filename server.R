source("auxiliarFunctions.R", local = TRUE)

server <- function(input, output, session) {
  # Inicialización de variables
  test_tles <-
    asteRisk::readTLE("www/testTLE.txt")
  id_notification_differents <- NULL
  id_notification_file <- NULL
  id_notification_hpop_neg <- NULL
  id_notification_error <- NULL

  # Actualización de datos mediante el botón get_latest_space_data
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

  # Método para obtener los satélites desde el fichero TLE
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

    has_na <- any(sapply(sats, function(x) any(is.na(x))))
    if (has_na) {
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

  # Inicializa los tiempos de propagación
  shiny::observe({
    if (!is.null(input$satelite)) {
      initial_datetime_sat <- NULL
      initial_date_sat <- NULL
      if (input$data == "selectSats") {
        initial_datetime_sat <- test_tles[[strtoi(input$satelite)]]$dateTime
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
          sats <- read_file(input$TLEFile)
          initial_datetime_sat <- get_more_recent_date(sats)
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
              paste("Debe subir un fichero para visualizar"),
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

  # Oculta las pestañas de los mapas individuales
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

  # Actualiza los tiempos de propagación en el simulador
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

  # Devuelve los satélites disponibles para el selector
  output$select_satelite <- shiny::renderUI({
    shiny::selectInput("satelite", p("Satélite"), choices = get_satelites())
  })

  # Inicializa la fecha inicial del satélite más reciente
  output$initialDateSat <- shiny::renderText({
      if (input$data == "selectSats") {
        test_tles[[strtoi(input$satelite)]]$dateTime
      } else {
        if (!is.null(input$TLEFile)) {
          sats <- read_file(input$TLEFile)
          get_more_recent_date(sats)
        } else {
          "Sube tu fichero TLE"
        }
      }
  })

  # Guarda en la variable "metodos", la pestaña seleccionada en el menú
  shiny::observeEvent(input$metodos, {
    shiny::updateTabsetPanel(session, "metodos", selected = input$metodos)
  })

  # Renderiza el mapa SGDP4
  output$SGDP4Map <- leaflet::renderLeaflet({
    if (!is.null(tabs_data())) {
      tabs_data()
    }
  })

  # Renderiza el mapa HPOP
  output$HPOPMap <- leaflet::renderLeaflet({
    if (!is.null(tabs_data())) {
      tabs_data()
    }
  })

  # Según la pestaña seleccionada, renderiza el mapa o los
  # gráficos correspondientes cuando se pulsa el botón "Generar"
  tabs_data <- shiny::eventReactive(input$generate, {
    if (input$metodos == "SGDP4") {
      render_sgdp4()
    } else if (input$metodos == "HPOP") {
      render_hpop()
    } else if (input$metodos == "Elementos orbitales keplerianos") {
      calculate_plots()
    }
  })

  # Renderiza el mapa del simulador
  output$myMap <- leaflet::renderLeaflet({
    if (!is.null(map_data_simulator())) {
      map_data_simulator()
    }
  })

  # Renderiza el mapa del simulador cuando se pulsa el botón "Generar mapa"
  map_data_simulator <- shiny::eventReactive(input$generateSimulator, {
    render_simulator()
  })

  # Genera el mapa de la pestaña SGDP4
  render_sgdp4 <- function() {
    shiny::req(input$satelite)
    shiny::req(input$propagationTime)
    shiny::req(input$dimension)
    shiny::req(input$data)
    shiny::req(input$method)
    shiny::req(input$colors)

    # Oculta las pestañas de los mapas individuales
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

    # Inicializa las variables
    sats <- list()
    differents <- FALSE
    method <- input$method
    type_colors <- input$colors
    is_negative <- FALSE

    results_position_matrix_gcrf <- NULL
    results_velocity_matrix_gcrf <- NULL
    orbital_elements <- NULL

    # Elimina las notificaciones por HPOP
    if (!is.null(id_notification_hpop_neg)) {
      shiny::removeNotification(id_notification_hpop_neg)
    }
    id_notification_hpop_neg <<- NULL

    if (input$data == "selectSats") {
      # Si se selecciona un satélite, se obtiene su TLE
      sat <- test_tles[[strtoi(input$satelite)]]
      sat <- c(sat, initialDateTime = sat$dateTime)
      sats[[1]] <- sat
    } else {
      if (!is.null(input$TLEFile)) {
        # Si se sube un fichero, se obtienen los TLEs
        file <- input$TLEFile
        TLE_sats <- read_file(file)
        if (!is.null(names(TLE_sats))) {
          sats[[1]] <- TLE_sats
        } else {
          sats <- TLE_sats
        }
        number_norad <- sats[[1]]$NORADcatalogNumber
        for (i in seq_along(sats)) {
          sats[[i]] <- c(sats[[i]], initialDateTime = sats[[i]]$dateTime)
          if (number_norad != sats[[i]]$NORADcatalogNumber) {
            differents <- TRUE
          }
        }
      } else {
        # Si no se sube un fichero, se muestra un mapa vacío
        return(render_empty_map(input$dimension))
      }
    }

    if (input$propagationTime == "datetime") {
      shiny::req(input$targetDateSat)
      shiny::req(input$targetTimeSat)

      # Elimina las notificaciones por satélites diferentes
      if (!is.null(id_notification_differents)) {
        shiny::removeNotification(id_notification_differents)
      }
      id_notification_differents <<- NULL

      # Si se selecciona propagar por fecha y hora absolutas,
      # se obtiene la fecha y hora seleccionadas de destino
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

      # Se propagan los satélites
      data <- calculate_data(sats, target_date = target_date, method = method)
    } else if (input$propagationTime == "minutes") {
      if (differents) {
        # Si se suben TLEs de satélites diferentes, se muestra un mensaje
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
      # Si se selecciona propagar por minutos relativos al epoch,
      # se obtienen los minutos seleccionados y se propagan los satélites
      data <- calculate_data(sats, min = input$propagationTimeSat,
                              method = method)
    }
    
    # Se obtienen los datos de la propagación
    geodetics_matrix <- data[[1]] # Matriz geodésica
    results_position_matrix_gcrf <- data[[2]] # Matriz de posiciones
    results_velocity_matrix_gcrf <- data[[3]] # Matriz de velocidades
    orbital_elements <- data[[4]] # Elementos orbitales
    positions_two_weeks <- data[[5]] # Punto de la trayectoria a dos semanas
    positions_two_weeks_two_days <- data[[6]] # Punto de la trayectoria
                                              # a dos semanas y dos días
    is_negative <- data[[7]] # Indica si la propagación es hacia atrás

    # Se calculan los marcadores para el mapa a través de la matriz geodésica
    geo_markers <- calculate_geo_markers(geodetics_matrix)

    # Se obtienen los nombres de los satélites para la leyenda
    names <- NULL
    for (i in seq_along(sats)) {
      if (input$data == "fileSats") {
        names <- c(names, sats[[i]]$objectName)
      } else {
        names <- c(names, sats[[i]]$NORADcatalogNumber)
      }
    }

    if (length(sats) > 1) {
      # Si se seleccionan varios satélites, se renderizan los mapas individuales
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
              list(results_position_matrix_gcrf[[my_i]]),
              list(results_velocity_matrix_gcrf[[my_i]]),
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

    # Se renderiza el mapa
    return(
      render_map_satellites(
        geo_markers,
        results_position_matrix_gcrf,
        results_velocity_matrix_gcrf,
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

  # Genera el mapa de la pestaña HPOP
  render_hpop <- function() {
    shiny::req(input$satelite)
    shiny::req(input$propagationTime)
    shiny::req(input$dimension)
    shiny::req(input$data)
    shiny::req(input$colors)

    # Oculta las pestañas de los mapas individuales
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

    # Elimina las notificaciones por satélites diferentes
    if (!is.null(id_notification_differents)) {
      shiny::removeNotification(id_notification_differents)
    }
    id_notification_differents <<- NULL

    # Elimina las notificaciones por HPOP
    if (!is.null(id_notification_hpop_neg)) {
      shiny::removeNotification(id_notification_hpop_neg)
    }
    id_notification_hpop_neg <<- NULL

    # Inicializa las variables
    sats <- list()
    differents <- FALSE
    type_colors <- input$colors

    if (input$data == "selectSats") {
      # Si se selecciona un satélite, se obtiene su TLE
      sat <- test_tles[[strtoi(input$satelite)]]
      sat <- c(sat, initialDateTime = sat$dateTime)
      sats[[1]] <- sat
    } else {
      if (!is.null(input$TLEFile)) {
        # Si se sube un fichero, se obtienen los TLEs
        file <- input$TLEFile
        TLE_sats <- read_file(file)
        if (!is.null(names(TLE_sats))) {
          sats[[1]] <- TLE_sats
        } else {
          sats <- TLE_sats
        }
        number_norad <- sats[[1]]$NORADcatalogNumber
        for (i in seq_along(sats)) {
          sats[[i]] <- c(sats[[i]], initialDateTime = sats[[i]]$dateTime)
          if (number_norad != sats[[i]]$NORADcatalogNumber) {
            differents <- TRUE
          }
        }
      } else {
        # Si no se sube un fichero, se muestra un mapa vacío
        return(render_empty_map(input$dimension))
      }
    }

    if (input$propagationTime == "datetime") {
      shiny::req(input$targetDateSat)
      shiny::req(input$targetTimeSat)

      # Si se selecciona propagar por fecha y hora absolutas,
      # se obtiene la fecha y hora seleccionadas de destino
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

      # Se propagan los satélites
      data_hpop <- calculate_data_hpop(sats, target_date = target_date)
    } else {
      if (differents) {
        # Si se suben TLEs de satélites diferentes, se muestra un mensaje
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

      # Si se selecciona propagar por minutos relativos al epoch,
      # se obtienen los minutos seleccionados y se propagan los satélites
      data_hpop <- calculate_data_hpop(sats, min = input$propagationTimeSat)
    }

    if (is.null(data_hpop)) {
      # Si se selecciona propagar hacia atrás en el tiempo, se muestra un mensaje
      if (!is.null(id_notification_hpop_neg)) {
        return()
      }
      id_notification_hpop_neg <<-
        shiny::showNotification(
          paste(
            "No se puede propagar hacia atrás en el tiempo para el método HPOP"
          ),
          duration = 0,
          type = "error"
        )
    }

    # Se obtienen los datos de la propagación
    geodetics_matrix_hpop <- data_hpop[[1]] # Matriz geodésica
    results_position_matrix_gcrf <- data_hpop[[2]] # Matriz de posiciones
    results_velocity_matrix_gcrf <- data_hpop[[3]] # Matriz de velocidades
    orbital_elements <- data_hpop[[4]] # Elementos orbitales
    positions_two_weeks_hpop <- data_hpop[[5]] # Punto de la trayectoria
                                               # a dos semanas
    positions_two_weeks_two_days_hpop <- data_hpop[[6]] # Punto de la trayectoria
                                                        # a dos semanas y dos días

    # Se calculan los marcadores para el mapa a través de la matriz geodésica
    geo_markers <- calculate_geo_markers(geodetics_matrix_hpop)

    # Se obtienen los nombres de los satélites para la leyenda
    names <- NULL
    for (i in seq_along(sats)) {
      if (input$data == "fileSats") {
        names <- c(names, sats[[i]]$objectName)
      } else {
        names <- c(names, sats[[i]]$NORADcatalogNumber)
      }
    }

    if (length(sats) > 1) {
      # Si se seleccionan varios satélites, se renderizan los mapas individuales
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
              list(results_position_matrix_gcrf[[my_i]]),
              list(results_velocity_matrix_gcrf[[my_i]]),
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

    # Se renderiza el mapa
    render_map_satellites(
      geo_markers,
      results_position_matrix_gcrf,
      results_velocity_matrix_gcrf,
      orbital_elements,
      input$dimension,
      names,
      positions_two_weeks_hpop,
      positions_two_weeks_two_days_hpop,
      type_colors,
      method = "HPOP"
    )
  }

  # Genera el mapa del simulador
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

    # Inicializa las variables
    geodetics_matrix <- NULL
    results_position_matrix_gcrf <- NULL
    results_velocity_matrix_gcrf <- NULL
    orbital_elements <- NULL

    # Elimina las notificaciones por satélites diferentes
    if (!is.null(id_notification_differents)) {
      shiny::removeNotification(id_notification_differents)
    }
    id_notification_differents <<- NULL

    # Elimina las notificaciones por fallo en el fichero
    if (!is.null(id_notification_file)) {
      shiny::removeNotification(id_notification_file)
    }
    id_notification_file <<- NULL

    # Elimina las notificaciones por HPOP
    if (!is.null(id_notification_hpop_neg)) {
      shiny::removeNotification(id_notification_hpop_neg)
    }
    id_notification_hpop_neg <<- NULL

    # Se obtienen los datos del satélite
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

      # Si se selecciona propagar por fecha y hora absolutas,
      # se obtiene la fecha y hora seleccionadas de destino
      target_time_sat <-
        substring(input$targetTimeSimulator, 12, 19)
      target_time_sat <-
        if (target_time_sat == "") {
          substring(sat$initialDateTime, 12, 19)
        } else {
          target_time_sat
        }

      # Se propaga el satélite
      data <- calculate_data(sats,
          target_date = paste(input$targetDateSimulator,
            target_time_sat,
            sep = " ")
        )
    } else if (input$propagationTimeSimulator == "minutes") {
      shiny::req(input$propagationTimeSatSimulator)
      data <- calculate_data(sats, min = input$propagationTimeSatSimulator)
    }

    # Se obtienen los datos de la propagación
    geodetics_matrix <- data[[1]] # Matriz geodésica
    results_position_matrix_gcrf <- data[[2]] # Matriz de posiciones
    results_velocity_matrix_gcrf <- data[[3]] # Matriz de velocidades
    orbital_elements <- data[[4]] # Elementos orbitales
    positions_two_weeks <- data[[5]] # Punto de la trayectoria a dos semanas
    positions_two_weeks_two_days <- data[[6]] # Punto de la trayectoria
                                              # a dos semanas y dos días
    is_negative <- data[[7]] # Indica si la propagación es hacia atrás

    # Se calculan los marcadores para el mapa a través de la matriz geodésica
    geo_markers <- calculate_geo_markers(geodetics_matrix)

    # Se renderiza el mapa
    render_map_satellites(
      geo_markers,
      results_position_matrix_gcrf,
      results_velocity_matrix_gcrf,
      orbital_elements,
      dimension = input$dimension,
      positions_two_weeks = positions_two_weeks,
      positions_two_weeks_two_days = positions_two_weeks_two_days,
      is_negative = is_negative,
      method = "Simulador"
    )
  }

  # Calcula los elementos orbitales keplerianos para los gráficos
  calculate_plots <- function() {
    shiny::req(input$satelite)
    shiny::req(input$propagationTime)
    shiny::req(input$dimension)
    shiny::req(input$data)

    # Inicializa las variables
    sats <- list()
    differents <- FALSE
    orbital_elements <- NULL

    # Elimina las notificaciones por HPOP
    if (!is.null(id_notification_hpop_neg)) {
      shiny::removeNotification(id_notification_hpop_neg)
    }
    id_notification_hpop_neg <<- NULL

    # Elimina las notificaciones por satélites diferentes
    if (!is.null(id_notification_differents)) {
      shiny::removeNotification(id_notification_differents)
    }
    id_notification_differents <<- NULL

    if (input$data == "selectSats") {
      # Si se selecciona un satélite, se obtiene su TLE
      sat <- test_tles[[strtoi(input$satelite)]]
      sat <- c(sat, initialDateTime = sat$dateTime)
      sats[[1]] <- sat
    } else {
      if (!is.null(input$TLEFile)) {
        # Si se sube un fichero, se obtienen los TLEs
        file <- input$TLEFile
        TLE_sats <- read_file(file)
        if (!is.null(names(TLE_sats))) {
          sats[[1]] <- TLE_sats
        } else {
          sats <- TLE_sats
        }
        number_norad <- sats[[1]]$NORADcatalogNumber
        for (i in seq_along(sats)) {
          sats[[i]] <- c(sats[[i]], initialDateTime = sats[[i]]$dateTime)
          if (number_norad != sats[[i]]$NORADcatalogNumber) {
            differents <- TRUE
          }
        }
      } else {
        # Si no se sube un fichero, se devuelve NULL
        return(orbital_elements)
      }
    }

    if (input$propagationTime == "datetime") {
      shiny::req(input$targetDateSat)
      shiny::req(input$targetTimeSat)

      # Si se selecciona propagar por fecha y hora absolutas,
      # se obtiene la fecha y hora seleccionadas de destino
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

      # Se propagan los satélites
      data <- calculate_data(sats, target_date = target_date)
    } else if (input$propagationTime == "minutes") {
      if (differents) {
        # Si se suben TLEs de satélites diferentes, se muestra un mensaje
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

      shiny::req(input$propagationTimeSat)

      # Si se selecciona propagar por minutos relativos al epoch,
      # se obtienen los minutos seleccionados y se propagan los satélites
      data <- calculate_data(sats, min = input$propagationTimeSat)
      
    }
    orbital_elements <- data[[4]] # Elementos orbitales

    # Se obtienen los nombres de los satélites para la leyenda
    names <- NULL
    for (i in seq_along(sats)) {
      if (input$data == "fileSats") {
        names <- c(names, sats[[i]]$objectName)
      } else {
        names <- c(names, sats[[i]]$NORADcatalogNumber)
      }
    }

    # Se devuelven los elementos orbitales y los nombres de los satélites
    return(list(orbital_elements, names))
  }

  # Genera el gráfico de la excentricidad
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

  # Genera el gráfico de la inclinación
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

  # Genera el gráfico de la anomalía media
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

  # Genera el gráfico del argumento del perigeo
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

  # Genera el gráfico de la ascención recta
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
