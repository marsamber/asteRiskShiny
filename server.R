source("auxiliarFunctions.R", local = TRUE)

server <- function(input, output, session) {
  test_TLEs <-
    readTLE(paste0(path.package("asteRisk"), "/testTLE.txt"))

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
      if (is.null(input$TLEFile)) {
        initial_datetime_sat <- test_TLEs[[strtoi(input$satelite)]]$dateTime
        initial_date_sat <- substr(initial_datetime_sat, 1, 10)

        updateDateInput(
          session,
          "targetDateSat",
          value = as.Date(initial_date_sat) + 1,
          min = initial_date_sat
        )
        updateTimeInput(session, "targetTimeSat", value = Sys.time())
      } else {
        initial_datetime_sat <- get_more_recent_date()
        initial_date_sat <- substr(initial_datetime_sat, 1, 10)
        updateDateInput(
          session,
          "targetDateSat",
          value = as.Date(initial_date_sat) + 1,
          min = initial_date_sat
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
    selectInput("satelite", p("Satélite"), choices = get_satelites())
  })

  output$initialDateSat <-
    renderText({
      req(input$satelite)
      if (!is.null(input$TLEFile)) {
        get_more_recent_date()
      } else {
        test_TLEs[[strtoi(input$satelite)]]$dateTime
      }
    })

  output$firstMap <- renderLeaflet({
    req(input$satelite)
    req(input$propagationTime)
    req(input$dimension)

    sats <- list()

    if (!is.null(input$TLEFile)) {
      file <- input$TLEFile
      TLE_sats <- readTLE(filename = file$datapath)
      if (!is.null(names(TLE_sats))) {
        sats[[1]] <- TLE_sats
      } else {
        sats <- TLE_sats
      }
      for (i in seq_along(sats)) {
        sats[[i]] <- c(sats[[i]], initialDateTime = sats[[i]]$dateTime)
      }
    } else {
      sat <- test_TLEs[[strtoi(input$satelite)]]
      sat <- c(sat, initialDateTime = sat$dateTime)
      sats[[1]] <- sat
    }

    if (input$propagationTime == "datetime") {
      req(input$targetDateSat)
      req(input$targetTimeSat)

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
    geo_polylines <- calculate_geo_polylines(geo_markers)


    names <- NULL
    for (i in seq_along(sats)) {
      names <- c(names, sats[[i]]$NORADcatalogNumber)
    }

    render_map_satellites(geo_markers, geo_polylines, input$dimension, names, positions_two_weeks)
  })


  output$hpopOutput <- renderLeaflet({
    req(input$satelite)
    req(input$satelite2)
    req(input$dimension)

    sat <- test_TLEs[[strtoi(input$satelite)]]
    sat2 <- test_TLEs[[strtoi(input$satelite2)]]

    geodetics_matrix_hpop <- get_geodetics_matrix_hpop(sat, sat2)
    geo_markers <- calculate_geo_markers(geodetics_matrix_hpop)
    geo_polylines <- calculate_geo_polylines(geo_markers)

    render_map_satellites(
      geo_markers, geo_polylines, input$dimension,
      list(sat$NORADcatalogNumber, sat2$NORADcatalogNumber)
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

      geodetics_matrix <-
        calculate_geodetic_matrix_and_position_two_weeks(
          sats,
          target_date = paste(input$targetDateSimulator,
            target_time_sat,
            sep = " "
          )
        )
    } else if (input$propagationTimeSimulator == "minutes") {
      req(input$propagationTimeSatSimulator)
      geodetics_matrix <-
        calculate_geodetic_matrix_and_position_two_weeks(
          sats,
          min = input$propagationTimeSatSimulator
        )
    }

    geo_markers <- calculate_geo_markers(geodetics_matrix)
    geo_polylines <- calculate_geo_polylines(geo_markers)

    render_map_satellites(geo_markers, geo_polylines, input$dimension)
  })
}
