SimulatorTabPanel <- tabPanel("Simulador",
                              sidebarLayout(
                                sidebarPanel(
                                  width = 6,
                                  h4("Inserta los datos del satélite aquí:"),
                                  fluidRow(
                                    column(3, textInput(
                                      "NORADCatalogNumber", p("Número de catálogo NORAD"),
                                      value =
                                        "00005"
                                    )),
                                    column(3,
                                           textInput(
                                             "classificationLevel", p("Nivel de clasificación"),
                                             value =
                                               "unclassified"
                                           )),
                                    column(
                                      3,
                                      textInput(
                                        "internationalDesignator",
                                        p("Designación internacional"),
                                        value = "2058-002B"
                                      )
                                    ),
                                    column(3, numericInput("launchYear", p("Año de lanzamiento"), value = "2000"))
                                  ),
                                  fluidRow(
                                    column(3, textInput(
                                      "launchNumber", p("Número de lanzamiento"),
                                      value = "002"
                                    )),
                                    column(3, textInput("launchPiece", p("Pieza de lanzamiento"), value = "B")),
                                    column(3, dateInput("date", p("Fecha"), value = "2000-06-27")),
                                    column(3, timeInput("time", p("Hora"), value = "18:50:19.7335679990829")),
                                  ),
                                  fluidRow(
                                    column(3, numericInput(
                                      "elementNumber", p("Número de elemento"),
                                      value =
                                        "475"
                                    )),
                                    column(3, numericInput("inclination", p("Inclinación"), value = "34.3")),
                                    column(3, numericInput("ascension", p("Ascenso"), value = "349")),
                                    column(3, numericInput(
                                      "eccentricity", p("Excentricidad"),
                                      value =
                                        "0.186"
                                    )),
                                  ),
                                  fluidRow(
                                    column(3, numericInput(
                                      "perigeeArgument", p("Argumento del perigeo"),
                                      value =
                                        "332"
                                    )),
                                    column(3, numericInput("meanAnomaly", p("Anomalía media"), value = "19.3")),
                                    column(3, numericInput("meanMotion", p("Movimiento medio"), value = "10.8")),
                                    column(3, numericInput(
                                      "meanMotionDerivative", p("Derivada del movimiento medio"),
                                      value =
                                        "4.6e-07"
                                    )),
                                  ),
                                  fluidRow(
                                    column(3, numericInput(
                                      "meanMotionSecondDerivative",
                                      p("Segunda derivada del movimiento medio"),
                                      value = "0"
                                    )),
                                    column(3, numericInput("Bstar", p("B Star"), value = "2.81e-05")),
                                    column(
                                      3,
                                      textInput("ephemerisType", p("Tipo de efemérides"), value = "Distributed data (SGP4/SDP4)")
                                    ),
                                    column(3, numericInput(
                                      "epochRevolutionNumber", p("Número de época de revolución"),
                                      value =
                                        "41366"
                                    ))
                                  ),
                                  textInput("objectName", p("Nombre del objeto"), value = "TEME example"),
                                ),
                                mainPanel(width = 6, h4("Simulación"), leafletOutput("myMap"))
                              ),)