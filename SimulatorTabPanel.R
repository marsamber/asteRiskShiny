SimulatorTabPanel <- tabPanel("Simulador",
                              sidebarLayout(
                                sidebarPanel(
                                  h4("Inserta los datos del satélite aquí:"),
                                  width = 5,
                                  fluidRow(
                                    column(4, dateInput("date", p("Fecha"), value = "2000-06-27")),
                                    column(4, timeInput("time", p("Hora"), value = "18:50:19.7335679990829")),
                                    column(4, numericInput("inclination", p("Inclinación"), value = "34.3")),
                                  ),
                                  fluidRow(
                                    column(4, numericInput("ascension", p("Ascenso"), value = "349")),
                                    column(4, numericInput(
                                      "eccentricity", p("Excentricidad"),
                                      value =
                                        "0.186"
                                    )),
                                    column(4, numericInput(
                                      "perigeeArgument", p("Argumento del perigeo"),
                                      value =
                                        "332"
                                    )),
                                  ),
                                  fluidRow(
                                    column(4, numericInput(
                                      "meanAnomaly", p("Anomalía media"), value = "19.3"
                                    )),
                                    column(4, numericInput(
                                      "meanMotion", p("Movimiento medio"), value = "10.8"
                                    )),
                                    column(4, numericInput("Bstar", p("B Star"), value = "2.81e-05")),
                                  ),
                                ),
                                mainPanel(width = 7, h4("Simulación"), leafletOutput("myMap"))
                              ),)