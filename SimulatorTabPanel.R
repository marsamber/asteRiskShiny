SimulatorTabPanel <- tabPanel("Simulador",
                              sidebarLayout(
                                sidebarPanel(
                                  h3("Inserta los datos del satélite aquí:"),
                                  br(),
                                  width = 5,
                                  fluidRow(
                                    column(6, numericInput("inclination", p("Inclinación media"), value = "34.3")),
                                    column(6, numericInput("ascension", p("Ascensión recta media"), value = "349")),
                                  ),
                                  fluidRow(
                                    column(6, numericInput(
                                      "eccentricity", p("Excentricidad media"),
                                      value =
                                        "0.186",
                                      min = 0,
                                      max = 1,
                                      step = 0.1
                                    )),
                                    column(6, numericInput(
                                      "perigeeArgument", p("Argumento del perigeo medio"),
                                      value =
                                        "332"
                                    )),
                                  ),
                                  fluidRow(
                                    column(6, numericInput(
                                      "meanAnomaly", p("Anomalía media"), value = "19.3"
                                    )),
                                    column(6, numericInput(
                                      "meanMotion", p("Movimiento medio"), value = "10.8"
                                    )),
                                  ),
                                  fluidRow(
                                    column(6, numericInput("Bstar", 
                                                           p("B*"), value = "2.81e-05")),
                                  ),
                                  hr(style = "border-top: 1px solid #000000;"),
                                  h4("Propagación de la trayectoria"),
                                  fluidRow(
                                    column(12, radioButtons("propagationTimeSimulator", "Tipo de propagación:",
                                                           c("Dada una fecha y hora" = "datetime",
                                                             "Dados unos minutos" = "minutes")))
                                  ),
                                  br(),
                                  p("Las fechas serán consideradas en UTC", style = "color: gray;"),
                                  fluidRow(
                                    column(6, dateInput("initialDateSimulator", p("Fecha inicial"), value = "2000-06-27")),
                                    column(6, timeInput("initialTimeSimulator", p("Hora inicial"), value = "18:50:19.7335679990829")),
                                  ),
                                  fluidRow(
                                     column(6, dateInput("targetDateSimulator", p("Fecha destino"), value = "2000-06-27")),
                                     column(6, timeInput("targetTimeSimulator", p("Hora destino"), value = "18:50:19.7335679990829")),
                                  ),
                                  h3("o", align = "center"),
                                  fluidRow(
                                    column(6, numericInput("propagationTimeSimulator", p("Tiempo de propagación (minutos)"), value = 0)),
                                  )
                                ),
                                mainPanel(width = 7, h4("Simulación"), leafletOutput("myMap", height = '80vh'))
                              ),)