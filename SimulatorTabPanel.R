simulatorTabPanel <- tabPanel(
  "Simulador",
  sidebarLayout(
    sidebarPanel(
      h4("Inserta los datos del satélite aquí:"),
      br(),
      width = 5,
      fluidRow(
        column(6, numericInput("inclination",
          p("Inclinación media (grados)"),
          value = "34.3"
        )),
        column(6, numericInput("ascension",
          p("Ascensión recta media (grados)"),
          value = "349"
        )),
      ),
      fluidRow(
        column(6, numericInput(
          "eccentricity", p("Excentricidad media (0-1)"),
          value =
            "0.186",
          min = 0,
          max = 1,
          step = 0.1
        )),
        column(6, numericInput(
          "perigeeArgument", p("Argumento del perigeo medio (grados)"),
          value =
            "332"
        )),
      ),
      fluidRow(
        column(6, numericInput(
          "meanAnomaly", p("Anomalía media (grados)"),
          value = "19.3"
        )),
        column(6, numericInput(
          "meanMotion", p("Velocidad angular media (grados/día)"),
          value = "10.8"
        )),
      ),
      fluidRow(
        column(6, numericInput("Bstar",
          p("B* (1/rad)"),
          value = "2.81e-05"
        )),
      ),
      hr(style = "border-top: 1px solid #000000;"),
      h4("Propagación de la trayectoria"),
      fluidRow(
        column(12, radioButtons(
          "propagationTimeSimulator", "Tipo de propagación:",
          c(
            "Dada una fecha y hora" = "datetime",
            "Dados unos minutos" = "minutes"
          )
        ))
      ),
      br(),
      p("Las fechas serán consideradas en UTC", style = "color: gray;"),
      fluidRow(
        column(6, dateInput("initialDateSimulator",
          p("Fecha inicial"),
          format = "yyyy-mm-dd"
        )),
        column(6, timeInput("initialTimeSimulator",
          p("Hora inicial"),
          value = "00:00:00"
        )),
      ),
      fluidRow(
        column(6, dateInput("targetDateSimulator",
          p("Fecha destino"),
          format = "yyyy-mm-dd"
        )),
        column(6, timeInput("targetTimeSimulator",
          p("Hora destino"),
          value = "00:00:00"
        )),
      ),
      h3("o", align = "center"),
      fluidRow(
        column(6, numericInput("propagationTimeSatSimulator",
          p("Tiempo de propagación (minutos)"),
          value = 0, min = 0
        )),
      )
    ),
    mainPanel(
      width = 7, h4("Simulación"),
      shinycssloaders::withSpinner(leafletOutput("myMap", height = "80vh"))
    )
  ),
)
