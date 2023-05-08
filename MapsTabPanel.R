mapsTabPanel <- tabPanel(
  "Mapas",
  sidebarLayout(
    sidebarPanel(
      width = 5,
      fluidRow(
        column(
          6,
          h4("Elige un satélite de ejemplo para probar"),
          uiOutput("select_satelite"),
        ),
        column(
          6,
          h4("Sube tu propio fichero TLE:"),
          fileInput(
            "TLEFile",
            "Fichero TLE",
            multiple = FALSE,
            accept = "text/plain",
            buttonLabel = "Examinar...",
            placeholder = "Ningún archivo seleccionado",
            capture = NULL
          ),
        )
      ),
      fluidRow(
        column(
          6, radioButtons(
            "data",
            "¿Qué satélites quieres visualizar?",
            c(
              "Predefinidos" = "selectSats",
              "Mi fichero" = "fileSats"
            )
          )
        ),
        column(
          6, radioButtons(
            "colors",
            "Seleccionar esquema de colores:",
            c(
              "Gama de colores" = "palette",
              "Escala verde a rojo (según cercanía al epoch)" = "scale"
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(
          12, p("En esta ventana podrás subir tu fichero
          con tantos satélites como quieras para visualizarlos
          en el mapa, sin embargo, debes tener en cuenta que:"),
          tags$ol(
            tags$li("Cuanto más distante sea el tiempo de propagación del epoch,
            menos confiable es la trayectoria. Como regla aproximativa, a partir
            de dos semanas es prácticamente inútil la predicción."),
            tags$li("Si se propagan varios satélites simultáneamente, los
            tiempos de propagación se tienen que proporcionar como
            fechas-tiempo absolutos.")
          )
        )
      ),
      hr(style = "border-top: 1px solid #000000;"),
      h4("Propagación de la trayectoria"),
      fluidRow(
        column(
          6,
          p("Fecha inicial satélite (más reciente)",
            style = "font-weight:bold;"
          ),
          verbatimTextOutput("initialDateSat")
        ),
        column(
          6,
          p("En caso de error, acude al botón que se encuentra en la página
           principal: 'Cargar datos más recientes'",
            style = "text-align:center; color:red;border: 1px solid black;
           padding: 10px;"
          )
        )
      ),
      br(),
      fluidRow(column(
        12, radioButtons(
          "propagationTime",
          "Modos de entrada de tiempo de propagación:",
          c(
            "Fecha y hora absolutos" = "datetime",
            "Minutos relativos al epoch" = "minutes"
          )
        )
      )),
      p("Las fechas serán consideradas en UTC", style = "color: gray;"),
      fluidRow(
        column(
          6, dateInput("targetDateSat", p("Fecha destino"),
            format = "yyyy-mm-dd",
            value = "0000-00-00"
          )
        ),
        column(
          6, numericInput("propagationTimeSat",
            p("Tiempo de propagación (minutos)"),
            value = 0,
          )
        )
      ),
      fluidRow(
        column(
          6, shinyTime::timeInput("targetTimeSat",
            p("Hora destino"),
            value = "00:00:00"
          )
        ),
        column(
          6, actionButton("generate", "Generar"),
          style = "display: flex;
          justify-content: center;"
        ),
        style = "display: flex; align-items: center;"
      ),
    ),


    # Show a plot of the generated distribution
    mainPanel(
      width = 7,
      tabsetPanel(
        id = "metodos",
        type = "tabs",
        header = tags$head(
          tags$style(
            type = "text/css",
            paste0(
              "
                                             #loadmessage {
                                             position: fixed;
                                             top: 0px;
                                             left: 0px;
                                             width: 100%;
                                             padding: 5px 0px 5px 0px;
                                             text-align: center;
                                             font-weight: bold;
                                             font-size: 100%;
                                             color: '#ffffff';
                                             background-color: '#B2DAE9';
                                             z-index: 105;
                                             }
                                   #message {
                                   color:'#B2DAE9
                                   }
                                   #shiny-notification-panel {
  top: unset;
  bottom: 0;
  left: unset;
  right: 0;
  margin-left: auto;
  margin-right: auto;
  width: 100%;
  max-width: 500px;
  font-size: large;
}
             "
            )
          ),
          tags$script(
            '
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            '
          )
        ),
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          tags$div(
            tags$p("Cargando...",
              id = "message"
            ),
            id =
              "loadmessage"
          )
        ),
        tabPanel(
          "SGDP4",
          br(),
          fluidRow(
            column(
              4,
              radioButtons("method",
                label = "¿Con qué método quieres
                                propagar la órbita?:",
                choices = c(
                  "Selección automática" = "SGDP4",
                  "SGP4" = "SGP4",
                  "SDP4" = "SDP4"
                ),
                inline = TRUE
              )
            ),
            column(6,
              offset = 1,
              p("En el caso de mostrar varios satélites, se generará una
                     pestaña individualizada por cada uno de ellos para una
                     mejor visualización con un máximo de 10 de estas.")
            )
          ),
          tabsetPanel(
            id = "tabsetpanelSGDP4",
            tabPanel("Trayectorias",
              value = "tab0",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("SGDP4Map", height = "80vh")
              )
            ),
            tabPanel("Mapa 1",
              value = "SGDP4Tab1",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("SGDP4Map1", height = "80vh")
              )
            ),
            tabPanel("Mapa 2",
              value = "SGDP4Tab2",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("SGDP4Map2", height = "80vh")
              )
            ),
            tabPanel("Mapa 3",
              value = "SGDP4Tab3",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("SGDP4Map3", height = "80vh")
              )
            ),
            tabPanel("Mapa 4",
              value = "SGDP4Tab4",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("SGDP4Map4", height = "80vh")
              )
            ),
            tabPanel("Mapa 5",
              value = "SGDP4Tab5",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("SGDP4Map5", height = "80vh")
              )
            ),
            tabPanel("Mapa 6",
              value = "SGDP4Tab6",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("SGDP4Map6", height = "80vh")
              )
            ),
            tabPanel("Mapa 7",
              value = "SGDP4Tab7",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("SGDP4Map7", height = "80vh")
              )
            ),
            tabPanel("Mapa 8",
              value = "SGDP4Tab8",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("SGDP4Map8", height = "80vh")
              )
            ),
            tabPanel("Mapa 9",
              value = "SGDP4Tab9",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("SGDP4Map9", height = "80vh")
              )
            ),
            tabPanel("Mapa 10",
              value = "SGDP4Tab10",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("SGDP4Map10", height = "80vh")
              )
            )
          )
        ),
        tabPanel(
          "HPOP",
          br(),
          p("En el caso de mostrar varios satélites, se generará una
                     pestaña individualizada por cada uno de ellos para una
                     mejor visualización con un máximo de 10 de estas."),
          tabsetPanel(
            id = "tabsetpanelHPOP",
            tabPanel("Trayectorias",
              value = "tab0",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("HPOPMap", height = "80vh")
              )
            ),
            tabPanel("Mapa 1",
              value = "HPOPTab1",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("HPOPMap1", height = "80vh")
              )
            ),
            tabPanel("Mapa 2",
              value = "HPOPTab2",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("HPOPMap2", height = "80vh")
              )
            ),
            tabPanel("Mapa 3",
              value = "HPOPTab3",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("HPOPMap3", height = "80vh")
              )
            ),
            tabPanel("Mapa 4",
              value = "HPOPTab4",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("HPOPMap4", height = "80vh")
              )
            ),
            tabPanel("Mapa 5",
              value = "HPOPTab5",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("HPOPMap5", height = "80vh")
              )
            ),
            tabPanel("Mapa 6",
              value = "HPOPTab6",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("HPOPMap6", height = "80vh")
              )
            ),
            tabPanel("Mapa 7",
              value = "HPOPTab7",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("HPOPMap7", height = "80vh")
              )
            ),
            tabPanel("Mapa 8",
              value = "HPOPTab8",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("HPOPMap8", height = "80vh")
              )
            ),
            tabPanel("Mapa 9",
              value = "HPOPTab9",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("HPOPMap9", height = "80vh")
              )
            ),
            tabPanel("Mapa 10",
              value = "HPOPTab10",
              shinycssloaders::withSpinner(
                leaflet::leafletOutput("HPOPMap10", height = "80vh")
              )
            )
          )
        ),
        tabPanel(
          "Elementos orbitales keplerianos",
          br(),
          fluidRow(
            column(width = 4, plotly::plotlyOutput("plot1")),
            column(width = 4, plotly::plotlyOutput("plot2")),
            column(width = 4, plotly::plotlyOutput("plot3")),
          ),
          br(),
          fluidRow(
            column(width = 6, plotly::plotlyOutput("plot4")),
            column(width = 6, plotly::plotlyOutput("plot5"))
          )
        )
      ),
    )
  ),
)
