MapsTabPanel <- tabPanel("Visualiza satélites",
                         sidebarLayout(
                           sidebarPanel(
                             width = 5,
                             fluidRow(
                               column(
                                 6,
                                 h3("Elige dos satélites para mostrar:"),
                                 uiOutput("select_satelite"),
                                 uiOutput("select_satelite2"),
                               ),
                               column(
                                 6,
                                 h3("Sube tu propio fichero TLE:"),
                                 fileInput(
                                   "TLEFile",
                                   "FicheroTLE",
                                   multiple = FALSE,
                                   accept = "text/plain",
                                   buttonLabel = "Examinar...",
                                   placeholder = "Ningún archivo seleccionado",
                                   capture = NULL
                                 ),
                                 fluidRow(
                                   column(4, offset = 4,
                                          actionButton('reset', 'Resetear fichero')
                                   ))
                               )
                             ),
                             hr(style = "border-top: 1px solid #000000;"),
                             h4("Propagación de la trayectoria"),
                             fluidRow(
                               column(
                                 6,
                                 p("Fecha inicial primer satélite", style = "font-weight:bold;"),
                                 verbatimTextOutput("initialDateSat")
                               ),
                               column(
                                 6,
                                 p("Fecha inicial segundo satélite", style = "font-weight:bold;"),
                                 verbatimTextOutput("initialDateSat2")
                               ),
                             ),
                             br(),
                             fluidRow(column(
                               12, radioButtons(
                                 "propagationTime",
                                 "Tipo de propagación:",
                                 c(
                                   "Dada una fecha y hora" = "datetime",
                                   "Dados unos minutos" = "minutes"
                                 )
                               )
                             )),
                             br(),
                             p("Las fechas serán consideradas en UTC", style = "color: gray;"),
                             fluidRow(column(
                               6, dateInput("targetDateSat", p("Fecha destino"), format = "yyyy-mm-dd")
                             ),
                             column(
                               6, dateInput("targetDateSat2", p("Fecha destino"), format = "yyyy-mm-dd")
                             ), ),
                             fluidRow(column(
                               6, timeInput("targetTimeSat", p("Hora destino"), value = "00:00:00")
                             ),
                             column(
                               6, timeInput("targetTimeSat2", p("Hora destino"), value = "00:00:00")
                             ), ),
                             h3("o", align = "center"),
                             fluidRow(column(
                               6, numericInput(
                                 "propagationTimeSat",
                                 p("Tiempo de propagación (minutos)"),
                                 value = 0,
                                 min = 0
                               )
                             ),
                             column(
                               6, numericInput(
                                 "propagationTimeSat2",
                                 p("Tiempo de propagación (minutos)"),
                                 value = 0,
                                 min = 0
                               )
                             ), )
                           ),
                           
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             width = 7,
                             tabsetPanel(
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
                                   }"
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
                               conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                                tags$div(
                                                  tags$p("Estamos cargando tu mapa, ¡no nos olvidamos de ti! :)", id = "message"),
                                                  id =
                                                    "loadmessage"
                                                )),
                               tabPanel("Primer método", leafletOutput("firstMap", height = '80vh')),
                               tabPanel(
                                 "Segundo método (HPOP)",
                                 shinycssloaders::withSpinner(leafletOutput("hpopOutput"))
                               )
                             ),
                           )
                         ))
