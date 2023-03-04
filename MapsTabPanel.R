MapsTabPanel <- tabPanel("Visualiza satélites",
                         sidebarLayout(
                           sidebarPanel(
                             width = 5,
                             fluidRow(
                               column(
                                 6,
                                 h3("Elige un satélite de ejemplo para probar"),
                                 uiOutput("select_satelite"),
                               ),
                               column(
                                 6,
                                 h3("Sube tu propio fichero TLE:"),
                                 fileInput(
                                   "TLEFile",
                                   "Fichero TLE",
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
                             br(),
                             fluidRow(
                               column(12, p("En esta ventana podrás subir tu fichero con tantos satélites como quieras para visualizarlos en el mapa, sin embargo, debes tener en cuenta que:"),
                               tags$ol(tags$li("No se puede asegurar una trayectoria correcta si se propaga más de dos semanas."),
                                       tags$li("Si los satélites son diferentes entre ellos, no se podrá propagar en minutos.")))
                               
                             ),
                             hr(style = "border-top: 1px solid #000000;"),
                             h4("Propagación de la trayectoria"),
                             fluidRow(
                               column(
                                 6,
                                 p("Fecha inicial satélite (más reciente)", style = "font-weight:bold;"),
                                 verbatimTextOutput("initialDateSat")
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
                             ),
                             fluidRow(column(
                               6, timeInput("targetTimeSat", p("Hora destino"), value = "00:00:00")
                             ),
                             ),
                             fluidRow(column(
                               6, h3("o", align = "center")
                             )),
                             fluidRow(column(
                               6, numericInput(
                                 "propagationTimeSat",
                                 p("Tiempo de propagación (minutos)"),
                                 value = 0,
                                 min = 0
                               )
                             ),
                             )
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
                               tabPanel("Primer método", shinycssloaders::withSpinner(leafletOutput("firstMap", height = '80vh'))),
                               tabPanel(
                                 "Segundo método (HPOP)",
                                 shinycssloaders::withSpinner(leafletOutput("hpopOutput"))
                               )
                             ),
                           )
                         ))
