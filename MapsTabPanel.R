MapsTabPanel <- tabPanel("Visualiza satélites",
                         sidebarLayout(
                           sidebarPanel(
                             h3("Elige dos satélites para mostrar:"),
                             uiOutput("select_satelite"),
                             uiOutput("select_satelite2"),
                           ),
                           
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             tabsetPanel(
                               type = "tabs",
                               header = tags$head(tags$style(
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
                               )),
                               conditionalPanel(
                                 condition = "$('html').hasClass('shiny-busy')",
                                 tags$div(tags$p("Estamos cargando tu mapa, ¡no nos olvidamos de ti! :)", id="message"),
                                          id =
                                            "loadmessage")
                               ),
                               tabPanel("Primer método", leafletOutput("firstMap")),
                               tabPanel(
                                 "Segundo método (HPOP)",
                                 shinycssloaders::withSpinner(leafletOutput("hpopOutput"))
                               )
                             ),
                           )
                         ))
