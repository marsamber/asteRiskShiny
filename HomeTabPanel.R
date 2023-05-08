homeTabPanel <- tabPanel(
  "Inicio",
  fluidRow(
    column(12, h4("¿Qué es asteRisk?")),
    column(12, p("asteRisk es una aplicación web que permite la visualización
    de trayectorias de satélites en el espacio. Además, permite la propagación
    de trayectorias de satélites en el tiempo, así como la simulación de
    trayectorias de satélites en el espacio.")),
    column(12, p("Para la propagación de trayectorias de satélites, la 
                 aplicación utiliza diferentes métodos, entre ellos SGP4 
                 (Simplified General Perturbations 4), SDP4 (Simplified 
                 Deep-space Perturbation 4) y HPOP (High Precision Orbit 
                 Propagator), los cuales permiten predecir las órbitas de los 
                 satélites con alta precisión y velocidad de propagación. 
                 Además, la función SGDP4 usada en la aplicación, se encarga de 
                 seleccionar el propagador más óptimo entre SGP4 y SDP4, en 
                 función de la época y las condiciones orbitales del satélite.")),
    column(12, p("La propagación de trayectorias de satélites se puede realizar 
                 tanto por fecha y tiempo absolutos como por minutos relativos 
                 al epoch. Esto proporciona una gran flexibilidad para 
                 seleccionar el momento preciso para la propagación de la 
                 trayectoria de los satélites.")),
    column(12, p("Además de las funcionalidades mencionadas anteriormente, 
                 asteRisk también ofrece una pestaña adicional en la que se 
                 pueden ver gráficas de la evolución de los elementos orbitales 
                 keplerianos de los satélites seleccionados durante la 
                 trayectoria."))
  ),
  fluidRow(
    column(12, h4("¿Cómo funciona?")),
    column(12, p("Para la visualización de trayectorias de satélites en el 
                 espacio, se utiliza la librería leaflet, que permite la 
                 visualización de mapas en el navegador. Para la propagación de 
                 trayectorias de satélites en el tiempo, se utiliza la librería 
                 asteRisk, que permite la propagación de trayectorias de 
                 satélites en el tiempo mediante fecha y hora absolutas, así 
                 como por minutos relativos al epoch. Además, asteRisk permite 
                 obtener información detallada de los satélites en cada punto 
                 de la trayectoria en el mapa, incluyendo su posición, velocidad 
                 y elementos keplerianos orbitales. Para la simulación de 
                 trayectorias de satélites en el espacio, se utiliza también la 
                 librería asteRisk, que permite la simulación de trayectorias de
                 satélites en el espacio.")),
  ),
  fluidRow(
    column(12, h4("¿Cómo se usa?")),
    column(12, p("Para usar la aplicación, debes seleccionar la pestaña de 
                 'Mapas' en la barra de navegación. En esta pestaña, puedes 
                 seleccionar los satélites que quieres visualizar en el mapa. 
                 Además, puedes seleccionar la fecha y hora en la que quieres 
                 propagar la trayectoria de los satélites, o bien propagarlos 
                 por minutos relativos al epoch.")),
  ),
  fluidRow(
    column(12, h4("¿Cómo se actualizan los datos?")),
    column(12, p("Para actualizar los datos, debes pulsar el botón 'Cargar datos
                 más recientes'. Cuando el proceso termine, verá un mensaje de
                 'El proceso ha terminado'")),
    column(12, p("Este proceso es ", strong("importante")," para el correcto 
                 funcionamiento de la aplicación, actualizará la posición de la 
                 Tierra, los datos del tiempo en el espacio, los datos de las 
                 tormentas solares y los datos de las tormentas geomagnéticas.")),
  ),
  fluidRow(
    column(2, actionButton("get_latest_space_data", "Cargar datos más recientes",
    style=" text-align: center;
        width: 100%;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;")),
    style = "display: flex; align-items: center; justify-content: center;"
  ),
  fluidRow(
    column(4, verbatimTextOutput("output"), style = "text-align: center;"),
    style = "display: flex; align-items: center; justify-content: center; 
    margin-top:1vh; text:center"
  ),
  br(),
  fluidRow(
    column(12, img(src = "logo_asterisk.png", style = "display: block; margin: 0
                   auto; width: 30vw;")))
)
