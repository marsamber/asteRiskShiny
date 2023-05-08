source("homeTabPanel.R")
source("mapsTabPanel.R")
source("simulatorTabPanel.R")

ui <-
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(tags$link(rel = "shortcut icon", href = "logo_asterisk.png")),
    titlePanel("asteRisk - Interfaz gráfica para astrodinámica"),
    fluidPage(
      tabsetPanel(
        id = "tabs",
        type = "tabs",
        homeTabPanel,
        mapsTabPanel,
        simulatorTabPanel
      )
    )
  )
