source("homeTabPanel.R")
source("mapsTabPanel.R")
source("simulatorTabPanel.R")

ui <-
  fluidPage(
    useShinyjs(),
    tags$head(tags$link(rel = "shortcut icon", href = "logo_asterisk.png")),
    titlePanel("asteRisk - Interfaz gráfica para astrodinámica"),
    fluidPage(
      tabsetPanel(
        type = "tabs",
        homeTabPanel,
        mapsTabPanel,
        simulatorTabPanel
      )
    )
  )
