source("homeTabPanel.R")
source("mapsTabPanel.R")
source("simulatorTabPanel.R")

ui <-
  fluidPage(
    tags$head(tags$link(rel = "shortcut icon", href = "logo.png")),
    titlePanel("AsteRisk - Visualiza Satélites"),
    fluidPage(
      tabsetPanel(
        type = "tabs",
        homeTabPanel,
        mapsTabPanel,
        simulatorTabPanel
      )
    )
  )
