source('HomeTabPanel.R')
source('MapsTabPanel.R')
source('SimulatorTabPanel.R')

ui <-
  fluidPage(tags$head(tags$link(rel="shortcut icon", href="logo.png")),
            titlePanel("AsteRisk - Visualiza Satélites"),
            fluidPage(
              tabsetPanel(type = "tabs",
                          HomeTabPanel,
                          MapsTabPanel,
                          SimulatorTabPanel)
            ))