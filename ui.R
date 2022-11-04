source('HomeTabPanel.R')
source('MapsTabPanel.R')
source('SimulatorTabPanel.R')

ui <-
  fluidPage(titlePanel("AsteRisk - Visualiza Satélites"),
            fluidPage(
              tabsetPanel(type = "tabs",
                          HomeTabPanel,
                          MapsTabPanel,
                          SimulatorTabPanel)
            ))