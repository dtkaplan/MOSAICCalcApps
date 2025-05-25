library(shiny)
library(mosaicCalc)
library(mosaic)
source("globals.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  source("server-sources/search_1d.R", local = TRUE)

  source("server-sources/search_2d.R", local = TRUE)

  source("server-sources/solar.R", local = TRUE)

  source("server-sources/cows.R", local = TRUE)

  source("server-sources/graph_antiD.R", local = TRUE)

  source("server-sources/euler.R", local = TRUE)

  source("server-sources/airfoil.R", local = TRUE)

  source("server-sources/average_and_marginal.R", local = TRUE)

  source("server-sources/sailing.R", local = TRUE)

  source("server-sources/logistic_balance.R", local = TRUE)

  source("server-sources/DE-euler.R", local = TRUE)

  source("server-sources/eigenflows.R", local = TRUE)

  source("server-sources/cobweb.R", local = TRUE)

  source("server-sources/dchain.R", local = TRUE)

  source("server-sources/interpolation-explorer.R", local = TRUE)

  source("server-sources/eigenvectors.R", local = TRUE)

  source("server-sources/matrix_iteration.R", local = TRUE)

  source("server-sources/linear_flow.R", local = TRUE)

  source("server-sources/flow-1d.R", local = TRUE)

  source("server-sources/sinusoids.R", local = TRUE)

  source("server-sources/latex.R", local = TRUE)

  # # # source("server-sources/ansatze.R", local = TRUE)
}

