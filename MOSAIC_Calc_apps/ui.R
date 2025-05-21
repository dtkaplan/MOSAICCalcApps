library(shiny)
library(bslib)


source("globals.R")

source("ui-definitions.R")
source("ui-sources/DE-Euler-ui.R")
source("ui-sources/eigenflows-ui.R")
source("ui-sources/graph_antiD_ui.R")
source("ui-sources/search_1d_ui.R")
source("ui-sources/search_2d_ui.R")
source("ui-sources/interpolation-explorer-ui.R")
source("ui-sources/cobweb_ui.R")
source("ui-sources/dchain_ui.R")
source("ui-sources/eigenvectors_ui.R")
source("ui-sources/matrix_iteration_ui.R")
source("ui-sources/linear_flow_ui.R")



navbarPage("MOSAIC Calc Apps",
  navbarMenu("About",
    do.call(nav_panel, c(list(title = "Welcome"), about_ui()))
    ),
  navbarMenu("D and antiD",
    do.call(nav_panel, c(list(title = "f() and F()"), graph_antiD_ui())),
    do.call(nav_panel, c(list(title = "Average & marginal"), am_ui())),
    do.call(nav_panel, c(list("Chain of D()"), dchain_ui())),
  ),
  navbarMenu("Vectors & Matrices",
    do.call(nav_panel, c(list("Eigenvectors"), eigenvectors_ui())),
    ),
  navbarMenu("Accumulation",
    do.call(nav_panel, c(list(title = "Solar Panels"), solar_ui())),
    do.call(nav_panel, c(list(title = "Euler integration"), euler_ui())),
    do.call(nav_panel, c(list("Sailing along"), sailing_ui())),
    do.call(nav_panel, c(list("Euler accumulation"), DE_euler_ui()))
  ),
  navbarMenu("Dynamics",
    do.call(nav_panel, c(list(title = "Grazing Cows"), cows_ui())),
    do.call(nav_panel, c(list("Balance in logistic growth"), logistic_balance_ui())),
    do.call(nav_panel, c(list("Eigenflows"), eigenflows_ui())),
    do.call(nav_panel, c(list("Linear flowfield"), linear_flow_ui())),
    do.call(nav_panel, c(list("Cobweb"), cobweb_ui())),
    do.call(nav_panel, c(list("Matrix iteration", matrix_iteration_ui())))
    # Not yet working
    # do.call(nav_panel, c(list(title = "Ansatze"), ansatze_ui())),
  ),
  navbarMenu("Misc.",
    do.call(nav_panel, c(list(title = "NACA Airfoil"), airfoil_ui())),
  ),

)


