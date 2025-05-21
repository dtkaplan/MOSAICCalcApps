graph_antiD_ui <- function() {
  list(
    p("The top graph shows a function f(t). The shape of that function is set by the number in this box:"),
    numericInput("gad_f_seed", "Shape of function:", value=101, step=1, min=1, max=1000),
    p("You can make measurements by *click/dragging* the mouse on the bottom graph."),
    plotOutput("gad_littlef",
               brush=brushOpts("gad_Fbrush", direction="x", fill=NA, stroke="tomato", opacity = 1),
               height="250px"),
    h5("Stats on F() for red lines"),
    htmlOutput("gad_boxstats")
  )
}
